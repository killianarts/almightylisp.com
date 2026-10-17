# shiso: HTTP error handling — implementation plan (revised after review)

Repo: `~/common-lisp/almighty/shiso` (system `shiso`, tests `shiso/tests`, lisp-unit2).
Scope: condition hierarchy + `abort-with`, one error middleware, `define-error-page`
registry, JSON/HTMX negotiation, real 405 + `Allow`, debug page, crash-safe fallback,
removal of hard-coded responses. `:error-pages` on `define-application` is phase 2.

## 1. Goals
1. Controllers can write `(s:abort-with 404)` or `(error 's:not-found)` anywhere in the
   request extent (incl. inside almighty-html rendering, Mito calls, guards).
2. Apps customise pages with one form per status:
   `(s:define-error-page 404 (c) (hypermedia:page "Not found" (ah:</> ...)))`.
3. Lookup: exact code → `:4xx`/`:5xx` → `t` → built-in default page.
4. JSON for `Accept: application/json` clients, bare fragment for HTMX (`HX-Request`).
5. Wrong method on an existing path → 405 with `Allow`, not 404.
6. Unhandled Lisp errors → logged with backtrace, then 500 page (prod) or debug page (dev).
7. A crashing error page never produces a loop or an empty connection: plain-text fallback.
8. Behaviour of existing apps unchanged except the bodies of 404/400/500 responses.

## 2. Files

| File | Change |
|---|---|
| `src/errors.lisp` (new, package `shiso/errors`) | conditions, `abort-with`, registry, negotiation, renderers, `wrap-http-errors` |
| `shiso.asd` | add `(:file "errors")` right after `"modules"` (before `"routing"`, whose macro expansion references `shiso/errors` symbols at read time) |
| `src/routing.lisp` | `define-application` final fallback → 404/405 via `shiso/errors` |
| `src/requests.lisp` | 400 on bad request → `bad-request`; on mapper miss record allowed methods in env |
| `src/server.lisp` | replace `wrap-error-logging` with `shiso/errors:wrap-http-errors` (logging moves inside it); set `*debug*` from `debugp` |
| `src/package.lisp` | import + re-export public names |
| `src/auth/guards.lisp` | (optional, step 8) `forbidden-response` renders via registry |
| `src/i18n/controllers.lisp` | `set-language`: hand-made 405 → `(errors:abort-with 405 :allowed '(:POST))` |
| `t/errors.lisp` (new) + `shiso.asd` `shiso/tests` | tests; add `run-tests :package :shiso/t/errors` to `:perform` |
| `skeleton/app/src/…`, `examples/mysite` | example `define-error-page` forms; docs |

## 3. Public exports (from `shiso/errors`, re-exported by `shiso`)
`http-error http-error-status http-error-message http-error-headers
 http-client-error http-server-error
 bad-request unauthorized forbidden not-found method-not-allowed allowed-methods
 internal-server-error
 abort-with define-error-page remove-error-page *error-pages* find-error-page
 *debug* *allowed-methods* *render-around* *fragment-p* wrap-http-errors render-error-response default-error-page`

## 4. Code

### 4.1 Conditions + abort-with (`src/errors.lisp`)
```lisp
(defpackage #:shiso/errors
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html))
  (:export ...))           ; list in §3
(in-package #:shiso/errors)

(defvar *debug* nil
  "Global (NOT let-bound): Woo serves requests on worker threads, which do not
inherit dynamic bindings made in the thread that called clackup.")

(define-condition http-error (error)
  ((status  :initarg :status  :reader http-error-status)
   (message :initarg :message :initform nil :reader http-error-message)
   (headers :initarg :headers :initform nil :reader http-error-headers))
  (:report (lambda (c s) (format s "HTTP ~D~@[: ~A~]"
                                 (http-error-status c) (http-error-message c)))))
(define-condition http-client-error (http-error) ())
(define-condition http-server-error (http-error) ())
(define-condition bad-request  (http-client-error) () (:default-initargs :status 400))
(define-condition unauthorized (http-client-error) () (:default-initargs :status 401))
(define-condition forbidden    (http-client-error) () (:default-initargs :status 403))
(define-condition not-found    (http-client-error) () (:default-initargs :status 404))
(define-condition method-not-allowed (http-client-error)
  ((allowed :initarg :allowed :initform nil :reader allowed-methods))
  (:default-initargs :status 405))
(define-condition internal-server-error (http-server-error) ()
  (:default-initargs :status 500))

(defun status-condition-class (status)
  (case status
    (400 'bad-request) (401 'unauthorized) (403 'forbidden)
    (404 'not-found) (405 'method-not-allowed) (500 'internal-server-error)
    (t (if (< status 500) 'http-client-error 'http-server-error))))

(defun abort-with (status &rest initargs &key message headers allowed)
  (declare (ignore message headers allowed))
  (check-type status (integer 400 599))
  (apply #'error (status-condition-class status) :status status initargs))
```
`allowed` is only a valid initarg for `method-not-allowed`; `abort-with` drops it
(`(remf initargs :allowed)`) for other classes to avoid an invalid-initarg error.

### 4.2 Registry
```lisp
(defvar *error-pages* (make-hash-table :test 'eql)
  "Keys: integer status, :4xx, :5xx, or T. Values: (lambda (condition) ...)
returning an HTML string, an almighty-html node, or a full Clack list.")

(defmacro define-error-page (key (condition-var) &body body)
  (check-type key (or (integer 400 599) (member :4xx :5xx t)))
  `(progn (setf (gethash ',key *error-pages*)
                (lambda (,condition-var) (declare (ignorable ,condition-var)) ,@body))
          ',key))

(defun remove-error-page (key) (remhash key *error-pages*))

(defun find-error-page (status)
  (or (gethash status *error-pages*)
      (gethash (if (< status 500) :4xx :5xx) *error-pages*)
      (gethash t *error-pages*)
      #'default-error-page))
```
Late-bound (looked up per request), so re-evaluating a form in the REPL takes effect
immediately — matches how `define-module` is late-bound.

### 4.3 Negotiation (Lack env: `:headers` is a hash table with lowercase keys)
```lisp
(defun request-header (env name)
  (let ((h (getf env :headers))) (and h (gethash name h))))

(defun response-kind (env)
  (let ((accept (or (request-header env "accept") "")))
    (cond ((request-header env "hx-request") :fragment)
          ((and (search "application/json" accept)
                (not (search "text/html" accept))) :json)
          (t :html))))
```
Simple substring check, deliberately not full q-value parsing (open question §8).

### 4.4 Rendering
```lisp
(defparameter +reason-phrases+
  '((400 . "Bad Request") (401 . "Unauthorized") (403 . "Forbidden")
    (404 . "Not Found") (405 . "Method Not Allowed") (500 . "Internal Server Error")))
(defun reason-phrase (status)
  (or (cdr (assoc status +reason-phrases+)) (if (< status 500) "Client Error" "Server Error")))

(defun default-error-page (c)
  (let ((status (http-error-status c)))
    (ah:</> (html (head (meta :charset "utf-8")
                        (title (format nil "~D ~A" status (reason-phrase status))))
                  (body (h1 (format nil "~D ~A" status (reason-phrase status)))
                        (p (or (http-error-message c) "")))))))

(defun json-escape (s) ...)   ; shiso has no JSON dependency; escape \" \\ and control chars

(defun extra-headers (c)
  (append (http-error-headers c)
          (when (typep c 'method-not-allowed)
            (list :allow (format nil "~{~A~^, ~}" (allowed-methods c))))))

(defun coerce-page-result (status result c)
  "Page functions may return a Clack list, a string, or an almighty-html node."
  (cond ((and (consp result) (integerp (first result))) result)
        (t (list status
                 (append (list :content-type "text/html; charset=utf-8") (extra-headers c))
                 (list (if (stringp result) result (ah:render-to-string result)))))))

(defun plain-fallback (status)
  (list status '(:content-type "text/plain; charset=utf-8")
        (list (format nil "~D ~A" status (reason-phrase status)))))

(defun render-error-response (c env)
  "Never signals. Always returns a Clack (status headers body) list."
  (let ((status (http-error-status c)))
    (handler-case
        (ecase (response-kind env)
          (:json
           (list status
                 (append '(:content-type "application/json; charset=utf-8") (extra-headers c))
                 (list (format nil "{\"status\":~D,\"error\":\"~A\"}"
                               status (json-escape (or (http-error-message c)
                                                       (reason-phrase status)))))))
          ((:html :fragment)
           ;; :fragment — HTMX swaps only the body; page fns may check *fragment-p*.
           (let ((*fragment-p* (eq (response-kind env) :fragment)))
             (coerce-page-result status (funcall (find-error-page status) c) c))))
      (error (e)
        (log-error e env "error page itself failed")
        (plain-fallback status)))))
```
`*fragment-p*` (exported) lets an app page return just the inner content for HTMX.
For `HEAD` requests the body is replaced by `'()` (headers still sent).

### 4.5 Debug page
```lisp
(defun debug-page-response (c backtrace env)
  (list 500 '(:content-type "text/html; charset=utf-8")
        (list (ah:render-to-string
               (ah:</> (html (body (h1 (format nil "500: ~A" (type-of c)))
                                   (pre (princ-to-string c))
                                   (h2 "Backtrace") (pre backtrace)
                                   (h2 "Request")
                                   (pre (format nil "~S ~S" (getf env :request-method)
                                                (getf env :request-uri))))))))))
```
Never dump the full env (session cookie, CSRF token). Text goes through almighty-html
so it's escaped (check `ah` escapes string children — see §8).

### 4.6 The middleware (replaces `wrap-error-logging`)
```lisp
(defun log-error (c env &optional backtrace note) ...) ; same format as current
                                                         ; shiso/server::wrap-error-logging

(defun wrap-http-errors (app)
  (lambda (env)
    (let ((*allowed-methods* nil)) ; per request, thread-local under Woo
    (block request
      (handler-bind
          ((http-error
             (lambda (c)
               (return-from request (render-error-response c env))))
           (error
             (lambda (c)
               ;; Runs BEFORE unwinding, so the backtrace is still on the stack.
               (let ((bt (ignore-errors
                          (with-output-to-string (s)
                            (uiop:print-condition-backtrace c :stream s)))))
                 (log-error c env bt)
                 (return-from request
                   (if *debug*
                       (handler-case (debug-page-response c bt env)
                         (error () (plain-fallback 500)))
                       (render-error-response
                        (make-condition 'internal-server-error) env)))))))
        (funcall app env))))))
```
Inner `handler-case`s in user code still win (handlers are searched innermost first).

### 4.7 Middleware order (`src/server.lisp`)
```lisp
(defun wrap-app (inner-app)
  (errors:wrap-http-errors                         ; outermost: catches session/CSRF/i18n errors too
   (funcall lack/middleware/session:*lack-middleware-session*
            (i18n:wrap-locale
             (funcall lack/middleware/csrf:*lack-middleware-csrf*
                      inner-app))
            :store (session:make-store))))
```
Resulting order, outer→inner: http-errors → session → locale → CSRF → app
(static → strip-trailing-slash → module mounts → 404/405 fallback).
Because the error layer is outside `wrap-locale`, `i18n:*locale*` is NOT bound while an
error page renders. `render-error-response` therefore rebinds it:
via the `errors:*render-around*` hook (R3), which `server.lisp` sets to
`(lambda (env thunk) (let ((i18n:*locale* (or (ignore-errors (i18n:best-locale env)) i18n:*default-locale*))) (funcall thunk)))`.

`%start`: `(setf errors:*debug* debugp)` before `clackup`. Keep passing `:debug debugp`
to clack; it no longer matters for app errors because we always return a response.

### 4.8 404/405 at the end of the mount chain
Modules return `(values 404-response nil)` on a miss so the next mount can try; that must
stay (signalling in the module would break fall-through). So:

`src/requests.lisp`, miss branch:
```lisp
(let ((allowed (allowed-methods-for (routing:routes-mapper (modules:module-routes this))
                                    (req:request-path-info *request*))))
  (when allowed
    (setf errors:*allowed-methods* (union allowed errors:*allowed-methods*))))
;; errors:*allowed-methods* is let-bound to NIL per request inside wrap-http-errors (R5).
```
```lisp
(defparameter +probe-methods+ '(:GET :POST :PUT :PATCH :DELETE :OPTIONS))
(defun allowed-methods-for (mapper path)
  "Methods for which PATH matches some route. Uses myway.rule::match-rule directly
(not myway:dispatch, which would CALL the controller)."
  (let ((ms (loop for m in +probe-methods+
                  when (some (lambda (route)
                               (myway.rule::match-rule (myway.route:route-rule route) m path
                                                       :allow-head nil))
                             (myway.mapper:mapper-routes mapper))
                    collect m)))
    (if (member :GET ms) (append ms '(:HEAD)) ms)))
```
Allowed methods are collected in a per-request special, not in env (R5).

`define-application` fallback:
```lisp
(lambda (env)
  (declare (ignore env))
  (let ((allowed shiso/errors:*allowed-methods*))
    (if allowed
        (shiso/errors:abort-with 405 :allowed allowed)
        (error 'shiso/errors:not-found))))
```
`requests.lisp` bad request: `(error 'shiso/errors:bad-request)` instead of the
literal `'(400 () ("Bad Request"))`.

OPTIONS: if the path exists but no route accepts OPTIONS, the fallback returns
`204` with `Allow` instead of 405 (phase 1.5; behind `*auto-options*`, default T).

### 4.9 Guards / CSRF
- `forbidden-response` (`src/auth/guards.lisp`) → render through
  `errors:render-error-response (make-condition 'errors:forbidden) env`, keeping its
  `(values result t)` contract in the `:around` method (no signalling there, so the
  mount wrapper still sees "handled").
- CSRF failure is `lack/middleware/csrf::return-400`, a plain response. Pass
  `:block-app (lambda (env) (declare (ignore env)) (error 'errors:bad-request :message "CSRF token invalid"))`
  to the CSRF middleware so it uses the registry too.

## 5. Implementation order
1. `errors.lisp` conditions/registry/renderers + asd entry; unit tests for lookup/render.
2. `wrap-http-errors` swapped into `wrap-app`; delete `wrap-error-logging` (logging inside).
3. `*debug*` from `%start`.
4. Replace hard-coded 404 (routing fallback) and 400 (requests).
5. 405 probing + Allow + HEAD; OPTIONS.
6. Exports in `src/package.lisp`.
7. Negotiation (JSON/HTMX) + locale rebinding.
8. Guards forbidden + CSRF block-app + i18n `set-language` 405.
9. Skeleton/example `define-error-page` forms, README/CHANGELOG.
10. Phase 2: `(:error-pages (404 'views:not-found) ...)` on `define-application`,
    expanding to `define-error-page` forms (global; per-app scoping is an open question).

## 6. Test plan (`t/errors.lisp`, package `shiso/t/errors`, lisp-unit2)
Call the Lack app directly with a fake env — no server needed:
```lisp
(defun env (method path &key (accept "text/html") hx)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "accept" h) accept)
    (when hx (setf (gethash "hx-request" h) "true"))
    (list :request-method method :path-info path :request-uri path :script-name ""
          :headers h :server-name "localhost" :server-port 80 :url-scheme "http"
          :query-string nil :raw-body (flex:make-in-memory-input-stream #()))))
```
Use a test app built with `define-module` + `define-application` wrapped by
`errors:wrap-http-errors` only (skip session/CSRF/DB). Restore `*error-pages*` via
`(let ((errors:*error-pages* (make-hash-table))) ...)` per test.
Cases:
- unknown path → 404, default page contains "404".
- `(define-error-page 404 ...)` used; `:4xx` used for 403 when no exact; `t` catch-all;
  exact beats class.
- POST to GET-only route → 405, `Allow: GET, HEAD`; same path in two modules → union.
- HEAD on GET route → 200 (myway allow-head), body empty for errors.
- controller `(abort-with 403 :message "x")` → 403; `(error "boom")` → 500 with
  `*debug*` nil (no backtrace in body) and debug page with `*debug*` t.
- page fn that signals → plain-text fallback with the original status.
- page fn returning a Clack list (e.g. redirect) passed through.
- `Accept: application/json` → JSON body/content-type; `HX-Request` → fragment.
- error inside `ah:render-to-string` in a controller → 500 (eager rendering).
- controller's own `handler-case` around `abort-with` still wins.
- regression: existing `t/routes`, `t/modules`, `t/auth`, `t/static` pass unchanged;
  module fall-through (`shiso-auth` at "" + "/polls") still works.
Run: `make test` / `sbcl --load test.lisp` (which calls `asdf:test-system :shiso`).

## 7. Migration notes
- Response bodies of 404/400/500 change (HTML page instead of "Not found"); status codes
  unchanged except wrong-method requests now 405 (previously 404).
- Apps catching errors themselves keep working.
- `wrap-error-logging` removed; anyone calling it directly → `wrap-http-errors`.
- `debugp` defaults to T in both `%start` and skeleton `config:debugp` → production
  deployments MUST set `DEBUGP=false` or backtraces are shown publicly. Consider flipping
  the debug page's default (open question).
- Handlers that return `(404 ...)` lists directly are NOT rewritten (opt-in only).
- Streaming/SSE (function) responses: errors after the response starts are outside
  the middleware extent; still uncaught (same as today).

## 8. Open questions
1. Should a returned `(404 headers body)` list with empty body be rewritten via the
   registry too ("status interception")? Plan: no, keep explicit.
2. Per-application vs global registry (multiple apps in one image).
3. Debug page default: on when `debugp` (default T) — safe enough?
4. Full Accept q-value parsing?
5. Does almighty-html escape string children by default? (Needed for the debug page and
   for `message` text.) Verify in `almighty-html/src/element.lisp` before shipping.
6. Should `forbidden-response`/guards signal `forbidden` instead (simpler, but changes the
   `:around` handled contract)?

## Review findings
Second pass, re-reading source (`shiso/src/{server,routing,requests,utils,modules,package}.lisp`,
`src/auth/guards.lisp`, `src/i18n/{middleware,controllers}.lisp`, `shiso.asd`, myway
`src/{mapper,rule}.lisp`, lack `src/middleware/csrf.lisp`). Changes made to the plan:

- **R1 Logging would have been silently lost.** Draft put `wrap-http-errors`
  (handler-case) *inside* the existing `wrap-error-logging` (handler-bind). The inner
  handler is found first and unwinds, so the outer logger never runs → 500s unlogged.
  Fix: single middleware; log in a `handler-bind` handler before the non-local exit
  (`return-from`), which also keeps the backtrace available (a `handler-case` clause runs
  after unwinding — the stack is gone).
- **R2 404 cannot be signalled in the module.** `lack/component:call` on a module returns
  `(values 404 nil)` so `make-module-mount-middleware` falls through to the next mount.
  Signalling there breaks multi-module apps (e.g. mysite: `shiso-auth` at "" before
  "/polls"). Only the final lambda in `define-application` signals.
- **R3 asd order / read-time symbol refs.** `define-application`'s expansion mentions
  `shiso/errors:` symbols, which are read when `routing.lisp` is compiled →
  `errors.lisp` must load before `routing`. But locale rebinding needs `shiso/i18n`,
  which loads later. Fix: `errors.lisp` depends only on almighty-html/uiop; it exposes a
  `*render-around*` hook (function of env + thunk); `server.lisp` (loads after i18n) sets
  it to bind `i18n:*locale*` via `best-locale`.
- **R4 myway can't tell 404 from 405.** `myway:dispatch` checks method inside
  `match-rule`; a method mismatch is just "no match", and using dispatch to probe would
  *execute* controllers. Fix: probe with `myway.rule::match-rule` (internal symbol, same
  style as the existing `myway.rule::rule-param-keys` use in `define-route`) per method,
  `:allow-head nil`, and add HEAD when GET is allowed (myway's `allow-head T` default
  already serves HEAD for GET routes).
- **R5 `setf getf` on env doesn't propagate.** Adding a new key with `(setf (getf env ...))`
  pushes onto the callee's local variable; the outer fallback lambda never sees it.
  Fix: `wrap-http-errors` binds a per-request special `*allowed-methods*` (nil); modules
  `union` into it; the fallback reads it. Per-request `let` inside the middleware is
  thread-safe under Woo.
- **R6 `*debug*` and Woo threads.** A `let` around `clackup` in `%start` is invisible to
  worker threads. Fix: `defvar` global + `setf` in `%start` (or capture `debugp` in the
  closure built by `wrap-app`). Documented in the docstring.
- **R7 Recursive failure.** Error page fn / `render-to-string` / JSON encoding can signal
  while we're in a handler. Fix: `render-error-response` wraps everything in
  `handler-case` → `plain-fallback`, and is called after `return-from` semantics so no
  nested `wrap-http-errors` handler re-enters. Debug page also guarded.
- **R8 Guards keep their contract.** The module `:around` method in `auth/guards.lisp`
  returns `(values response t)`; signalling `forbidden` there would still work
  (propagates through the mount middleware) but changes control flow for redirects.
  Plan keeps response-returning and only reuses the renderer; listed as open question.
- **R9 CSRF 400 bypassed the registry** (`lack/middleware/csrf::return-400`). Use its
  `:block-app` key to signal `bad-request`.
- **R10 almighty-html errors are caught.** `ah:render-to-string` is eager (string built
  inside the controller call), so rendering errors occur within the middleware extent.
  Only function/streaming responses escape — documented.
- **R11 No JSON library** in `shiso.asd` deps → hand-rolled `json-escape` rather than
  adding a dependency.
- **R12 Security.** Debug page must not print the env plist (session id, CSRF token);
  `debugp` defaults to T in `%start` and in the skeleton config → loud migration note.
- **R13 `abort-with` with `:allowed`** for non-405 classes would signal an invalid
  initarg error; strip it.
- **R14 Tests** use lisp-unit2 and the explicit `run-tests :package` list in
  `shiso.asd :perform` — new package must be added there or it silently never runs.
- **R15 i18n `set-language`** already hand-builds a 405 without `Allow`; migrated.
