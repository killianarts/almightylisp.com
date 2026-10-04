"""Serve the effects test page (index.html here) next to the dev site.

    python3 dev/fx-lab/serve.py [PORT] [SITE]

then open http://127.0.0.1:PORT/fx-lab/ (PORT defaults to 5090, SITE to
http://127.0.0.1:5002, the dev server). /fx-lab/ is this folder; every
other request goes to the site. The test page reaches into the magazine
page in its frame, so the two have to come from the same origin: this.

This folder isn't under static/, so the site never serves it.
"""

import http.server
import mimetypes
import os
import sys
import urllib.error
import urllib.request

PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 5090
SITE = (sys.argv[2] if len(sys.argv) > 2 else "http://127.0.0.1:5002").rstrip("/")
HERE = os.path.dirname(os.path.abspath(__file__))

# Headers that belong to one connection, not to the request or response.
HOP = {"connection", "keep-alive", "proxy-authenticate", "proxy-authorization",
       "te", "trailers", "transfer-encoding", "upgrade", "host", "content-length"}


class NoRedirect(urllib.request.HTTPRedirectHandler):
    """Pass redirects back to the browser, as the site sent them."""

    def redirect_request(self, *args):
        return None


opener = urllib.request.build_opener(NoRedirect)


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_GET(self):
        if self.path == "/fx-lab":
            self.reply(301, b"", {"Location": "/fx-lab/"})
        elif self.path.split("?")[0].startswith("/fx-lab/"):
            self.lab_file()
        else:
            self.forward()

    do_HEAD = do_POST = do_PUT = do_PATCH = do_DELETE = do_GET

    def lab_file(self):
        name = self.path.split("?")[0][len("/fx-lab/"):] or "index.html"
        path = os.path.realpath(os.path.join(HERE, name))
        if not path.startswith(HERE + os.sep) or not os.path.isfile(path):
            self.reply(404, b"Not found", {"Content-Type": "text/plain"})
            return
        with open(path, "rb") as f:
            body = f.read()
        kind = mimetypes.guess_type(path)[0] or "application/octet-stream"
        if kind.startswith("text/") or kind in ("application/javascript", "text/javascript"):
            kind += "; charset=utf-8"
        # Always the file as it is now: it's being edited.
        self.reply(200, body, {"Content-Type": kind, "Cache-Control": "no-store"})

    def forward(self):
        length = int(self.headers.get("Content-Length") or 0)
        data = self.rfile.read(length) if length else None
        headers = {k: v for k, v in self.headers.items() if k.lower() not in HOP}
        request = urllib.request.Request(SITE + self.path, data=data, headers=headers,
                                         method=self.command)
        try:
            response = opener.open(request)
        except urllib.error.HTTPError as e:
            response = e
        except urllib.error.URLError as e:
            self.reply(502, ("Can't reach the site at %s: %s" % (SITE, e.reason)).encode(),
                       {"Content-Type": "text/plain"})
            return
        body = response.read()
        headers = [(k, v) for k, v in response.headers.items() if k.lower() not in HOP]
        self.reply(response.status, body, headers)

    def reply(self, status, body, headers):
        self.send_response(status)
        for k, v in (headers.items() if isinstance(headers, dict) else headers):
            self.send_header(k, v)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(body)

    def log_message(self, *args):
        pass


if __name__ == "__main__":
    print("Effects test page: http://127.0.0.1:%d/fx-lab/  (site: %s)" % (PORT, SITE))
    http.server.ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
