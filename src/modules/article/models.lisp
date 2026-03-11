(defpackage #:article/models
  (:use #:cl))

(in-package #:article/models)

(shiso:define-model article
    ((title :field-type :char
       :max-length 50)
     (body :field-type :text)
     (status :field-type :choice
             :choices ((:draft "Draft")
                       (:published "Published")
                       (:private "Private")))
     (published-at :field-type :datetime
                   :default (local-time:now))
     (category :field-type :char :max-length 200)
     ;; (author :col-type (shiso/models:get-model-name "author"))
     ))

(shiso/admin:define-admin article)
