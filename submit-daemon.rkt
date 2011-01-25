#lang racket

(require web-server/servlet
         web-server/servlet-env
         racket/date
         "signatures.rkt"
         "model.rkt")


;; create a connection to the error-recorder database:
(define db-name "errrecorderdb")
(define-values/invoke-unit model@
  (import db-name^)
  (export db-funs^))


; contract for request
(provide/contract (start (request? . -> . any/c)))

; post-error : request? -> nothing
; sends error off to errors collection in errrecorder mongo db
(define (start request)
  (let* ([bindings (request-bindings/raw request)]
         [time (number->string (current-seconds))]
         [type (bytes->string/utf-8 (binding:form-value (bindings-assq #"type" bindings)))]
         [msg (bytes->string/utf-8 (binding:form-value (bindings-assq #"msg" bindings)))])
    (error-insert! type time msg)
    (response/xexpr
     '(success))))


; servlet settings
(serve/servlet start
               #:launch-browser? #f
               #:listen-ip #f
               #:port 8022
               #:servlet-path "/ers-submit"
               #:log-file "./submit-log")
