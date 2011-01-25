#lang racket

(require web-server/servlet
         web-server/servlet-env
         "signatures.rkt"
         "model.rkt")


;; create a connection to the error-recorder database:
(define db-name "errrecorderdb")
(define-values/invoke-unit model@
  (import db-name^)
  (export db-funs^))


; contract for request
(provide/contract (start (request? . -> . any/c)))

; start : request? -> (or any nothing)
; start location of servlet:
; either sends error to db or generates summary html-page
(define (start request) 
  (post-error request))


; post-error : request? -> nothing
; sends error off to errors collection in errrecorder mongo db
(define (post-error request)
  (let* ([bindings (request-bindings/raw request)]
         [type (bytes->string/utf-8 (binding:form-value (bindings-assq #"type" bindings)))]
         [time (bytes->string/utf-8 (binding:form-value (bindings-assq #"time" bindings)))]
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
