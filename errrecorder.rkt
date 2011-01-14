#lang racket

(require web-server/servlet
         web-server/servlet-env
         racket/runtime-path
         "errrecorder-view.rkt")

(define-runtime-path here ".")

; contract for request
(provide/contract (start (request? . -> . any/c)))

; start : request? -> (or any nothing)
; start location of servlet:
; either sends error to db or generates summary html-page
(define (start request) 
  (if (request-post-data/raw request)
      (post-error request)
      (render-page request)))

; servlet settings
(serve/servlet start
               #:launch-browser? #f
               #:listen-ip #f
               #:port 8021
               #:servlet-path "/errrecorder"
               #:extra-files-paths (list (build-path here "errrecorder-docs")
                                         (build-path here "errrecorder-images")))
