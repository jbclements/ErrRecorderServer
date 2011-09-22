#lang racket

(require web-server/servlet
         web-server/servlet-env
         racket/runtime-path
         "errrecorder-view.rkt"
         "submit-daemon.rkt")

(define-runtime-path here ".")

; start : request? -> (or any nothing)
; start location of servlet:
; either sends error to db or generates summary html-page
(define (viewer-start request) 
  (if (request-post-data/raw request)
      (post-error request)
      (render-page request)))




; servlet settings
(thread
 (lambda ()
   (serve/servlet viewer-start
                  #:launch-browser? #f
                  #:listen-ip #f
                  #:port 8021
                  #:servlet-path "/errrecorder"
                  #:extra-files-paths (list (build-path here "errrecorder-docs")
                                            (build-path here "errrecorder-images")))))


; servlet settings
(thread 
 (lambda ()
   (serve/servlet submitter-start
                  #:launch-browser? #f
                  #:listen-ip #f
                  #:port 8022
                  #:servlet-path "/ers-submit"
                  #:log-file "./submit-log")))
