#lang racket

(require racket/system
         racket/runtime-path)

(define-runtime-path here ".")
(define errrecorder (build-path here "errrecorder.rkt"))
(define log-file (build-path here "log"))


(let loop ()
  (system (format "racket ~a >> ~a 2>&1" 
                  (path->string errrecorder)
                  (path->string log-file)))
  (sleep 30)
  (loop))
