#lang racket

(require "signatures.rkt"
         "model.rkt")

(define db-name "errrecorderdb2")

(define-values/invoke-unit new-model@
  (import db-name^)
  (export db-funs^))

(define g (sort (get-all-groups-with-frequency) > #:key second))

