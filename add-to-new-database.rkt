#lang racket

(require "new-model.rkt"
         "signatures.rkt")

(define db-name "errrecorderdb3")

(define-values/invoke-unit new-model@
  (import db-name^)
  (export db-funs^))

(provide (rename-out [error-insert! new-error-insert!]))
