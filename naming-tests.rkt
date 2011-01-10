#lang racket

(require "naming.rkt"
         rackunit)

(check-exn (lambda (exn) (regexp-match #px"expects argument of type"
                                       (exn-message exn))) 
           (lambda () (messages->common-token-list `())))
(check-equal? (messages->common-token-list `("a b c d"))
              `("a" "b" "c" "d"))
(check-equal? (messages->common-token-list `("a b c e d" "a b c d"))
              `("a" "b" "c" #f "d"))
(check-equal? (messages->common-token-list `("expected huge number, got boolean"
                                "expected boolean, got number"))
              `("expected" #f "got" #f))