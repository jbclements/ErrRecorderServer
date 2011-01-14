#lang racket

(require "new-model.rkt"
         "signatures.rkt"
         srfi/13
         (planet neil/levenshtein:1:3)
         (planet jaymccarthy/mongodb:1:7)
         rackunit)


(define db-name "errrecorderdb2")

(define-values/invoke-unit new-model@
  (import db-name^)
  (export db-funs^))

#;(define types (get-all-types))

(define gs (apply append
                  (map group->messages
                       (type->groups "exn:fail:contract"))))

(random-seed 3234)

(define (pick-n-random n l)
  (when (< (length l) (* 4 n))
    (error 'pick-n-random "don't bother splitting ~a items into ~a groups" l n))
  (define elts (build-list n (lambda (dc) (list-ref l (random (length l))))))
  (cond [(contains-duplicates? elts) (pick-n-random n l)]
        [else elts]))

(define (contains-duplicates? l)
  (let loop ([l l][so-far `()])
    (cond [(empty? l) #false]
          [else (cond [(member (first l) so-far) #true]
                      [else (loop (rest l) (cons (first l) so-far))])])))


(check-equal? (contains-duplicates? '(a b c d)) #false)
(check-equal? (contains-duplicates? '(a b c b d)) #true)

(define chosen-centers (pick-n-random 10 gs))

(define sets-hash (make-hash))



(define ((compare-to-msg msg) center)
  (levenshtein/predicate (string-tokenize msg) (string-tokenize center) string=?))

(for ([msg (in-list gs)])
  (define best-center (argmin (compare-to-msg msg) chosen-centers))
  (hash-update! sets-hash best-center (lambda (msgs) (cons msg msgs)) `()))

(define sets (hash-map sets-hash (lambda (k v) v)))
