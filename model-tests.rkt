#lang racket

(require "signatures.rkt"
         "model.rkt"
         rackunit)


(define db-name "testing")

(define-values/invoke-unit model@
  (import db-name^)
  (export db-funs^))

(clear-all-collections!)

(error-insert! "exn:bogotype" 345 "I am a frog")
(error-insert! "exn:bogotype" 347 "I am not a frog")

(define query (get-all-groups-with-frequency))
;; should still only be one group:
(check-equal? (length query) 1)
;; it should have frequency 2
(check-equal? (second (first query)) 2)

;; add one with a different type
(define dc1 (error-insert! "exn:bogotype2" 348 "I am a frog"))

(define query2 (get-all-groups-with-frequency))
(check-equal? (length query2) 2)
(check-equal? (map second query2) '(2 1))

(let ([messages (group->messages (first (first query)))])
  (check-equal? messages `("I am a frog" "I am not a frog")))

(define first-group (first (first query)))

(define dc2 
  (solution-insert! first-group "Stop being a frog" "Mr. Helpful"))

(let ([solutions (group->solutions first-group)])
  (check-equal? (length solutions) 1)
  (check-equal? (dict-ref (first solutions) 'text) "Stop being a frog"))

(define dc3
  (solution-insert! first-group "Learn to enjoy being a frog" "Bob"))

(let ([solutions (group->solutions first-group)])
  (check-equal? (length solutions) 2)
  (check-equal? (dict-ref (second solutions) 'text) "Learn to enjoy being a frog"))

(let ([first-solution (first (group->solutions first-group))])
  (solution-upvote! first-solution)
  (solution-upvote! first-solution)
  (let ([reloaded (first (group->solutions first-group))])
    (check-equal? (dict-ref reloaded 'rating) 2))
  (solution-downvote! first-solution)
  (let ([reloaded (first (group->solutions first-group))])
    (check-equal? (dict-ref reloaded 'rating) 1)))

(check-exn exn:fail? 
           (lambda () (group-find "exn:bogotype" "new error message")))
  
