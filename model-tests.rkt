#lang racket

(require "signatures.rkt"
         "new-model.rkt"
         rackunit)


(define db-name "testing")

(define-values/invoke-unit new-model@
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
(error-insert! "exn:bogotype2" 348 "I am a frog")

(define query2 (get-all-groups-with-frequency))
(check-equal? (length query2) 2)
(check-equal? (map second query2) '(2 1))

(let ([messages (group->messages (first (first query)))])
  (check-equal? messages `("I am a frog" "I am not a frog")))

(define first-group (first (first query)))

(solution-insert! first-group "Stop being a frog" "Mr. Helpful")

(let ([solutions (group->solutions first-group)])
  (check-equal? (length solutions) 1)
  (check-equal? (dict-ref (first solutions) 'text) "Stop being a frog"))

(solution-insert! first-group "Learn to enjoy being a frog" "Bob")

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

(check-exn exn:fail? (lambda () (group-find "exn:bogotype" "new error message")))
  
#|
(require (planet jaymccarthy/mongodb:1:7))

; the mongodb connection
(define m (create-mongo))

; errrecorder mongo db
(define db (make-mongo-db m "testing"))


; create/define needed collections:
(define errs (make-mongo-collection db "errors"))
(define groups (make-mongo-collection db "groups"))

(mongo-collection-remove! (make-mongo-collection db "groups") '())

(current-mongo-db db)
(define-mongo-struct group "groups"
  ([name]))

(define g (make-group #:name "happygroup"))

(for/list ([i (mongo-collection-find groups '())])
  i)

|#
#|
(define newgroups (list `((_id . (new-bson-objectid)) (name . "happygroup"))
                        `((name . "sadgroup"))))

(for ([g (in-list newgroups)])
  (mongo-collection-insert-one! groups g))

(for/list ([i (mongo-collection-find groups '())])
  i)
|#
    

