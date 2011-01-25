#lang racket/unit

(require racket/list
         racket/dict
         srfi/13
         (planet neil/levenshtein:1:3)
         (planet jaymccarthy/mongodb:1:7))

(require "signatures.rkt")

(import db-name^)
(export db-funs^)

;; what does the database look like?
;;
;; an error has 
;; - a type (string, e.g. "exn:fail")
;; - a group-id, the Mongo ID of a group
;; - a time, in seconds since epoch, and
;; - a msg, the exn-message string
(define-mongo-struct error "errors"
  ([type]
   [group-id]
   [time]
   [msg]))

;; a group has
;; - a type (string, e.g. "exn:fail")
;; - a message, the string associated with the
;;   error that caused the creation of the group.
(define-mongo-struct group "groups"
  ([type]
   [msg]))

;; 
(define-mongo-struct solution "solutions"
  ([type]
   [group-id]
   [time]
   [text]
   [author]
   [rating]))






; the mongodb connection
(define m (create-mongo))

; errrecorder mongo db
(define db (make-mongo-db m db-name))

; create/define needed collections:
(define solutions-collection (make-mongo-collection db "solutions"))
(define errors-collection (make-mongo-collection db "errors"))
(define groups-collection (make-mongo-collection db "groups"))

;; clear-all-collections! : delete the database (as long as it's name is "testing")
(define (clear-all-collections!)
  (unless (string=? db-name "testing")
    (error 'clear-all-collections! "I won't clear databases other than 'testing'."))
  (mongo-collection-remove! solutions-collection '())
  (mongo-collection-remove! groups-collection '())
  (mongo-collection-remove! errors-collection '()))

; error-insert! : str? str? str? -> nothing
; inserts error into errors collection
(define (error-insert! type time msg)
  
  (define group (group-find-or-create type msg))
  
  (parameterize ([current-mongo-db db])
    (make-error #:type type
                #:group-id (id-of group)
                #:time time
                #:msg msg)))


; solution-insert! : group? str? str? -> nothing
; inserts solution into solutions collection
(define (solution-insert! group text author) 
  (when (and (> (string-length text) 0) (> (string-length author) 0))
    (parameterize ([current-mongo-db db])
      (make-solution #:type (dict-ref group 'type)
                     #:group-id (id-of group)
                     #:time (number->string (current-seconds))
                     #:text text
                     #:author author
                     #:rating 0))))

;; get-all-groups-with-frequency : return a list of the groups paired
;; with the number of errors in each one
;; -> (listof (list/c group number))
(define (get-all-groups-with-frequency)
  (for/list ([group (mongo-collection-find groups-collection `())])
    (list group 
          (mongo-collection-count errors-collection
                                  `((group-id . ,(id-of group)))))))

;; get-all-types : -> (listof string?)
;; return a list of all the types that occur in the errors
(define (get-all-types)
  (remove-duplicates
   (for/list ([error (mongo-collection-find errors-collection `())])
     (dict-ref error 'type))))


; group->messages : group -> (listof string?)
; returns a list of every message for a given type+gid in the errors collection
(define (group->messages group)
  (for/list ([e (mongo-collection-find errors-collection 
                                       `((group-id . ,(id-of group))))]) 
    (hash-ref e 'msg)))


; group->solutions : group? -> (listof solution)
; returns a list of every proposed solution for a given group in the solutions collection
(define (group->solutions group)
  (for/list ([e (mongo-collection-find 
                 solutions-collection
                 `((group-id . ,(id-of group))))])
    e))

; upvote! : solution ->
; adds 1 to the rating of a solution in the solutions collection
(define (solution-upvote! solution)
  (mongo-collection-modify! 
   solutions-collection 
   `((_id . ,(id-of solution)))
   `(($inc . ((rating . 1))))))

; solution-downvote! : solution -> 
; subtracts 1 to the rating of a solution in the solutions collection
(define (solution-downvote! solution)
  (mongo-collection-modify! 
   solutions-collection 
   `((_id . ,(id-of solution)))
   `(($inc . ((rating . -1))))))

(define (solution->group solution)
  (id->group (dict-ref solution 'group-id)))

;; type->groups : string? -> (listof group?)
(define (type->groups type)
  (for/list ([g (mongo-collection-find groups-collection `((type . ,type)))])
    g))

;; sequence-length : return the length of a sequence
(define (sequence-length sequence)
  (for/fold ([length 0])
    ([item sequence])
    (+ length 1)))


;; group-create! : string? string? -> id
(define (group-create! type msg)
  (parameterize ([current-mongo-db db])
    (define id (mongo-dict-id
                (make-group #:type type
                            ;; not really sure the msg should be in there:
                            #:msg msg)))
    (id->group id)))

;; group-find-or-create : return a gid for this type and message, possibly new.
;; string? (listof string?) -> id
(define (group-find-or-create type err)
  
  (define groups (mongo-collection-find groups-collection `((type . ,type))))
  (define tokens (string-tokenize err))
  
  (or (best-existing-below-threshold type tokens groups)
      (group-create! type err)))

;; group-find : find the group of an error with this type and message
;; string? string? -> group?
(define (group-find type msg)
  (define matching (mongo-collection-find errors-collection
                                          `((type . ,type)
                                            (msg . ,msg))))
  (define-values (more? get!) (sequence-generate matching))
  (unless (more?) (error 'group-find "didn't find any matching errors"))
  (define the-error (get!))
  (id->group (dict-ref the-error 'group-id)))

;; string? (listof string?) cursor? -> (or/c false id)
(define (best-existing-below-threshold type err-tok groups)
  #;(define evaluator (lvnshtns-avg err-tok))
  (define (group->score g)
    ;; for now, we're going to try just comparing to the message stored in the group.
    ;; this should be *much* faster, and will probably give similar results.
    (define group-msg (dict-ref g 'msg))
    (levenshtein/predicate err-tok (string-tokenize group-msg) string=?)
    #;(define errors (mongo-collection-find errors-collection `((group-id . ,(id-of g)))))
    #;(evaluator errors))
  (define with-scores (for/list ([g groups])
                        (list g (group->score g))))
  (cond [(empty? with-scores) #f]
        [else 
         (define best (argmin second with-scores))
         (cond [(< (second best) lvnshtn-cutoff) (first best)]
               [else #f])]))

;; determine the mean levenshtein distance to each of the elements in cg.
;; (listof string) -> cursor? -> number?
(define ((lvnshtns-avg err-tok) error-cursor)
  ;; assuming that the cursor can't be empty
  (define distances
    (for/list ([e error-cursor])
      (levenshtein/predicate err-tok (string-tokenize (dict-ref e 'msg)) string=?)))
  (cond [(empty? distances) +inf.0]
        [else (mean distances)]))

;; GROUP NAMING


(define (mean l)
  (/ (foldl + 0 l) (length l)))


; the levenshtein cutoff for making a group
(define lvnshtn-cutoff 3)

;; get the '_id field of a doc
(define (id-of doc)
  (dict-ref doc '_id))

;; id->group : find the group 
(define (id->group id)
  (exactly-one (mongo-collection-find groups-collection `((_id . ,id)))))

(define (exactly-one sequence)
  (define-values (more? get!) (sequence-generate sequence))
  (unless (more?) 
    (raise-type-error 'exactly-one "sequence containing one element" 0 sequence))
  (define first (get!))
  (when (more?)
    (raise-type-error 'exactly-one "sequence containing one element" 0 sequence))
  first)