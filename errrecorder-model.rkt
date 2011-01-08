#lang racket

(require racket/date
         racket/list
         racket/match
         racket/class
         srfi/13
         (planet neil/levenshtein:1:3)
         (planet jaymccarthy/mongodb:1:7))

(provide db-connection%)

(define db-connection%
  (class object%
    (init-field db-name)
    
    (super-new)
    
    ; the mongodb connection
    (define m (create-mongo))

    ; errrecorder mongo db
    (define db (make-mongo-db m db-name))

    ; create/define needed collections:
    (define solutions-collection (make-mongo-collection db "solutions"))
    (define errors-collection (make-mongo-collection db "errors"))
    (define nextgids-collection (make-mongo-collection db "nextgids"))
    

    
    ; insert-error : str? str? str? -> nothing
    ; inserts error into errors collection
    (define (insert-error type time msg)
      
      (define (create-log maxgid)
        (if (null? maxgid)
            null
            (for/list ([i (in-range (first maxgid))]) (get-type-messages type i))))
      
      (parameterize ([current-mongo-db db])
        ;; this really seems like a mis-use of the dict interface...
        (make-error #:type type
                    #:gid (determine-group type msg (create-log (get-maxgid type)))
                    #:time time
                    #:msg msg)))
    
    
    
    ; insert-solution : str? num? str? str? -> nothing
    ; inserts solution into solutions collection
    (define (insert-solution type gid text author) 
      (when (and (> (string-length text) 0) (> (string-length author) 0))
        (parameterize ([current-mongo-db db])
          (make-solution #:type type
                         #:gid gid
                         #:time (number->string (current-seconds))
                         #:text text
                         #:author author
                         #:rating 0))))
    
    ; get-all-types :  -> (listof (listof str? num?))
    ; returns the type and gid every error in the errors collection.
    ;; should this really be public?
    (define/public (get-all-types)
      (for/list ([e (mongo-collection-find errors-collection (list))]) 
        (list (hash-ref e 'type) (hash-ref e 'gid))))
    
    
    ; get-type-messages : str? num? -> (listof str?)
    ; returns a list of every message for a given type+gid in the errors collection
    (define/public (get-type-messages type gid)
      (for/list ([e (mongo-collection-find errors-collection 
                                           (list (cons 'type type) (cons 'gid gid)))]) 
        (hash-ref e 'msg)))
     
    ; get-solutions : str? num? -> (listof solution)
    ; returns a list of every proposed solution for a given type+gid in the solutions collection
    (define (get-solutions type gid)
      (for/list ([e (mongo-collection-find 
                     solutions-collection
                     (list (cons 'type type) (cons 'gid gid)))]) 
        (list (hash-ref e 'time) (hash-ref e 'text) (hash-ref e 'author) (hash-ref e 'rating))))
    
    ; get-gid : str? str? -> (listof num?)
    ; returns a gid for a given type+msg in the errors collection
    (define (get-gid type msg)
      (for/list ([e (mongo-collection-find errors-collection
                                           (list (cons 'type type) (cons 'msg msg)))]) 
        (hash-ref e 'gid)))
    
    (define/public (upvote!) 3)
    
    (define/public (downvote!) 3)
    ))



; the levenshtein cutoff for making a group
(define lvnshtn-cutoff 3)

; an instance of the solutions mongo collection
(define-mongo-struct solution "solutions"
  ([type]
   [gid]
   [time]
   [text]
   [author]
   [rating]))

; an instance of the nextgids mongo collection
(define-mongo-struct nextgid "nextgids"
  ([type]
   [gid]))

; an instance of the errors mongo collection
(define-mongo-struct error "errors"
  ([type]
   [gid]
   [time]
   [msg]))




;; determine what group an error belongs to
;; ? ? ? -> ?
(define (determine-group type err log)
  (let* ([err-tok (string-tokenize err)])
    (or (best-existing-below-threshold type err-tok log)
        (get-nextgid type))))

(define (best-existing-below-threshold type err-tok log)
  (define evaluator (lvnshtns-avg err-tok))
  (cond [(empty? log) #f]
        [else
         (match-let* ([paired (pair-with-ints log)]
                      [(list errs idx avg) (argmin evaluator paired)])
           (cond [(< (evaluator errs) lvnshtn-cutoff) idx]
                 [else #f]))]))

;; pair each element of the list with its index
(define (pair-with-ints l)
  (for/list ([a (in-list l)]
             [b (in-naturals)])
    (list a b)))


;; determine the mean levenshtein distance to each of the elements in cg.
;; string? (listof string?) -> number?
(define ((lvnshtns-avg err-tok) cg)
  (let* ([cg-tok (map string-tokenize cg)]
         [lvnshtns (map (λ (x) (levenshtein/predicate err-tok x string=?)) cg-tok)]
         [mean (/ (foldl + 0 lvnshtns) (length lvnshtns))])
    mean))



; get-nextgid : str? -> num
; returns the next unique gid for the specified type
(define (get-nextgid type)
  (define (inc-gid type gid)
            (begin (mongo-collection-modify! nextgids-collection 
                                             (list (cons 'type type) (cons 'gid gid))
                                             (list (cons '$set (list (cons 'gid (+ 1 gid))))))
                   (+ 1 gid)))
          
          (define (insert-nextgid type) 
            (begin (make-nextgid #:type type
                                 #:gid 0)
                   0))
  (let* ([gid-list (get-maxgid type)])
    (if (empty? gid-list)
        (insert-nextgid type)
        (inc-gid type (first gid-list)))))


; get-maxgid : str? -> num
; returns the max gid for the specified type
(define (get-maxgid type)
  (for/list ([e (mongo-collection-find nextgids-collection (list (cons 'type type)))]) 
    (hash-ref e 'gid)))

; get-all-types : str? -> (listof (listof str? num?))
; returns every error in the errors collection
(define (get-all-types)
  (for/list ([e (mongo-collection-find errors-collection (list))]) 
    (list (hash-ref e 'type) (hash-ref e 'gid))))

; get-type : str? -> (listof str?)
; returns a list of every instance of a type in the errors collection
(define (get-type type)
  (for/list ([e (mongo-collection-find errors-collection (list))]) 
    (list (hash-ref e 'msg) (hash-ref e 'gid))))

; get-type-messages : str? num? -> (listof str?)
; returns a list of every message for a given type+gid in the errors collection
(define (get-type-messages type gid)
  (for/list ([e (mongo-collection-find errors-collection (list (cons 'type type) (cons 'gid gid)))]) 
    (hash-ref e 'msg)))





; upvote : str? num? solution -> nothing
; adds 1 to the rating of a solution in the solutions collection
(define (upvote type gid solution)
  (mongo-collection-modify! solutions-collection 
                            (list (cons 'type type) (cons 'gid gid) (cons 'time (first solution)) (cons 'text (second solution)))
                            (list (cons '$set (list (cons 'rating (+ 1 (fourth solution))))))))

; downvote : str? num? solution -> nothing
; subtracts 1 to the rating of a solution in the solutions collection
(define (downvote type gid solution)
  (mongo-collection-modify! solutions-collection 
                            (list (cons 'type type) (cons 'gid gid) (cons 'time (first solution)) (cons 'text (second solution)))
                            (list (cons '$set (list (cons 'rating (if (= (fourth solution) 0)
                                                                      0
                                                                      (- (fourth solution) 1))))))))

