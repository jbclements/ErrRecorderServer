#lang racket

(require racket/date
         srfi/13
         (planet neil/levenshtein:1:3)
         (planet jaymccarthy/mongodb:1:7))

; the mongodb connection
(define m (create-mongo))

; errrecorder mongo db
(define db (make-mongo-db m "errrecorderdb"))

; set the current mongo db to errrecorder db
(current-mongo-db db)

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

; create/define solutions collection
(define solutions-collection (make-mongo-collection db "solutions"))

; create/define errors collection
(define errors-collection (make-mongo-collection db "errors"))

; create/define nextgids collection
(define nextgids-collection (make-mongo-collection db "nextgids"))

; insert-error : str? str? str? -> nothing
; inserts error into errors collection
(define (insert-error type time msg)
  (local [(define (determine-group err log)
            (let* ([err-tok (string-tokenize err)])
              (if (empty? log)
                  (get-nextgid type)
                  (let* ([idxs (reverse (create-idx-list (first (get-maxgid type))))]
                         [lvnshtns-avgs (map (λ (x y) (cons (lvnshtns-avg err-tok x) y)) log idxs)]
                         [best (argmin car lvnshtns-avgs)]
                         [best-lvnshtn (car best)]
                         [best-idx (cdr best)])
                    (if (<= best-lvnshtn lvnshtn-cutoff)
                        best-idx
                        (get-nextgid type))))))
          
          (define (lvnshtns-avg err-tok cg)
            (let* ([cg-tok (map string-tokenize cg)]
                   [lvnshtns (map (λ (x) (levenshtein/predicate err-tok x string=?)) cg-tok)]
                   [mean (/ (foldl + 0 lvnshtns) (length lvnshtns))])
              mean))
          
          (define (create-idx-list num)
            (if (< num 0)
                empty
                (cons num (create-idx-list (- num 1)))))
          
          (define (create-log maxgid)
            (if (empty? maxgid)
                empty
                (map (λ (x) (get-type-messages type x)) (reverse (create-idx-list (first maxgid))))))]
    
    (make-error #:type type
                #:gid (determine-group msg (create-log (get-maxgid type)))
                #:time time
                #:msg msg)))

; insert-solution : str? num? str? str? -> nothing
; inserts solution into solutions collection
(define (insert-solution type gid text author) 
  (when (and (> (string-length text) 0) (> (string-length author) 0))
    (make-solution #:type type
                   #:gid gid
                   #:time (number->string (current-seconds))
                   #:text text
                   #:author author
                   #:rating 0)))

; get-nextgid : str? -> num
; returns the next unique gid for the specified type
(define (get-nextgid type)
  (local [(define (inc-gid type gid)
            (begin (mongo-collection-modify! nextgids-collection 
                                             (list (cons 'type type) (cons 'gid gid))
                                             (list (cons '$set (list (cons 'gid (+ 1 gid))))))
                   (+ 1 gid)))
          
          (define (insert-nextgid type) 
            (begin (make-nextgid #:type type
                                 #:gid 0)
                   0))]
    
    (let* ([gid-list (get-maxgid type)])
      (if (empty? gid-list)
          (insert-nextgid type)
          (inc-gid type (first gid-list))))))


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

; get-gid : str? str? -> (listof num?)
; returns a gid for a given type+msg in the errors collection
(define (get-gid type msg)
  (for/list ([e (mongo-collection-find errors-collection (list (cons 'type type) (cons 'msg msg)))]) 
    (hash-ref e 'gid)))

; get-solutions : str? num? -> (listof solution)
; returns a list of every proposed solution for a given type+gid in the solutions collection
(define (get-solutions type gid)
  (for/list ([e (mongo-collection-find solutions-collection (list (cons 'type type) (cons 'gid gid)))]) 
    (list (hash-ref e 'time) (hash-ref e 'text) (hash-ref e 'author) (hash-ref e 'rating))))

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

(provide insert-error
         insert-solution
         get-all-types
         get-type-messages
         get-solutions
         get-gid
         upvote
         downvote)