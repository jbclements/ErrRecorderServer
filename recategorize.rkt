#lang racket

(require (planet jaymccarthy/mongodb:1:7)
         "add-to-new-database.rkt")

; the mongodb connection
(define m (create-mongo))

; errrecorder mongo db
(define db (make-mongo-db m "errrecorderdb2"))

; create/define needed collections:
(define errors-collection (make-mongo-collection db "errors"))

(define db3 (make-mongo-db m "errrecorderdb3"))

(define newer-errors-collection (make-mongo-collection db2 "errors"))
(define newer-groups-collection (make-mongo-collection db2 "groups"))

#|
(mongo-collection-count errors-collection `())

(define-values (more? get!) (sequence-generate (mongo-collection-find errors-collection `())))

(for ([e (mongo-collection-find errors-collection `())]
      [i (in-naturals)])
  (printf "~a\n" i)
  (new-error-insert! (dict-ref e 'type) (dict-ref e 'time) (dict-ref e 'msg)))

|#


#;(for ([e (mongo-collection-find errors-collection `())]
      [i (in-naturals)])
  (printf "~a\n" i)
  (mongo-collection-insert! `)
  (mongo-collection-modify! errors-collection
                            `((_id . (dict-ref e '_id)))
                            `(($set . ((group-id . ,(dict-ref e 'group)))))))