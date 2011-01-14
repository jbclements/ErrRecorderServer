#lang racket

(require (planet jaymccarthy/mongodb:1:7)
         "add-to-new-database.rkt")

; the mongodb connection
(define m (create-mongo))

; errrecorder mongo db
(define db (make-mongo-db m "errrecorderdb"))

; create/define needed collections:
(define errors-collection (make-mongo-collection db "errors"))


(mongo-collection-count errors-collection `())


(for ([e (mongo-collection-find errors-collection `())]
      [i (in-naturals)])
  (when (= 0 (modulo i 10)) (printf "~a\n" i))
  (new-error-insert! (dict-ref e 'type) (dict-ref e 'time) (dict-ref e 'msg)))




#;(for ([e (mongo-collection-find errors-collection `())]
      [i (in-naturals)])
  (printf "~a\n" i)
  (mongo-collection-modify! errors-collection
                            `((_id . (dict-ref e '_id)))
                            `(($set . ((group-id . ,(dict-ref e 'group)))))))