#lang racket

;; destroy the local database, replace with the master.

(require (planet jaymccarthy/mongodb))

(define conn (create-mongo))


(define (destroy-local-db name)
  (let ([db (mongo-db conn name)])
    (mongo-db-drop db)))

(define db-name "errrecorderdb")

(define (check-for-error response)
  (unless (equal? (hash-ref response 'ok) 1.0)
    (error 'check-for-error "response not okay: ~a\n" response))
  #t)


(define (ggg)
  (check-for-error (destroy-local-db db-name))
  (check-for-error
   (mongo-db-execute-command!
    (mongo-db conn db-name)
    '((clone . "localhost:57017")))))

