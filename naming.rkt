#lang racket

(require srfi/13)

(provide messages->common-token-list)

;; messages->name : (listof string?) -> (listof string?)
;; determine a name by finding the tokens that appear in every sequence,
;; and arranging in the order associated with the first message,
;; with #f for every place where non-shared tokens occur
(define (messages->common-token-list messages)
  (when (empty? messages)
    (raise-type-error 'messages->name "nonempty list" 0 messages))
  (define hash (make-hash))
  (define num-messages (length messages))
  (define (increment-token-count t) 
    (hash-set! hash t (add1 (hash-ref hash t 0))))
  (define (add-to-hash message)
    (for-each increment-token-count
              (remove-duplicates (string-tokenize message))))
  (for-each add-to-hash messages)
  (define tokens-to-keep
    (map first
         (filter (lambda (a) (= (second a) num-messages))
                 (hash-map hash list))))
  (foldr (lambda (new-tok so-far)
           (cond [(member new-tok tokens-to-keep) (cons new-tok so-far)]
                 [else (cond [(and (cons? so-far) (equal? (first so-far) #f)) so-far]
                             [else (cons #f so-far)])]))
         `()
         (string-tokenize (first messages))))


