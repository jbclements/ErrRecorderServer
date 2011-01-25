#lang racket

(require net/url
         net/uri-codec)

(define db-host "localhost"#;"li21-127.members.linode.com")
(define db-submit-port 8022)
(define db-query-port 8021)

(define servlet-path (list (path/param "ers-submit" '())))

(define submit-url 
 (url "http" #f db-host db-submit-port #t servlet-path `() #f))

(define (query-error-url type-str msg-str)
  (url->string
   (url "http" #f db-host db-query-port #t servlet-path
        `((type . ,type-str)
          (msg . ,msg-str))
        #f)))

(define (string->post-bytes s)
  (string->bytes/utf-8
   (form-urlencoded-encode s)))

; symbol->post-bytes : symbol? -> bytes?
; converts symbol into url-encoded bytes
(define symbol->post-bytes
  (compose string->post-bytes symbol->string))

; bindings->post-bytes : (list (symbol? string?)) -> bytes?
; converts a list of symbol to string bindings into url-encoded bytes
(define bindings->post-bytes
  (match-lambda
    [(list) #""]
    [(list (list (? symbol? sym) (? string? val)))
     (bytes-append (symbol->post-bytes sym) #"=" (string->post-bytes val))]
    [(list-rest (list (? symbol? sym) (? string? val)) bs)
     (bytes-append (symbol->post-bytes sym) #"=" (string->post-bytes val)
                   #"&" (bindings->post-bytes bs))]))


(define (test-submit type msg)
  (define in-port
    (post-pure-port
     submit-url 
     (bindings->post-bytes 
      `((type ,type) 
        (time ,(number->string (current-seconds)))
        (msg ,msg)))))
  ;; ignore the result:
  (close-input-port in-port))

(test-submit "bogus-type" "not a real error message")