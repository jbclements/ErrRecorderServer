#lang racket

(require web-server/servlet
         racket/date
         net/uri-codec
         srfi/13
         srfi/14
         (planet jaymccarthy/mongodb:1:7)
         "errrecorder-model.rkt")

; render-page : request? -> any
; determines which page to render
(define (render-page request)
  (let ([binds (request-bindings request)])
    (if (and (exists-binding? 'type binds) (exists-binding? 'msg binds))
        (determine-group-page request)
        (render-summary-page request))))

; determine-group-page : request? -> html-response
; determines which group page to render
(define (determine-group-page request)
  (let* ([binds (request-bindings request)]
         [type (form-urlencoded-decode (extract-binding/single 'type binds))]
         [msg (form-urlencoded-decode (extract-binding/single 'msg binds))]
         [gid (first (get-gid type msg))])
    (render-group-page request type gid)))

; render-summary-page : request? -> html-response
; generate summary/home page
(define (render-summary-page request)
  (local [(define (response-generator make-url)
            (let* ([query-results (get-all-types)]
                   [total (length query-results)]
                   [groups (count-groups query-results)]
                   [groups-ranked (sort groups (λ(x y) (> (third x) (third y))))])
              `(html (head (title "ErrRecorder"))
                     (link ((rel "stylesheet")
                            (href "/errrecorder-style-sheet.css")
                            (type "text/css")))
                     (body
                      (a ((href "http://li21-127.members.linode.com:8021/errrecorder")) (img ((src "errrecorder-logo.png"))))
                      (p "Total Errors Submitted: " (b ,(format "~a" total)))
                      (div ((id "whatis"))
                           (h3 ((id "whatis-h3")) "What is ErrRecorder?")
                           (p ((id "whatis-p")) 
                              (string-append "ErrRecorder is a Dr. Racket collection that records "
                                             "errors received by users while programming in Dr. "
                                             "Racket.  It groups like errors and allows users to "
                                             "contribute solutions.  In Dr. Racket, a user can be "
                                             "taken to an error's page by clicking on the magnifying "
                                             "glass icon next to the error.")))
                      ,(render-groups make-url groups-ranked total)))))
          
          (define (count-groups qr)
            (if (empty? qr)
                empty
                (count-groups-h (sort qr (λ (x y) (if (string=? (first x) (first y))
                                                      (> (second x) (second y))
                                                      (string>? (first x) (first y))))) "" -1 0)))
          
          (define (count-groups-h qr last-type last-gid count)
            (cond [(empty? qr) (cons (list last-type last-gid count) empty)]
                  [(and (string=? (first (first qr)) last-type) (= (second (first qr)) last-gid)) 
                   (count-groups-h (rest qr) last-type last-gid (+ count 1))]
                  [else (if (string=? last-type "")
                            (count-groups-h (rest qr) (first (first qr)) (second (first qr)) 1)
                            (cons (list last-type last-gid count) 
                                  (count-groups-h (rest qr) (first (first qr)) (second (first qr)) 1)))]))]
    
    (send/suspend/dispatch response-generator)))

; render-groups : make-response (listof group) num? -> html-response
; generates list of groups for summary page
(define (render-groups make-url groups total)
  (local [(define (render-group/make-url group)
            (render-group make-url group total))]
    (if (> total 0)
        `(div ((id "groups"))
              ,@(map render-group/make-url groups))
        `(p "No Errors in ErrRecorder Database."))))

; render-group : make-response group num? -> html-response
; generates a specific group link for summary page
(define (render-group make-url group total)
  (local [(define (view-group-handler request)
            (render-group-page request (first group) (second group)))]
    (let* ([exn-type (first group)]
           [gid (second group)]
           [count (third group)]
           [exn-msg (last (get-type-messages exn-type gid))]
           [per-str (format "~a" (exact->inexact (* 100 (/ count total))))]
           [per-str-len (string-length per-str)]
           [percent (substring per-str 0 (if (> per-str-len 5) 5 per-str-len))])
      `(div ((id "type"))
            (p ,(format "~a (~a%)" count percent) "  "
               (a ((href ,(make-url view-group-handler)))
                  ,(format "~a e.g. \"~a\"" exn-type exn-msg)))))))

; render-group-page : request? str? num? -> html-response
; generates type page for the specified group
(define (render-group-page request type gid)
  (local [(define (response-generator make-url)
            `(html (head (title ,(format "~a Page" type)))
                   (link ((rel "stylesheet")
                          (href "/errrecorder-style-sheet.css")
                          (type "text/css")))
                   (body
                    (a ((href ,(make-url back-handler))) "Back")
                    (h1 ,type)
                    (h2 "Recent Example Exception Messages:")
                    ,(render-example-messages type gid)
                    (h2 "Solutions:")
                    ,(render-solutions make-url type gid)
                    (h2 "Propose Solution:")
                    (div ((id "solution-form"))
                         (form ((action ,(make-url insert-solution-handler)))
                               "Author: " (input ((type "text") (value "Anonymous") (name "author"))) (br) (br)
                               "Solution: " (br) (textarea ((rows "10") (cols "30") (name "text")) "") (br)
                               (input ((type "submit") (value "Submit"))))))))
          
          (define (back-handler request)
            (render-summary-page request))
          
          (define (insert-solution-handler request)
            (begin
              (insert-solution type 
                               gid
                               (extract-binding/single 'text (request-bindings request)) 
                               (extract-binding/single 'author (request-bindings request)))
              (render-group-page (redirect/get) type gid)))]
    
    (send/suspend/dispatch response-generator)))

; render-solutions : make-response str? num? -> html-response
; generates proposed solutions for this group
(define (render-solutions make-url type gid)
  (local [(define (render-solution/make-url solution)
            (render-solution make-url solution type gid))]
    (let* ([solutions (get-solutions type gid)]
           [sorted-solutions (sort solutions (λ (x y) (if (= (fourth x) (fourth y))
                                                          (> (string->number (first x)) (string->number (first y)))
                                                          (> (fourth x) (fourth y)))))]
           [total (length sorted-solutions)])
      (if (> total 0)
          `(div ((id "solutions"))
                ,@(map render-solution/make-url sorted-solutions))
          `(p "No solutions have been submitted.")))))

; render-solution : make-response solution str? num? -> html-response
; generates a specific solution for this group
(define (render-solution make-url solution type gid)
  (local [(define (upvote-handler request)
            (upvote type gid solution)
            (render-group-page (redirect/get) type gid))
          
          (define (downvote-handler request)
            (downvote type gid solution)
            (render-group-page (redirect/get) type gid))]
    (let* ([time (first solution)]
           [text (second solution)]
           [author (third solution)]
           [rating (fourth solution)])
      `(div ((id "solution"))
            ,(insert-solution-text text)
            (p "Submitted at " (b ,(format "~a" (date->string (seconds->date (string->number time)) #t))) " by " (b ,(format "~a." author)))
            (p "Rating: " 
               (a ((href ,(make-url upvote-handler))) (img ((src "up_arrow.jpg"))))
               (b ,(format "  ~a  " rating))
               (a ((href ,(make-url downvote-handler))) (img ((src "down_arrow.jpg")))) 
               (br))
            (hr)))))

; insert-solution-text : str? -> html
; turns newlines in a string to html tag breaks
(define (insert-solution-text str)
  (local [(define (insert-brs los)
            (if (empty? los) 
                empty
                (cons (first los) (cons '(br) (insert-brs (rest los))))))]
    
    (insert-brs (cons 'p (string-tokenize str (char-set-complement (char-set #\newline)))))))

; render-example-messages : str? num? -> html-response
; generates a list of up to 5 example messages for this group
(define (render-example-messages type gid)
  (let* ([messages (get-type-messages type gid)]
         [total (length messages)]
         [display-messages (reverse (list-tail messages (if (> total 5) (- total 5) 0)))])
    `(ul ,@(map (λ (m) `(li ,m)) display-messages))))

; post-error : request? -> nothing
; sends error off to errors collection in errrecorder mongo db
(define (post-error request)
  (let* ([bindings (request-bindings/raw request)]
         [type (bytes->string/utf-8 (binding:form-value (bindings-assq #"type" bindings)))]
         [time (bytes->string/utf-8 (binding:form-value (bindings-assq #"time" bindings)))]
         [msg (bytes->string/utf-8 (binding:form-value (bindings-assq #"msg" bindings)))])
    (insert-error type time msg)
    "Got it!"))

(provide post-error 
         render-page)