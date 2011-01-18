#lang racket

(require web-server/servlet
         racket/date
         net/uri-codec
         srfi/13
         srfi/14
         "new-model.rkt"
         "signatures.rkt"
         "naming.rkt")

(define db-name "errrecorderdb")

(define-values/invoke-unit new-model@
  (import db-name^)
  (export db-funs^))

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
         [group (group-find type msg)])
    (render-group-page request group)))

; render-summary-page : request? -> html-response
; generate summary/home page
(define (render-summary-page request)
  (define (response-generator make-url)
    (define groups-and-counts (sort (get-all-groups-with-frequency) > #:key second))
    (define total (apply + (map second groups-and-counts)))
    (response/xexpr
     `(html (head (title "ErrRecorder"))
            (link ((rel "stylesheet")
                   (href "/errrecorder-style-sheet.css")
                   (type "text/css")))
            (body
             (a ((href "http://li21-127.members.linode.com:8021/errrecorder")) 
                (img ((src "errrecorder-logo.png"))))
             (p "Total Errors Submitted: " (b ,(format "~a" total)))
             (div ((id "whatis"))
                  (h3 ((id "whatis-h3")) "What is ErrRecorder?")
                  (p ((id "whatis-p")) 
                     (string-append "ErrRecorder is a DrRacket collection that records "
                                    "errors received by users while programming in Dr. "
                                    "Racket.  It groups like errors and allows users to "
                                    "contribute solutions.  In DrRacket, a user can be "
                                    "taken to an error's page by clicking on the magnifying "
                                    "glass icon next to the error.")))
             ,(render-groups make-url groups-and-counts total)))))
  
  (send/suspend/dispatch response-generator))

; render-groups : make-response (listof (list/c group number)) -> html-response
; generates list of groups for summary page
(define (render-groups make-url groups total-count)
  (if (> total-count 0)
      `(div ((id "groups"))
            ,@(map (render-group make-url total-count) groups))
      `(p "No Errors in ErrRecorder Database.")))

; render-group : make-response num? -> group -> html-response
; generates a specific group link for summary page
(define ((render-group make-url total) group-and-count)
  (match-define (list group count) group-and-count)
  (define (view-group-handler request)
            (render-group-page request group))
  (define name-tokens (messages->common-token-list (group->messages group)))
  (let* ([exn-type (dict-ref group 'type)]
         [percent (round (* 100 (/ count total)))])
    `(div ((id "type"))
          (p ,(format "~a (~a%)" count percent) "  "
             (a ((href ,(make-url view-group-handler)))
                ,(format "~a e.g. \"~a\"" exn-type (assemble-name name-tokens)))))))

; render-group-page : request? str? group? -> html-response
; generates type page for the specified group
(define (render-group-page request group)
  (define group-type (dict-ref group 'type))
  
  (define (response-generator make-url)
    (response/xexpr
     `(html (head (title ,(format "~a Page" group-type)))
            (link ((rel "stylesheet")
                   (href "/errrecorder-style-sheet.css")
                   (type "text/css")))
            (body
             (a ((href ,(make-url back-handler))) "Back")
             (h1 ,group-type)
             (h2 "Recent Example Exception Messages:")
             ,(render-example-messages group)
             (h2 "Solutions:")
             ,(render-solutions make-url group)
             (h2 "Propose Solution:")
             (div ((id "solution-form"))
                  (form ((action ,(make-url insert-solution-handler)))
                        "Author: " (input ((type "text") (value "Anonymous") (name "author"))) (br) (br)
                        "Solution: " (br) (textarea ((rows "10") (cols "30") (name "text")) "") (br)
                        (input ((type "submit") (value "Submit")))))))))
  
  (define (back-handler request)
    (render-summary-page request))
  
  (define (insert-solution-handler request)
    (begin
      (solution-insert! group
                        (extract-binding/single 'text (request-bindings request)) 
                        (extract-binding/single 'author (request-bindings request)))
      (render-group-page (redirect/get) group)))
  
  (send/suspend/dispatch response-generator))

; render-solutions : make-response str? group? -> html-response
; generates proposed solutions for this group
(define (render-solutions make-url group)
  (local [(define (render-solution/make-url solution)
            (render-solution make-url solution))]
    (let* ([solutions (group->solutions group)]
           [sorted-solutions (sort solutions > #:key solution->rating)]
           [total (length sorted-solutions)])
      (if (> total 0)
          `(div ((id "solutions"))
                ,@(map render-solution/make-url sorted-solutions))
          `(p "No solutions have been submitted.")))))

; render-solution : make-response solution str? num? -> html-response
; generates a specific solution for this group
(define (render-solution make-url solution)
  (define group (solution->group solution))
  
  (define (upvote-handler request)
    (solution-upvote! solution)
    (render-group-page (redirect/get) group))
  
  (define (downvote-handler request)
    (solution-downvote! solution)
    (render-group-page (redirect/get) group))
  
  (let* ([time (dict-ref solution 'time)]
         [text (dict-ref solution 'text)]
         [author (dict-ref solution 'author)]
         [rating (dict-ref solution 'rating)])
    `(div ((id "solution"))
          ,(insert-solution-text text)
          (p "Submitted at " (b ,(format "~a" (date->string (seconds->date (string->number time)) #t))) " by " (b ,(format "~a." author)))
          (p "Rating: " 
             (a ((href ,(make-url upvote-handler))) (img ((src "up_arrow.jpg"))))
             (b ,(format "  ~a  " rating))
             (a ((href ,(make-url downvote-handler))) (img ((src "down_arrow.jpg")))) 
             (br))
          (hr))))

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
(define (render-example-messages group)
  (let* ([messages (group->messages group)]
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
    (error-insert! type time msg)
    (response/xexpr
     '(success))))

(define (solution->rating solution)
  (dict-ref solution 'rating))

(define (assemble-name tokens)
  (apply 
   string-append
   (add-between
    (map (lambda (token)
           (match token
             [#f "*"]
             [other token]))
         tokens)
    " ")))

#;(equal? (assemble-name `("a" "b" #f "c")) "a b * c")

(provide post-error 
         render-page)

