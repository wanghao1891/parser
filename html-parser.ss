#!/usr/bin/petite --script

;petite --script html-parser.ss hello
(define vocabulary-search (cadr (command-line)))

(define start-tag #f)
(define word '())
(define word-need '())
(define continue 0)
(define times 0)
(define word-need1 '())
(define word-need2 '())
(define word-need3 '())
(define is-display (lambda (x) (if (not start-tag) (display x))))
(define append-word (lambda (x) (set! word (append word (list x)))))

(define get-vocabulary-info
  (lambda (filename get-accent-info)
    (let* ((input-port (open-input-file filename)))
      (let loop ((x (get-char input-port)))
	(if (or (eof-object? x) (not (null? word-need1)))
	    (begin (close-port input-port) 
		   (display word-need)
		   (display word-need1)
		   (set! word-need1 '())
		   ;(set! x (eof-object))
		   )
	    (begin
	      (cond
	       ((equal? x #\<) (set! start-tag #t))
	       ((equal? x #\>) (set! start-tag #f))
	       (else (if (not start-tag)
			 (get-accent-info x))
		     ;(is-display x)		     
		     #;
		     (if (and (not start-tag) (not (equal? continue 0)))
			 (begin (display continue)
				(display " ")
				(display x)
				(display " ")))
		     ))
	      (loop (get-char input-port)))))
      )))

(define get-BrE-info
  (lambda (x)
    (cond 
     ((equal? x #\B) (append-word x) (set! continue 1))
     ((equal? x #\r) (if (or (equal? continue 1) (equal? continue 2)) (append-word x)))
     ((equal? x #\E) (if (or (equal? continue 1) (equal? continue 2)) (append-word x)))
     ((equal? x #\/) 
      (if (equal? continue 1) 
	  (begin (append-word x) 
		 (if (equal? times 0) 
		     (set! times 1) 
		     (begin 
		       (set! word-need word) 
		       (set! continue 2)
		       (set! word '()))))
	  (if (equal? continue 2) 
	      (begin (set! word-need1 word)
		     (set! continue 0)
		     (set! times 0)))))
     (else (if (equal? continue 2)
	       (append-word x)
	       (begin (set! continue 0) (set! word '())))))))

(define get-NAmE-info
  (lambda (x)
    (cond 
     ((equal? x #\N) (append-word x) (set! continue 1))
     ((equal? x #\A) (if (or (equal? continue 1) (equal? continue 2)) (append-word x)))
     ((equal? x #\m) (if (or (equal? continue 1) (equal? continue 2)) (append-word x)))
     ((equal? x #\E) (if (or (equal? continue 1) (equal? continue 2)) (append-word x)))
     ((equal? x #\/) 
      (if (equal? continue 1) 
	  (begin (append-word x) 
		 (if (equal? times 0) 
		     (set! times 1) 
		     (begin 
		       (set! word-need word) 
		       (set! continue 2)
		       (set! word '()))))
	  (if (equal? continue 2) 
	      (begin (set! word-need1 word)
		     (set! continue 0)
		     (set! times 0)))))
     (else (if (equal? continue 2)
	       (append-word x)
	       (begin (set! continue 0) (set! word '())))))))

(system (string-append "wget http://www.oxfordlearnersdictionaries.com/search/english/direct/?q=" vocabulary-search " -O /root/workspace/proxy-node/" vocabulary-search ".html"))

(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-BrE-info)
;(display continue)
;(display times)
;(display word)
;(display word-need)
;(display word-need1)
;(display start-tag)
(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-NAmE-info)

(define t (string->utf8 "b"))
(string-append "%" (number->string (bytevector-u8-ref t 0) 16))
