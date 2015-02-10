#!/usr/bin/petite --script

;petite --script html-parser.ss hello
(define vocabulary-search (cadr (command-line)))

(define start-tag #f)
(define word '())
(define word-need '())
(define continue 0)
(define times 0)
(define word-need1 '())
(define is-display (lambda (x) (if (not start-tag) (display x))))
(define append-word (lambda (x) (set! word (append word (list x)))))

(define get-vocabulary-info
  (lambda (filename)
    (let* ((input-port (open-input-file filename)))
      (let loop ((x (get-char input-port)))
	(if (eof-object? x)
	    (close-port input-port)
	    (begin
	      (cond
	       ((equal? x #\<) (set! start-tag #t))
	       ((equal? x #\>) (set! start-tag #f))
	       (else (if (not start-tag)
			 (get-bre-info x))
					;(is-display x)
		     ))
	      (loop (get-char input-port)))))
      (display word-need)
      (display word-need1))))

(define get-bre-info
  (lambda (x)
    (cond 
     ((equal? x #\B) (append-word x) (set! continue 1))
     ((equal? x #\r) (if (equal? continue 1) (append-word x)))
     ((equal? x #\E) (if (equal? continue 1) (append-word x)))
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
		     (set! continue 0)))))
     (else (if (equal? continue 2)
	       (append-word x)
	       (begin (set! continue 0) (set! word '())))))))

(system (string-append "wget http://www.oxfordlearnersdictionaries.com/search/english/direct/?q=" vocabulary-search " -O /root/workspace/proxy-node/" vocabulary-search ".html"))

(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ))

(define t (string->utf8 "b"))
(string-append "%" (number->string (bytevector-u8-ref t 0) 16))
