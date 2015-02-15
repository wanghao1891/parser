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
  (lambda (filename get-dialect-pronuciation)
    (let* ((input-port (open-input-file filename)))
      (let loop ((x (get-char input-port)))
	(if (or (eof-object? x) (not (null? word-need2)))
	    (begin (close-port input-port) 
		   (display word-need)
		   (display word-need1)
		   (display word-need2)
		   (set! word-need1 '())
		   (set! word-need2 '())
		   (set! word '()))
	    (begin
	      (cond
	       ((equal? x #\<) (set! start-tag #t))
	       ((equal? x #\>) (set! start-tag #f))
	       (else (if (equal? start-tag #t)
			 (if (not (null? word-need1)) (get-dialect-sound x))
			 (if (null? word-need1) (get-dialect-pronuciation x))
			 #;
			 (begin		;(display x) 
			   (get-dialect-sound x)
			   )
					;(display x)
			 )
		     ;(is-display x)		     
		     #;
		     (if (and (not start-tag) (not (equal? continue 0)))
			 (begin (display continue)
				(display " ")
				(display x)
				(display " ")))
		     ))
	      (loop (get-char input-port))))))))

#;
(define-syntax get-pronunciation
  (syntax-rules ()
    ((_ e1 f)
     (+ e1 f
	))))

;(get-pronunciation 1 2 3 4 5)
#;
(define get-pronunciation
  (lambda (v1 . v2)
    (map (lambda (x) (+ x v1)) v2)))
#;
(define add 
  (lambda () (+ x y)))
#;
(let ((x 1)
      (y 2))
  (add)
  ((lambda () (+ x y)))
  )

(length '(("BrE//" #\/ #f) ("data-src-mp3=\"" #\" #t) ("NAmE//" #\/ #f) ("data-src-mp3=\"" #\" #t)))

(define process-file
  (lambda (name match-list)
    (let ((input-port (open-input-file name)))
      (let loop ((x (get-char input-port))
		 (num-match 0)
		 (word '())
		 (match-length (length match-list))
		 (match-positon 0)
		 (is-tag #f))
	(if (eof-object? x)
	    (close-port input-port)
	    (begin
	      (cond
	       ((equal? x #\<) (set! is-tag #t))
	       ((equal? x #\>) (set! is-tag #f))
	       (else
		(let* ((match (list-ref match-list match-positon))
		       (start (list-ref match 0))
		       (end (list-ref match 1))
		       (flag (list-ref match 2)))
		  (if (equal? is-tag flag)
		      (let ((result (process-char x num-match word start end)))
			(set! num-match (list-ref result 0))
			(set! word (list-ref result 1)))))))
	      (loop (get-char input-port)
		    num-match
		    word
		    match-length
		    match-positon
		    is-tag)))))))

(define process-char
  (lambda (char num-match word start end)
    (let loop ((key (car start))
	       (key-list (cdr start))
	       (key-length (length start))
	       (key-position 0))
      (if (not (null? list))
	  (cond
	   ((equal? char key)
	    (if (or 
		 (equal? num-match key-position)
		 (equal? num-match key-length))
		(set! word (append word (list x)))
		(set! num-match (+ num-match 1))))
	   ((equal? char end)
	    (if (equal? num-match key-length)
		(begin
		  (display word)
		  (set! num-match 0)
		  (set! word '())))) 
	   (else
	    (if (equal? num-match key-length)
		(set! word (append word (list x)))
		(begin (set! num-match 0) (set! word '()))))))
      (loop (car key-list)
	    (cdr key-list)
	    key-length
	    (+ key-position 1)))))

(define get-BrE-pronouciation
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

(define get-NAmE-pronuciation
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

(define get-dialect-sound
  (lambda (x)
    (cond
     ((equal? x #\d) (append-word x) (if (not (equal? continue 14)) (set! continue 1)) ;(display x)
      )
     ((equal? x #\a) (if (or (equal? continue 1) (equal? continue 3) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
												  )))
     ((equal? x #\t) (if (or (equal? continue 2) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									      )))
     ;((equal? x #\a) (if (or (equal? continue 3) (equal? continue 13)) (begin (append-word x) (set! continue 4) (display x))))
     ((equal? x #\-) (if (or (equal? continue 4) (equal? continue 8) (equal? continue 13)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
												  )))
     ((equal? x #\s) (if (or (equal? continue 5) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									      )))
     ((equal? x #\r) (if (or (equal? continue 6) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									      )))
     ((equal? x #\c) (if (or (equal? continue 7) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									      )))
     ;((equal? x #\-) (if (or (equal? continue 8) (equal? continue 13)) (begin (append-word x) (set! continue (+ continue 1)) (display x))))
     ((equal? x #\m) (if (or (equal? continue 9) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									      )))
     ((equal? x #\p) (if (or (equal? continue 10) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									       )))
     ((equal? x #\3) (if (or (equal? continue 11) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									       )))
     ((equal? x #\=) (if (or (equal? continue 12) (equal? continue 14)) (begin (append-word x) (if (not (equal? continue 14)) (set! continue (+ continue 1))) ;(display x)
									       )))
     ((equal? x #\")
      (if (equal? continue 13)
          (begin (append-word x)
		 ;(display word)
		 ;(set! word-need word)
		 (set! continue 14)
		 (set! word '()))
          (if (equal? continue 14)
              (begin (set! word-need2 word)
                     (set! continue 0)))))
     (else (if (equal? continue 14)
               (begin (append-word x) ;(display x)
		      )
               (begin (set! continue 0) (set! word '())))))))

(system (string-append "wget http://www.oxfordlearnersdictionaries.com/search/english/direct/?q=" vocabulary-search " -O /root/workspace/proxy-node/" vocabulary-search ".html"))

(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-BrE-pronouciation)
;(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-BrE-sound #t)
;(display continue)
;(display times)
;(display word)
;(display word-need)
;(display word-need1)
;(display start-tag)
(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-NAmE-pronuciation)

(define t (string->utf8 "b"))
(string-append "%" (number->string (bytevector-u8-ref t 0) 16))
