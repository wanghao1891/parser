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
			 (begin;(display x) 
			 (get-dialect-sound x))
					;(display x)
			 )
					;(is-display x)     
		     #;
		     (if (and (not start-tag) (not (equal? continue 0)))
		     (begin (display continue)
		     (display " ")
		     (display x)
		     (display " ")))))
	      (loop (get-char input-port))))))))

;(length '(("BrE//" #\/ #f) ("data-src-mp3=\"" #\" #t) ("NAmE//" #\/ #f) ("data-src-mp3=\"" #\" #t)))

(define process-file
  (lambda (name match-list)
    (let ((input-port (open-input-file name)))
      (let loop ((x (get-char input-port))
		 (num-match 0);the number of matching.
		 (word "");store the char of matching.
		 (match-length (length match-list));the length of list of matchinig pair.
		 (match-positon 0);the position of the list of matching pair.
		 (is-tag #f));check wether the char is the tag.
	(if (eof-object? x)
	    (close-port input-port)
	    (begin
	      (cond
	       ((equal? x #\<) (set! is-tag #t))
	       ((equal? x #\>) (set! is-tag #f))
	       (else
		(let* ((match (list-ref match-list match-positon))
		       (start (string->list (list-ref match 0)))
		       (end (list-ref match 1))
		       (flag-tag (list-ref match 2))
		       (flag-encode (list-ref match 3)))
		  (if (equal? is-tag flag-tag)
		      (let ((result (process-char x num-match word start end flag-encode)))
			;(display result)
			(set! num-match (list-ref result 0))
			(set! word (list-ref result 1))
			(if (list-ref result 2)
			    (set! match-positon (+ match-positon 1))))))))
	      (if (< match-positon match-length)
		  (loop (get-char input-port)
			num-match
			word
			match-length
			match-positon
			is-tag))))))))

(define process-char
  (lambda (char num-match word start end flag-encode)
    ;(display "process-char") (display " ") (display char) (display " ") (display num-match) (display " ") (display word) (display " ") (display start) (display " ") (display end) (newline)
    (let ((num-match-tmp num-match)
	  (key-length (length start))
	  (is-done #f))
      (cond
       ((< num-match key-length)
	(if (equal? char (list-ref start num-match))
	    (set! num-match (+ num-match 1))))
       (else
	(if (equal? char end)
	    (begin
	      (display word)
	      (newline)
	      (set! word "")
	      (set! num-match 0) 
	      (set! is-done #t)
	      )
	    (set! word (string-append 
			word 
			(if flag-encode
			    (encode-uri char)
			    (string char)))))))
      (list (if (and
		 (equal? num-match-tmp num-match)
		 (< num-match key-length))
		0
		num-match)
	    word
	    is-done))))

(define encode-uri
  (lambda (char)
    (let* ((vu8 (string->utf8 (string char)))
	  (u8 (bytevector-u8-ref vu8 0))
	  (su8 (number->string u8 16)))
      (string-append "%" su8))))

;(list->string '(#\b "%62" #\b))

(string-append (string #\a) (string #\b))

;(process-char #\B 0 '() '(#\B #\r #\E #\/ #\/) #\/)
;("BrE//" #\/ #f #t)
;(start end is-tag is-encode)
(process-file "/root/workspace/proxy-node/hello.html" '(("BrE//" #\/ #f #t) ("data-src-mp3=\"" #\" #t #f) ("NAmE//" #\/ #f #t) ("data-src-mp3=\"" #\" #t #f)))

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
#;
(system (string-append "wget http://www.oxfordlearnersdictionaries.com/search/english/direct/?q=" vocabulary-search " -O /root/workspace/proxy-node/" vocabulary-search ".html"))
#;
(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-BrE-pronouciation)
;(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-BrE-sound #t)
;(display continue)
;(display times)
;(display word)
;(display word-need)
;(display word-need1)
;(display start-tag)
#;
(get-vocabulary-info (string-append "/root/workspace/proxy-node/" vocabulary-search ".html" ) get-NAmE-pronuciation)

(define t (string->utf8 "b"))
(string-append "%" (number->string (bytevector-u8-ref t 0) 16))
