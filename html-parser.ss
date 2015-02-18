#!/usr/bin/petite --script

;petite --script html-parser.ss hello
(define vocabulary-search (cadr (command-line)))

;(length '(("BrE//" #\/ #f) ("data-src-mp3=\"" #\" #t) ("NAmE//" #\/ #f) ("data-src-mp3=\"" #\" #t)))

(define process-file
  (lambda (name match-list)
    (let ((input-port (open-input-file name))
	  (insert-command 
	   (string-append "cd /root/workspace/database/; petite --script insert.ss data-01 " vocabulary-search)))
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
		      (let ((result (process-char x num-match word start end flag-encode insert-command)))
			;(display result)
			(set! num-match (list-ref result 0))
			(set! word (list-ref result 1))
			(if (list-ref result 2)
			    (set! match-positon (+ match-positon 1)))
			(set! insert-command (list-ref result 3)))))))
	      (if (< match-positon match-length)
		  (loop (get-char input-port)
			num-match
			word
			match-length
			match-positon
			is-tag)))))
      (display insert-command)
      (system insert-command))))

(define process-char
  (lambda (char num-match word start end flag-encode insert-command)
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
	      ;(display word)
	      ;(newline)
	      (set! insert-command ( string-append insert-command " " word))
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
	    is-done
	    insert-command))))

(define encode-uri
  (lambda (char)
    (let* ((vu8 (string->utf8 (string char)))
	   (length (bytevector-length vu8)))
      (let loop ((position 0))
	(cond
	 ((equal? position length) "")
	 (else 
	  (string-append
	   "%"
	   (number->string 
	    (bytevector-u8-ref vu8 position)
	    16)
	   (loop (+ position 1)))))))))

;(process-char #\B 0 '() '(#\B #\r #\E #\/ #\/) #\/)
;("BrE//" #\/ #f #t)
;(start end is-tag is-encode)
(system (string-append "wget http://www.oxfordlearnersdictionaries.com/search/english/direct/?q=" vocabulary-search " -O /root/workspace/proxy-node/" vocabulary-search ".html"))
(process-file (string-append "/root/workspace/proxy-node/" vocabulary-search ".html")
	      '(("BrE//" #\/ #f #t) ("data-src-mp3=\"" #\" #t #f) ("NAmE//" #\/ #f #t) ("data-src-mp3=\"" #\" #t #f)))

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
