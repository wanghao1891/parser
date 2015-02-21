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
	      (set! insert-command (string-append insert-command " " word))
	      (if (not flag-encode)
		  (download-mp3 word))
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

(define download-mp3
  (lambda (url)
    (let* ((ls (string-split url "/"))
	   (filename (list-ref ls (- (length ls) 1)))
	   (path (string-append http-path 
				(car
				 (string-split 
				  (cadr 
				   (string-split url "www.oxfordlearnersdictionaries.com/"))
				  filename)))))
      ;(display (string-append path filename))
      (system (string-append "mkdir -p " path))
      (system (string-append "wget " url " -O " path filename)))))

;(string-split "http://www.oxfordlearnersdictionaries.com/media/english/uk_pron/h/hel/hello/hello__gb_1.mp3" "/")
;(string-split "media/english/uk_pron/h/hel/hello/hello__gb_1.mp3" "hello__gb_1.mp3")

;(define url "http://www.oxfordlearnersdictionaries.com/media/english/uk_pron/h/hel/hello/hello__gb_1.mp3")
;(define filename "hello__gb_1.mp3")
#;
(car
 (string-split
  (cadr (string-split url "www.oxfordlearnersdictionaries.com/"))
  filename))

;(download-mp3 "http://www.oxfordlearnersdictionaries.com/media/english/uk_pron/h/hel/hello/hello__gb_1.mp3")

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

(define string-split
  (lambda (src separator)
    (let ((src-list (string->list src))
	  (separator-list (string->list separator))
	  (num-match 0)
	  (piece "")
	  (piece-tmp ""))
      (let loop (;(char (car src-list))
		 (ls src-list))
	(cond
	 ((null? ls) (list piece))
	 (else
	  (let ((result (process-string-split (car ls) separator-list num-match piece piece-tmp)))
	    ;(display result)
	    (set! num-match (list-ref result 0))
	    (set! piece (list-ref result 1))
	    (set! piece-tmp (list-ref result 2))
	    (append
	     (if (list-ref result 3)
		 (let ((tmp piece))
		   (set! piece "")
		   (list tmp))
		 '())
	     (loop (cdr ls))))))))))

(define process-string-split
  (lambda (char separator num-match piece piece-tmp)
    ;(display "(process-string-split ") (display char) (display " ") (display separator) (display " ") (display num-match) (display " ") (display piece)
    (let ((num-match-tmp num-match)
	  (key-length (length separator))
	  (is-done #f))
      (cond 
       ((equal? char (list-ref separator num-match))                                                                         
	(set! num-match (+ num-match 1))
	(set! piece-tmp (string-append piece-tmp (string char))))
       (else (if (> num-match 0)
		 (begin (set! piece (string-append piece piece-tmp))
			(set! piece-tmp "")))
		 (set! num-match 0)                                                             
		 (set! piece (string-append piece (string char)))))
      (if (= num-match key-length)                                                                                    
	  (begin (set! num-match 0)
		 (set! piece-tmp "")
		 (set! is-done #t)))
      (list num-match piece piece-tmp is-done))))

;(process-string-split #\h '(#\/) 0 "")
(string-split "http://www.oxfordlearnersdictionaries.com/media/english/uk_pron/h/hel/hello/hello__gb_1.mp3" "www.oxfordlearnersdictionaries.com/")
(string-split "http://www.oxfordlearnersdictionaries.com/media/english/uk_pron/h/hel/hello/hello__gb_1.mp3" "hello__gb_1.mp3")


;(process-char #\B 0 '() '(#\B #\r #\E #\/ #\/) #\/)
;("BrE//" #\/ #f #t)
;(start end is-tag is-encode)
(define http-path "/root/workspace/proxy-node/")
(define out-file (string-append http-path vocabulary-search ".html"))
(system (string-append "wget http://www.oxfordlearnersdictionaries.com/search/english/direct/?q=" vocabulary-search " -O " out-file))
(process-file out-file
	      '(("BrE//" #\/ #f #t) ("data-src-mp3=\"" #\" #t #f) ("NAmE//" #\/ #f #t) ("data-src-mp3=\"" #\" #t #f)))
(delete-file out-file)
