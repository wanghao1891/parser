#!/usr/bin/petite --script

(define vocabulary-search (cadr (command-line)))

(define process-file
  (lambda (name)
    (let ((input-port (open-input-file name))
	  (is-tag #f);check wether the char is the tag.
	  )
      (let loop ((x (get-char input-port)))
	(if (eof-object? x)
	    (close-port input-port)
	    (begin
	      (cond
	       ((equal? x #\<) (set! is-tag #t))
	       ((equal? x #\>) (set! is-tag #f))
	       (else
		(if (not is-tag)
		    (display x))))
	      (loop (get-char input-port)))))
      )))

(define http-path "/root/workspace/proxy-node/")
(define out-file (string-append http-path "out/" vocabulary-search ".html"))
(system (string-append "wget http://www.oxfordlearnersdictionaries.com/search/english/direct/?q=" vocabulary-search " -O " out-file))
(process-file out-file)
