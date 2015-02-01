(define get-html-content
  (lambda (filename)
    (let* ((input-port (open-input-file filename))
	   (start-tag #f)
	   (is-display (lambda (x) (if (not start-tag) (display x))))
	   (word '())
	   (append-word (lambda (x) (set! word (append word (list x))))))
      (let loop ((x (get-char input-port)))
	(if (eof-object? x)
	    (close-port input-port)
	    (begin
	      (cond
	       ((equal? x #\<) (set! start-tag #t))
	       ((equal? x #\>) (set! start-tag #f))
	       (else (if (not start-tag) 
			 (cond 
			  ((equal? x #\B) (append-word x))
			  ((equal? x #\r) (append-word x))
			  ((equal? x #\E) (append-word x))))
		     (is-display x)
		     ))
	      (loop (get-char input-port)))))
      (display word))))

(get-html-content "/root/workspace/proxy-node/oxford-hello.html")
