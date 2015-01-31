(define get-html-content
  (lambda (filename)
    (let ((input-port (open-input-file filename)))
      (let loop ((x (get-char input-port)))
	(if (eof-object? x)
	    (close-port input-port)
	    (begin
	      (cond
	       ((equal? x #\<) (display x)))
	      ;(display x)
	      (loop (get-char input-port))))))))

(get-html-content "/root/workspace/proxy-node/oxford-vocabulary.html")
