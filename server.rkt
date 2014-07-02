(module rMUD-server racket/base
  (require racket/tcp)
  (require racket/cmdline)

  (define outport (make-parameter 12345))
  (define hostname (make-parameter "localhost"))
  (define args (current-command-line-arguments))
  (when (= 2 (vector-length args))
    (outport (vector-ref args 0))
    (hostname (vector-ref args 1)))

  (define (close-connections in out)
    (close-input-port in)
    (close-output-port out))

  (define (read-all-chars port)
    (let ((st (open-output-string)))
      (let loop ()
	(when (char-ready? port)
	  (display (read-char port) st)
	  (loop)))
      (get-output-string st)))

  (define (read-all-chars-strip port strip-fn)
    (let ((st (open-output-string)))
      (let loop ()
	(when (char-ready? port)
	  (let ((r (read-char port)))
	    (when (strip-fn r)
	      (display r st)))
	  (loop)))
      (get-output-string st)))

  (define (read-all-chars-blocking port (strip-fn #f))
    (let loop ()
      (if (char-ready? port)
	  (if strip-fn
	      (read-all-chars-strip port strip-fn)
	      (read-all-chars port))
	  (loop))))

  (define (begin-server)
    ;;start listening on our specified port
    (define server (tcp-listen (if (string? (outport))
				   (string->number (outport))
				   (outport))))
    ;;accept an incoming connection on that port
    (define-values (s-in s-out) (tcp-accept server))

    ;;loop to communicate with user
    (let loop ((its 10)
	       (last-line #f))
      ;;we'll exit if its = 0 or the client sent "quit" last cycle
      (unless (or (equal? last-line "quit")
		  (zero? its))
	;;read and strip out newlines and carriage returns
	(let ((r (read-all-chars-blocking s-in 
					  (lambda (c)
					    (cond [(eq? c #\return) #f]
						  [(eq? c #\newline) #f]
						  [else #t])))))
	  ;;silly formatting stuff
	  (display "You said :: " s-out)
	  (display r s-out)
	  (display "\r\n" s-out)
	  (flush-output s-out)
	  (display r)
	  (newline)
	  (flush-output)
	  (loop (sub1 its) r))))

    ;;close connections to client
    (close-connections s-in s-out)
    (tcp-close server))

  (provide begin-server outport hostname)
  (begin-server))
