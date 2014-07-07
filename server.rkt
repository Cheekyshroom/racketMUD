(module rMUD-server racket/base
  (require racket/tcp)
  (require racket/cmdline)

  (define outport (make-parameter 12346))
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

  (define (handle in out (user #f) (continue #f))
    (let loop ((its 10)
	       (last-line #f))
      ;;we'll exit if its = 0 or the client sent "quit" last cycle
      (unless (or (equal? last-line "quit")
		  (zero? its)
		  (if continue
		      (not (unbox continue))
		      #f))
	;;read and strip out newlines and carriage returns
	(let ((r (read-all-chars-blocking in 
					  (lambda (c)
					    (cond [(eq? c #\return) #f]
						  [(eq? c #\newline) #f]
						  [else #t])))))
	  ;;silly formatting stuff
	  (display "You said: " out)
	  (display r out)
	  (display "\r\n" out)
	  (flush-output out)
	  (display r)
	  (newline)
	  (flush-output)
	  (loop (sub1 its) r)))))

  (define (accept-handle-close-serv server (continue #f))
    (define-values (s-in s-out) (tcp-accept server))
    (accept-handle-close s-in s-out continue))

  (define (accept-handle-close in out continue)
    (handle in out #f continue)
    (close-connections in out)
    (set-box! continue #f))

  ;;close-server = a box that allows this connection to stop receiving incoming connections
  (define (accept-thread-handle-continue server continue)
    (define-values (s-in s-out) (tcp-accept server))
    (thread (lambda ()
	      (accept-handle-close s-in s-out continue))))

  (define (begin-server)
    ;;start listening on our specified port
    (define server (tcp-listen (if (string? (outport))
				   (string->number (outport))
				   (outport))))
    
    (define continue (box #t))
    (let loop ((usern 0))
      (let block ()
	(unless (or (tcp-accept-ready? server)
		    (not (unbox continue))) ;wait until we have an incoming connection
	  (block)))
      (when (unbox continue) ;;when we know we do, check if we -can- accept it, if not end
	(accept-thread-handle-continue server continue)
	(loop (add1 usern))))
    (tcp-close server))

  (provide begin-server outport))
