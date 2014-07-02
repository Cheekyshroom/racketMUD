(module rMUD-server racket/base
  (require racket/tcp)
  (require racket/port)
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

  (define (read-all-chars-blocking port)
    (let loop ()
      (if (char-ready? port)
	  (read-all-chars port)
	  (loop))))

  (define (begin-server)
    ;;start listening on our specified port
    (define server (tcp-listen (outport)))
    ;;accept an incoming connection on that port
    (define-values (s-in s-out) (tcp-accept server)) 

    (read-all-chars-blocking s-in)
    (flush-output)
    (display "Goodbye\n" s-out)
    (flush-output s-out)

    ;(let loop ((its 0))
      
     ; (loop (add1 its)))

    ;;close connections to client
    (close-connections s-in s-out)
    (tcp-close server))

  (provide begin-server outport hostname))
