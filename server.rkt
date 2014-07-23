(module rMUD-server racket/base
  (require racket/tcp)
  (require racket/cmdline)

  (require "map.rkt")
  (require "user.rkt")
  (require "parsing.rkt")

  (define outport (make-parameter 12345)) ;make this set by tcp-listen
  (define hostname (make-parameter "localhost"))
  (define args (current-command-line-arguments))
  (when (= 2 (vector-length args))
    (outport (vector-ref args 0))
    (hostname (vector-ref args 1)))

  (define (close-connections in out)
    (close-input-port in)
    (close-output-port out))

  (define (connection-killed-close! user . rest)
    (if (or (user-kill user)
	    (unbox (user-global-kill user))) ;if the user is killed in any way
	(begin
	  (close-connections (user-in user) (user-out user)) ;close their connections
	  #f) ;and signal that apply-remove-userlist can remove it
	#t)) ;else say that it can't remove it

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

  (define env (make-environment
		(cons "quit" 
		      (lambda (env rest user str)
			(display "Bye\r\n" (user-out user)) 
			(flush-output (user-out user))
			(set-user-kill! user #t) ;set the user's killswitch to true
			#t))
		(cons "global-quit"
		      (lambda (env rest user str)
			(display "Server killed\n")
			(flush-output)
			(set-box! (user-global-kill user) #t)
			#t))
		(cons "name"
		      (lambda (env rest user str)
			(if (list-longer rest -1)
			    (set-user-name! user (car rest))
			    (begin
			      (fprintf (user-out user) 
				       "Enter a name after \"name\" next time\r\n")
			      (flush-output (user-out user))))
			#t))
		(cons "say"
		      (lambda (env rest user str)
			(printf "~a said: ~a\r\n" (user-name user) (substring str 4))
			(flush-output)
			(apply-userlist
			 (lambda (u x)
			   (unless (equal? (user-name u) (user-name user))
			     (fprintf (user-out u) "~a says: ~a\r\n" 
				      (user-name user) 
				      (substring str 4))
			     (flush-output (user-out u))))
			 (user-parent user))
			#t))
		(cons "users"
		      (lambda (env rest user str)
			(fprintf (user-out user) "~a Users\r\n"
				 (userlist-count (user-parent user)))
			(apply-userlist
			 (lambda (u x)
			   (fprintf (user-out user) "~a\r\n"
				    (user-name u)))
			 (user-parent user))
			(flush-output (user-out user))
			#t))
		(cons "log"
		      (lambda (env rest user str)
			(fprintf (user-out user) "log here:\r\n")
			(print-log user (user-out user))
			(flush-output (user-out user))
			#t))))
    
  (define (handle-string! user str)
    (if (equal? "" str)
	#f
	(begin
	  (add-log! user str) ;add string to user's log
	  (printf "~a executed: ~a\r\n" (user-name user) str) ;print user name and execution
	  (flush-output)
	  (if (parse-string str env user str) ;if the str relates to a verb in the environment
	      #t ;return true
	      (begin
		(fprintf (user-out user) "Say something sensible, man.\r\n")
		(flush-output (user-out user))
		#t)))))
  
  ;;gets input from the users in socket, then handles it
  (define (input-handle user . rest)
    (unless (or (user-kill user)
		(unbox (user-global-kill user)))
      (let ((str (read-all-chars-strip (user-in user)
				       (lambda (c)
					 (cond [(eq? c #\return) #f]
					       [(eq? c #\newline) #f]
					       [else #t])))))
	(handle-string! user str))))

  (define (accept-make-user server ulist)
    (define-values (s-in s-out) (tcp-accept server))
    (make-user ulist s-in s-out (userlist-global-kill ulist)))

  (define (accept-add-user! ulist server)
    (add-user! ulist (accept-make-user server ulist)))

  (define (close-all-connections ulist server)
    (apply-userlist (lambda (user . rest)
		      (close-connections (user-in user) (user-out user)))
		    ulist)
    (tcp-close server))

  (define (begin-server (port #f))
    ;;start listening on our specified port
    (define server (tcp-listen (if port
				   port
				   (if (string? (outport))
				       (string->number (outport))
				       (outport)))))

    (define global-kill (box #f))
    (define users (make-userlist global-kill))
    
    (let loop ()
      (unless (unbox global-kill)
	(when (tcp-accept-ready? server) ;if a connection is pending
	  (accept-add-user! users server)) ;add a user from that connection
	;;here we handle the users
	(apply-userlist input-handle users)
	(set-userlist-users! users (apply-remove-userlist connection-killed-close! users))
	(loop)))
    (close-all-connections users server))

  (begin-server (if (string? (outport))
		    (string->number (outport))
		    (outport)))

  (provide begin-server outport))
