
;if, cond, let, let*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      PREDICATES     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define define-stmt? (lambda (e)
    (and (list? e) (= (length e) 3) (equal? (car e) 'define) (symbol? (cadr e)))))

(define if-stmt? (lambda (e)
	(and (list? e) (= (length e) 4) (equal? (car e) 'if))))
	
(define cond-stmt? (lambda (e)
	(and (list? e) (equal? (car e) 'cond) (conditional-list? (cdr e)))))

(define conditional-list? (lambda (e)
    (cond 
		((not(list? e)) (display "ERROR cs 305 not a conditional-list "))
		((null? e) (display "ERROR cs 305 not a conditional-list " ))
		((not(conditional? (car e))) (display " cs 305 not a conditional " ))
		((else-condition? (cdr e)) #t)	
		(else (conditional-list? (cdr e)))))) ; en sonda else olmalı

(define conditional? (lambda (e)
    (and (list? e) (= (length e) 2))))

(define else-condition? (lambda (e)     ; working fine
    (and (list? e) (= (length e) 2) (equal? (car e) 'else))))

(define let-stmt? (lambda (e)
	(and (list? e) (= (length e) 3) (equal? (car e) 'let) (var-list? (cadr e)))))

(define letstar-stmt? (lambda (e)
	(and (list? e) (= (length e) 3) (equal? (car e) 'let*) (var-list? (cadr e)))))

(define var-list? (lambda (e)
	(cond 
	((null? e) #t)
	((not(list? e)) #f)
	((not(symbol? (caar e))) #f) 
	(else (var-list? (cdr e))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      HELPER FUNCTIONS     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-operator (lambda (op-symbol) 
  (cond 
    ((equal? op-symbol '+) +)
    ((equal? op-symbol '-) -)
    ((equal? op-symbol '*) *)
    ((equal? op-symbol '/) /)
    (else (display "ERROR cs 305 s7-interpret: operator not implemented")))))
	  
(define if-operator (lambda (e env)
	(if (not(equal? (s7-interpret (cadr e) env) 0)) (s7-interpret (caddr e) env) (s7-interpret (cadddr e) env))))
	
(define cond-operator(lambda (e env)
	(cond-helper (cdr e) env)))
	
(define cond-helper (lambda (e env)
	(if (else-condition? (car e)) 
		(s7-interpret (cadar e) env)
		(if (equal? (s7-interpret (caar e) env) 0) (cond-helper (cdr e) env) (s7-interpret (cadar e) env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         S-7 EXPR         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s7-interpret (lambda (e env)
  (cond 
    ((number? e) e)
    ((symbol? e) (get-value e env))
    ((not (list? e)) (display "ERROR cs 305 s7-interpret: cannot evaluate "))
	((if-stmt? e) (if-operator e env))
	((cond-stmt? e) (cond-operator e env))
	((let-stmt? e)
      (let ((names (map car  (cadr e))) (inits (map cadr (cadr e))))
      (let ((vals (map (lambda (init) (s7-interpret init env)) inits)))
	  (let ((new-env (append (map cons names vals) env))) (s7-interpret (caddr e) new-env)))))
	
	((letstar-stmt? e) 
	
	(if (= (length (cadr e)) 1) 
		(let ((let-arg (list 'let (cadr e) (caddr e))))    ; same as let
			(let ((names (map car (cadr let-arg))) (inits (map cadr (cadr let-arg)))) 
			(let ((vals (map (lambda (init) (s7-interpret init env)) inits)))
			(let ((new-env (append (map cons names vals) env))) (s7-interpret (caddr let-arg) new-env)))))
																				
		(let ((first (list 'let (list (caadr e)))) (rest (list 'let* (cdadr e) (caddr e)))) ;not a list
			(let ((let-arg (append first (list rest)))) 
			(let ((names (map car (cadr let-arg))) (inits (map cadr (cadr let-arg))))
			(let ((vals (map (lambda (init) (s7-interpret init env)) inits)))
			(let ((new-env (append (map cons names vals) env)))
					(s7-interpret (caddr let-arg) new-env))))))
	))
																		
				
	
    ;; First evaluate the value of the operands
    ;; and the procedure of the expression.
    (else 
       (let ((operands (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
             (operator (get-operator (car e))))

         ;; And finally apply the operator to the 
         ;; values of the operands
         (apply operator operands))))))
		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      ENVIRONMENT     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-value (lambda (var env)
    (cond
      ;; If the environment is empty, then we could not find 
      ;; a binding in this environment.
      ((null? env) (display "ERROR cs 305 s7-interpret: unbound variable"))

      ;; Check if the first pair is a binding for the
      ;; variable that we are looking for, and if it is
      ;; return the current value of the variable.
      ((equal? (caar env) var) (cdar env))

      ;; Otherwise, search in the remaning of the environment.
      (else (get-value var (cdr env))))))

(define extend-env (lambda (var val old-env)
      ;; Add the new variable binding to the 
      ;; beginning of the old environment.
      (cons (cons var val) old-env)))		 
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         REPL         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define repl (lambda (env)
  (let* (
         ; first print out some prompt
         (dummy1 (display "cs305> "))

         ; READ an expression
         (expr (read))

         ; Form the new environment
         (new-env (if (define-stmt? expr)
                      (extend-env (cadr expr) (s7-interpret (caddr expr) env) env)
                      env))

         ; EVALuate the expression read
         (val (if (define-stmt? expr)
                  (cadr expr)
                  (s7-interpret expr env)))

         ; PRINT the value evaluated together
         ; with a prompt as MIT interpreter does
         (dummy2 (display "cs305: "))
         (dummy3 (display val))

         ; get ready for the next prompt
         (dummy4 (newline))
         (dummy4 (newline)))
     (repl new-env))))


(define cs305 (lambda () (repl '())))
