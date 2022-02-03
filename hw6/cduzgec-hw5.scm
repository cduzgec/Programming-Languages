;symbol-length
;sequence?
;same-sequence?
;reverse-sequence
;palindrome?
;member?
;remove-member
;anagram?
;anapoli?

(define symbol-length
	(lambda (inSym)
		(if (symbol? inSym)
			(string-length (symbol->string inSym))
			0
		)
	)
)

(define sequence?
	(lambda (inSeq)
	(cond
	  ((null? inSeq) #t )
		((not(list? inSeq)) #f )
		((not(symbol? (car inSeq)))  #f )
		((not(eq? (symbol-length(car inSeq)) 1 )) #f )
		((not(sequence? (cdr inSeq))) #f )
		(else #t)
	))
)

(define same-sequence?
	(lambda (inSeq1 inSeq2)
	(cond
		((and (null? inSeq1) (null? inSeq2)) #t)
		((not(sequence? inSeq1)) (error "this is not a sequence" inSeq1))
		((not(sequence? inSeq2)) (error "this is not a sequence" inSeq2))
		((not(eq? (length inSeq1) (length inSeq2))) #f)
		((not(eq? (car inSeq1) (car inSeq2))) #f)                     ;		((not(equal? inSeq1 inSeq2)) #f) - same thing without recursion
		((not(same-sequence? (cdr inSeq1) (cdr inSeq2)))	#f)
		(else #t)
	))
)

(define (reverse lst)
  (reverse-helper lst '()))

(define (reverse-helper lst acc)                                          
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define reverse-sequence
	(lambda (inSeq)
	(cond
		((null? inSeq) '())
		((not(sequence? inSeq)) (error "this is not a sequence" inSeq))
    (else (reverse inSeq))
	))
)

(define palindrome?
  (lambda (inSeq)
  (cond
        ((not(sequence? inSeq)) (error "this is not a sequence: " inSeq))
        ((not(same-sequence? inSeq (reverse-sequence inSeq)))#f )        
        (else #t)    
  ))
)

(define element?
  (lambda (x lst)
  (cond 
    ((null? lst) #f) 
    ((eq? x (car lst)) #t) 
    (else (element? x (cdr lst)))
  ))
)
    
(define member?
  (lambda (inSym inSeq)
  (cond
    ((not(symbol? inSym)) (error "this is not a symbol: " inSym))
    ((not(sequence? inSeq )) (error "this is not a sequence: " inSeq))
    (else (element? inSym inSeq))
  ))
)

(define (remove x lst)
  (cond 
    ((null? lst) '())
    ((equal? (car lst) x) (cdr lst))
    (else (cons (car lst) (remove x (cdr lst))))
  )
)
      
(define remove-member
  (lambda (inSym inSeq)
  (cond
    ((not(symbol? inSym)) (error "this is not a symbol: " inSym))
    ((not(sequence? inSeq )) (error "this is not a sequence: " inSeq))
    ((not(member? inSym inSeq))(error "symbol is not in the sequence" ))
    (else (remove inSym inSeq))
  ))
)

(define (anagram-check lst1 lst2)
  (cond 
    ((null? lst1) #t)
    ((element? (car lst1) lst2) (anagram-check (cdr lst1)(remove (car lst1) lst2)))                                                                          (else #f)
  )
) 

(define anagram?
  (lambda (inSeq1 inSeq2)
  (cond
		((and (null? inSeq1) (null? inSeq2)) #t)
		((not(sequence? inSeq1)) (error "this is not a sequence" inSeq1))
		((not(sequence? inSeq2)) (error "this is not a sequence" inSeq2))
    ((not(eq? (length inSeq1) (length inSeq2))) #f )
    ((anagram-check inSeq1 inSeq2) #t)
    (else #f)
  ))
)

(define anapoli?
  (lambda (inSeq1 inseq2)
  (cond
		((not(sequence? inSeq1)) (error "this is not a sequence" inSeq1))
		((not(sequence? inSeq2)) (error "this is not a sequence" inSeq2))
    ((not(palindrome? inSeq2)) #f )     
    ((not(anagram? inSeq1 (reverse-sequence inSeq2))) #f )
    (else #t)
  ))
)








