;+====================================================================+
;|                                                                    |
;| Name:    test.lisp                                                 |
;|                                                                    |
;| Authors: 72904 - Luís Filipe Pookatham Ribeiro Gomes               |
;|          76119 - Ana Marta Correia Nunes                           |
;|                                                                    |
;| Description: Test psr.lisp                                         |
;|                                                                    |
;+====================================================================+

;+====================================================================+
;| Loading files                                                      |
;+====================================================================+

(load "exemplos.lisp")


(defun testing (file limit)
 (load file)
 (loop for n from 1 to limit do
 (format T "Test ~a~%" n)
	(let* ((in  (open (format nil "test~a~a/~a" (if (< n 10) 0 "") n "input" ) :if-does-not-exist nil))
				 (out (open (format nil "test~a~a/~a" (if (< n 10) 0 "") n "output") :if-does-not-exist nil))
				 (l (loop for line = (read-line in nil)
				 		  while line
								  if (and (plusp (length line)) (not (equalp #\; (char line 0))))
									  collect (read-from-string (format nil "~a" (multiple-value-list (eval (read-from-string line nil)))))
           and collect line))
     (u (remove-if-not (lambda (x) (stringp x)) l))
     (i (remove-if-not (lambda (x) (not (stringp x))) l))
				 (o (loop for line = (read-line out nil)
						   while line
								  if (and (plusp (length line)) (not (equalp #\; (char line 0))))
									  collect (read-from-string (format nil "(~a)" (remove #\; line)) nil))))
		(close in)
		(close out)
  (dotimes (m (length u))
   (if (not (equalp (nth m i) (nth m o)))
    (format T "~% -> Original Output : ~a~% -> Answered Output : ~{~a ~}~% -> Expected Output : ~{~a ~}~%~%" (nth m u) (nth m i) (nth m o)))))))

(testing "../psr_v1.lisp" 20)
(testing "../psr_v2.lisp" 20)