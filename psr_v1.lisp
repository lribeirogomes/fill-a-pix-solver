;+---------------------------------------------------------------------+
;|                                                                     |
;| Name: psr.lisp                                                      |
;|                                                                     |
;| Authors: 72904 - Luis Filipe Pookatham Ribeiro Gomes                |
;|          76119 - Ana Marta Correia Nunes                            |
;|                                                                     |
;| Description: Fill-a-Pix Solver                                      |
;|                                                                     |
;+---------------------------------------------------------------------+


;+---------------------------------------------------------------------+
;| Load exemplos.fas file                                              |
;+---------------------------------------------------------------------+

(load "exemplos.fas")


;+---------------------------------------------------------------------+
;| Structures                                                          |
;+---------------------------------------------------------------------+


(defstruct psr
 (variaveis ())
 (dominios ())
 (restricoes ()))
 
 
(defstruct restricao
 (variaveis ())
 (funcao-validacao ()))


;+---------------------------------------------------------------------+
;| Auxiliar Functions                                                  |
;+---------------------------------------------------------------------+


(defmacro seta  (acc x)                               `(setf ,acc (append ,acc (cons ,x ()))))
(defmacro setr  (i j p q ii ji)                       `(if (not (or (= ,i ,p) (= ,j ,q))) (seta acc (format nil "~d-~d" ,ii ,ji))))
(defmacro sett  (i p ii ji)                           `(if (not (= ,i ,p))                (seta acc (format nil "~d-~d" ,ii ,ji))))
(defmacro setp  (psr var)                             `(progn (seta (psr-variaveis ,psr) ,var) (seta (psr-dominios ,psr) '(0 1))))
(defmacro bda   (f g)                                 `(function (lambda (x) (,f (,g x)))))
(defmacro findr (v)                                   `(lambda (x) (find ,v (restricao-variaveis x) :test 'equal)))
(defmacro doarray (i j mi mj ex l e &optional (n ())) `(dotimes (,i ,mi ,ex) (dotimes (,j ,mj) (let ,l (if elm ,e ,n)))))
(defmacro defif (f arg a b &optional (c ()))
 `(defun ,f ,arg
   (let ((elm (first lvr)))
    (cond
			((null lvr)            ,c)
			((equal var (car elm)) ,a)
			(t                     ,b)))))
(defmacro fres  ()
 `(lambda (psr) 
   (or (and (zerop (counter psr ran ()))
            (= elm (counter psr ran 1)))
       (and (plusp        (counter psr ran ()))
            (>= (- 9 elm) (counter psr ran 0))
            (>= elm       (counter psr ran 1))))))

;creates a list from a psr
(defun psr-list (lvr p f)
 (let (acc)
  (if lvr
   (dolist (elm lvr (if acc acc))
    (if (funcall p elm)
        (seta acc (funcall f elm))))
   ())))


(defun counter (psr lvr val)
 (let ((acc 0))
  (dolist (elm lvr acc)
   (if (equal val (psr-variavel-valor psr elm))
       (incf acc)))))


(defun conr (lvr lst dom val)
 (dolist (elm lvr dom)
  (let ((pos (position elm lst :test 'equal)))
   (if (or (equal 1 (length val))
       (null (nth pos dom)))
       (setf (nth pos dom) val)))))

;access an element from a list
(defif psr-find (lvr var lst)     (first lst)              (psr-find (rest lvr) var (rest lst)    ))
(defif psr-dom  (lvr var lst val) (setf (first lst) val  ) (psr-dom  (rest lvr) var (rest lst) val))
(defif psr-rem  (lvr var)         (setf (cdr elm)  (    )) (psr-rem  (rest lvr) var               ))
(defif psr-add  (lvr var val psr) (setf (cdr elm)   val  ) (psr-add  (rest lvr) var val psr        ) (setp psr (cons var val)))


(defun psr-pp (psr lvr acc)
 (cond ((null lvr)                                             (values t acc))
	     ((funcall (restricao-funcao-validacao (first lvr)) psr) (psr-pp psr (rest lvr) (1+ acc)))
	     (t                                                      (values () (1+ acc)))))

(defun psr-range (i j maxi maxj)
	(let (acc)
		(seta acc (format nil "~d-~d" i j))
		(setr i j 0    0    (1- i) (1- j))
		(setr i j 0    maxj (1- i) (1+ j))
		(setr i j maxi 0    (1+ i) (1- j))
		(setr i j maxi maxj (1+ i) (1+ j))
		(sett i 0           (1- i)  j)
		(sett j 0            i     (1- j))
		(sett i maxi        (1+ i)  j)
		(sett j maxj         i     (1+ j))
		acc))


;+---------------------------------------------------------------------+
;| Main Functions                                                      |
;+---------------------------------------------------------------------+

(defun psr-atribuicoes              (psr) (psr-list (psr-variaveis psr) (bda numberp cdr) 'identity))
(defun psr-variaveis-todas          (psr) (psr-list (psr-variaveis psr) (bda atom cdr)    'car))
(defun psr-variaveis-nao-atribuidas (psr) (psr-list (psr-variaveis psr) (bda null cdr)    'car))

(defun cria-psr (variaveis dominios restricoes)    (make-psr :variaveis (psr-list variaveis 'atom 'list) :dominios dominios :restricoes restricoes))
(defun cria-restricao (variaveis funcao-validacao) (make-restricao :variaveis variaveis :funcao-validacao funcao-validacao))

(defun psr-variavel-valor      (psr variavel) (cdr (psr-find (psr-variaveis psr) variavel (psr-variaveis psr))))
(defun psr-variavel-dominio    (psr variavel)      (psr-find (psr-variaveis psr) variavel (psr-dominios psr)))
(defun psr-variavel-restricoes (psr variavel)      (psr-list (psr-restricoes psr) (findr variavel) 'identity))

(defun psr-adiciona-atribuicao! (psr variavel valor) (psr-add (psr-variaveis psr) variavel valor psr))
(defun psr-remove-atribuicao!   (psr variavel)       (psr-rem (psr-variaveis psr) variavel))
(defun psr-altera-dominio!      (psr variavel valor) (psr-dom (psr-variaveis psr) variavel (psr-dominios psr) valor))

(defun psr-completo-p (psr)
 (if (null psr)
  t
  (null (car (psr-variaveis-nao-atribuidas psr)))))


(defun psr-consistente-p (psr)
 (if (null psr)
  (values () 0)
  (psr-pp psr (psr-restricoes psr) 0)))


(defun psr-variavel-consistente-p (psr variavel)
 (if (null psr)
  (values () 0)
  (psr-pp psr (psr-variavel-restricoes psr variavel) 0)))


(defun psr-atribuicao-consistente-p (psr variavel valor)
 (if (null psr)
  (values () 0)
  (let ((vai (psr-variavel-valor psr variavel)))
   (psr-adiciona-atribuicao! psr variavel valor)
   (multiple-value-bind (p n)
     (psr-pp psr (psr-variavel-restricoes psr variavel) 0)
    (psr-adiciona-atribuicao! psr variavel vai)
    (values p n)))))


(defun psr-atribuicoes-consistentes-arco-p (psr vara vala varb valb)
 (if (null psr)
  (values () 0)
  (let ((vaia (psr-variavel-valor psr vara))
       (vaib (psr-variavel-valor psr varb)))
   (psr-adiciona-atribuicao! psr vara vala)
   (psr-adiciona-atribuicao! psr varb valb)
   (multiple-value-bind (p n)
     (psr-pp psr (intersection (psr-variavel-restricoes psr vara) (psr-variavel-restricoes psr varb)) 0)
    (psr-adiciona-atribuicao! psr vara vaia)
    (psr-adiciona-atribuicao! psr varb vaib)
    (values p n)))))


(defun fill-a-pix->psr (arr)
	(if arr
		(let ((maxi (array-dimension arr 0)) (maxj (array-dimension arr 1)))
			(let ((var) (res))
				(doarray i j maxi maxj (cria-psr var (make-list (length var) :initial-element '(0 1)) res) ((elm (aref arr i j)) (ran (psr-range i j (1- maxi) (1- maxj))))
					(progn (setf var (remove-duplicates (append var ran) :test 'equal :from-end t))
					(seta res (cria-restricao ran (lambda (psr) (or (and (zerop (counter psr ran ())) (= elm (counter psr ran 1))) (and (plusp (counter psr ran ())) (>= (- 9 elm) (counter psr ran 0)) (>= elm (counter psr ran 1)))))))))))))
(defun psr->fill-a-pix (psr maxi maxj)
	(if psr 
		(let ((arr (make-array `(,maxi ,maxj))))
			(dolist (elm (psr-variaveis psr))
				(setf (aref arr (parse-integer (car elm) :junk-allowed t) (parse-integer (car elm) :start 2)) (cdr elm)))
			(doarray i j maxi maxj arr ((elm (aref arr i j)))
				() (setf (aref arr i j) 0)))))

(defun retrocesso-simples (psr acc)
 (if (psr-completo-p psr)
  (values psr acc)
  (let ((var (first (psr-variaveis-nao-atribuidas psr))))
   (dolist (val (psr-variavel-dominio psr var) (values () acc))
    (multiple-value-bind (p n)
      (psr-atribuicao-consistente-p psr var val)
     (setf acc (+ acc n))
     (if p
        (progn (psr-adiciona-atribuicao! psr var val)
               (multiple-value-bind (r m)
                 (retrocesso-simples psr acc)
                (setf acc m)
                (multiple-value-bind (s o)
                  (psr-consistente-p r)
                 (setf acc (+ acc o))
                 (if s
                  (return (values r m))
                  (progn (psr-remove-atribuicao! psr var)
                         (setf acc m))))))))))))


(defun procura-retrocesso-simples (psr)
	(if psr
		 (retrocesso-simples psr 0)
		 (values () 0)))


(defun resolve-simples (arr)
 (multiple-value-bind (psr)
   (procura-retrocesso-simples (fill-a-pix->psr arr))
  (psr->fill-a-pix psr (array-dimension arr 0) (array-dimension arr 1))))
