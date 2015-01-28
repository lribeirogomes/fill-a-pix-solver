;+====================================================================+
;|                                                                    |
;| Name: psr.lisp                                                     |
;|                                                                    |
;| Authors: 72904 - Luis Filipe Pookatham Ribeiro Gomes               |
;|          76119 - Ana Marta Correia Nunes                           |
;|                                                                    |
;| Description: Fill-a-Pix Solver                                     |
;|                                                                    |
;+====================================================================+


;+====================================================================+
;| Files                                                              |
;+====================================================================+


(load "exemplos.fas" :if-does-not-exist nil)

;+====================================================================+
;| Structs                                                            |
;+====================================================================+


(defstruct psr
 (variaveis nil)
 (dominios nil)
 (restricoes nil))
 
 
(defstruct restricao
 (variaveis nil)
 (funcao-validacao nil)
 (ocupados 0))

;+====================================================================+
;| Macros                                                             |
;+====================================================================+

(defmacro rotate-left (lista elemento)
	`(if ,lista
			(rplacd (last ,lista) (list ,elemento))
			(setf ,lista (list ,elemento))))
(defmacro composite (f g) `(function (lambda (x) (,f (,g x)))))
(defmacro variavelp ()    `(lambda (n) (equal variavel (car n))))
(defmacro nextf (valor extremo)
	`(lambda (psr)
		(let* (
			(variaveisNaoAtribuidas (psr-variaveis-nao-atribuidas psr))
			(tamanho (lambda (variavel) (length ,valor)))
			(valores (mapcar tamanho variaveisNaoAtribuidas))
			(maxRestricoes (apply ,extremo valores))
			(maxp (lambda (elemento) (= elemento maxRestricoes))))

			(nth (position-if maxp valores) variaveisNaoAtribuidas))))

;+====================================================================+
;| List Functions                                                     |
;+====================================================================+


(defun psr-atribuicoes (psr)
 (remove-if-not (composite numberp cdr) (psr-variaveis psr)))


(defun psr-variaveis-todas (psr)
 (mapcar 'car (psr-variaveis psr)))


(defun psr-variaveis-nao-atribuidas (psr)
 (mapcar 'car (remove-if-not (composite null cdr) (psr-variaveis psr))))

;+====================================================================+
;| Constructor Functions                                              |
;+====================================================================+


(defun cria-psr (variaveis dominios restricoes)
 (make-psr :variaveis (mapcar 'list variaveis)
           :dominios dominios
           :restricoes restricoes))


(defun cria-restricao (variaveis funcao-validacao)
 (make-restricao :variaveis variaveis
                 :funcao-validacao funcao-validacao))

;+====================================================================+
;| Get/Set Functions                                                  |
;+====================================================================+


(defun psr-variavel-valor      (psr variavel)
 (cdr (find-if (variavelp) (psr-variaveis psr))))


(defun psr-variavel-dominio    (psr variavel)
 (nth (position-if (variavelp) (psr-variaveis psr))
      (psr-dominios psr)))


(defun psr-variavel-restricoes (psr variavel)      
 (remove-if-not (lambda (x)
                 (find variavel (restricao-variaveis x) :test 'equal))
                (psr-restricoes psr)))


(defun psr-adiciona-atribuicao! (psr variavel valor)
 (if (member-if (variavelp) (psr-variaveis psr))
	(nsubstitute-if (cons variavel valor) (variavelp) (psr-variaveis psr))
	(progn (nconc (psr-variaveis psr) (cons variavel valor))
	       (nconc (psr-dominios psr)  '(0 1)))))


(defun psr-remove-atribuicao! (psr variavel)
 (nsubstitute-if (cons variavel nil) (variavelp) (psr-variaveis psr)))


(defun psr-altera-dominio! (psr variavel valor)
 (setf (nth (position-if (variavelp) (psr-variaveis psr))
            (psr-dominios psr))
        valor))

;+====================================================================+
;| Conditional Functions                                              |
;+====================================================================+


(defun psr-completo-p (psr)
 (null (car (psr-variaveis-nao-atribuidas psr))))


(defmacro psr-pp (psr lvr)
 `(let ((totalTestes 0))
   (dolist (elm ,lvr (values t totalTestes))
    (incf totalTestes)
    (if (not (funcall (restricao-funcao-validacao elm) ,psr))
     (return (values nil totalTestes))))))


(defun psr-consistente-p (psr)
 (psr-pp psr (psr-restricoes psr)))


(defun psr-variavel-consistente-p (psr variavel)
 (psr-pp psr (psr-variavel-restricoes psr variavel)))


(defun psr-atribuicao-consistente-p (psr variavel valor)
 (let ((vai (psr-variavel-valor psr variavel)))
  (psr-adiciona-atribuicao! psr variavel valor)
  (multiple-value-bind (p n)
    (psr-pp psr (psr-variavel-restricoes psr variavel))
   (psr-adiciona-atribuicao! psr variavel vai)
   (values p n))))


(defun psr-atribuicoes-consistentes-arco-p (psr variavel1 valor1 variavel2 valor2)
 (let ((valorInicial1 (psr-variavel-valor psr variavel1))
      (valorInicial2 (psr-variavel-valor psr variavel2)))
  (psr-adiciona-atribuicao! psr variavel1 valor1)
  (psr-adiciona-atribuicao! psr variavel2 valor2)
  (multiple-value-bind (p n)
    (psr-pp psr (intersection (psr-variavel-restricoes psr variavel1)
                              (psr-variavel-restricoes psr variavel2)))
   (psr-adiciona-atribuicao! psr variavel1 valorInicial1)
   (psr-adiciona-atribuicao! psr variavel2 valorInicial2)
   (values p n))))

;+====================================================================+
;| Parser Functions                                                   |
;+====================================================================+


(defun fill-a-pix->psr (arr &optional modo)
 (if arr
  (let* ((numeroLinhas (array-dimension arr 0))
         (numeroColunas (array-dimension arr 1))
         (numeroPixeis (* numeroLinhas numeroColunas))
         (variavel  (loop for n from 0 to (1- numeroPixeis)
                     for totalTestes = (write-to-string n)
                      collect totalTestes))
         (dom  (make-list numeroPixeis :initial-element '(0 1)))
         (res))
   (dotimes (i numeroLinhas (cria-psr variavel dom res))
    (dotimes (j numeroColunas)
     (let ((elm (aref arr i j)))
      (if elm
       (let* ((totalTestes (list (nth (+ (* numeroLinhas i) j) variavel)))
              (o (- numeroLinhas 1))
              (p (- numeroColunas 1))
              (ran (macrolet ((setr  (p q a b)
                              `(if (not (or (= i ,p)
                                            (= j ,q)))
                                (nconc totalTestes (list (nth (+ (* numeroLinhas ,a) ,b) variavel)))))
                              (sett  (i p a b)
                              `(if (not (= ,i ,p))
                                (nconc totalTestes (list (nth (+ (* numeroLinhas ,a) ,b) variavel))))))
                    (setr 0 0 (1- i) (1- j))
                    (sett i 0 (1- i)  j    )
                    (setr 0 p (1- i) (1+ j))
                    (sett j 0  i     (1- j))
                    (sett j p  i     (1+ j))
                    (setr o 0 (1+ i) (1- j))
                    (sett i o (1+ i)  j    )
                    (setr o p (1+ i) (1+ j))
                    totalTestes)))
                 (if modo
                  (cond ((not modo)                      (rotate-left dom (make-list (length ran) :initial-element '(0 1))))
                        ((and modo (= elm (length ran))) (rotate-left variavel (make-list (length ran) :initial-element '(0 1))))
                        ((and modo (zerop elm))          (rotate-left variavel (make-list (length ran) :initial-element '(0 1))))))
				 (rotate-left res (cria-restricao ran (lambda (psr) (let ((n) (O 0) (l 0))
					             (dolist (x ran (or (and (not n)
						             (= elm l))
						              (and  n
								             (>= (- (length ran) elm) O)
									             (>= elm l))))
								              (let ((val (psr-variavel-valor psr x)))
								             (cond ((null val) (setf n t))
								             ((= 0  val) (incf O))
								             ((= 1  val) (incf l)))))))))))))))))


(defun psr->fill-a-pix (psr numeroLinhas numeroColunas)
 (if psr 
  (let ((arr (make-array (list numeroLinhas numeroColunas)))
        (lst (mapcar 'cdr (psr-variaveis psr))))
   (dotimes (i numeroLinhas arr)
    (dotimes (j numeroColunas)
     (setf (aref arr i j) (first lst))
     (setf lst (rest lst)))))))

;+====================================================================+
;| Search Auxiliar Functions                                          |
;+====================================================================+


;;Pseudo-codigo:
;;function revise(psr,x,y,inferencias)
(defun revise (psr x y inference)
	(let* (

;;		Nota:
;;		funcao auxiliar busca
;;		variavel auxiliar variavel
		(busca       (lambda (w) (if (and inference (member-if (lambda (n) (equal x (cdr n))) inference))
		                          (cdr (find-if (lambda (n) (equal w (car n))) inference))
		                          (psr-variavel-dominio psr w))))
		(variavel    (psr-variavel-valor psr y))

;;		Pseudo-codigo:
;;		testesTotais = 0
;;		revised = False
;;		dominio-x = buscar dominio da var ao cj. de inferencias (se existir), ou ao psr
;;		novo-dominio-x = dominio-x
;;		if y esta atribuida
;;			dominio-y = {valor atribuido a y}
;;		else
;;			dominio-y = buscar dominio da var ao cj. de inf. (se existir), ou ao psr
		(totalTestes  0)
		(revised)
		(dominioX    (funcall busca x))
		(novoDominio  dominioX)
		(dominioY    (if variavel
		               variavel
		              (funcall busca y))))

;;		Nota:
;;		para meter os valores isolados em lista (1 -> (1), p.e.)
		(setf dominioX (if (listp dominioX) dominioX (list dominioX)))
		(setf dominioY (if (listp dominioY) dominioY (list dominioY)))

;;		Pseudo-codigo:
;;		foreach valor vx in dominio-x
;;			foundConsistentValue = False
;;			foreach valor vy in dominio-y
;;				consistente,testes = psr-atribuicoes-consistentes-arco-p(psr,x,vx,y,vy)
;;				testesTotais incf testes
;;				if consistente
;;					foundConsistentValue = True
;;					break from closest loop
;;			if foundConsistentValue == False
;;				revised = True
;;				remover vx de novo-dominio-x
		(dolist (vx dominioX)
			(setf foundConsistentValue nil)
			(dolist (vy dominioY) ;;Nota (block): para sair do loop
				(multiple-value-bind (consistente testes) (psr-atribuicoes-consistentes-arco-p psr x vx y vy)
				(incf totalTestes testes)
				(if consistente
					(progn (setf foundConsistentValue t)
					(return)))))
			(if (not foundConsistentValue)
				(progn (setf revised t)
				(delete vx novoDominio))))

;;		Pseudo-codigo:
;;		if revised
;;			adicionar/actualizar novo-dominio-x no conjunto de inferencias
		(if revised
			(if inference
				(progn (delete-if (lambda (n) (equal x (first n))) inference)
				(rotate-left inference (append (list x) (remove x novoDominio))))
			(setf inference (list (append (list x) (remove x novoDominio))))))

;;		Pseudo-codigo:
;;		return revised,testesTotais
		;(print inference)
		(values revised totalTestes inference)))

;;Pseudo-codigo:
;;function arcos-vizinhos-nao-atribuidos(psr,var)
(defun arcos-vizinhos-nao-atribuidos (psr variavel)
	(let (

;;		Pseudo-codigo:
;;		lista-arcos = {}
		(listaArcos))

;;		Pseudo-codigo:
;;		foreach var-natribuida in psr-variaveis-nao-atribuidas(psr)
;;			if var != var-natribuida
;;				if var-natribuida esta envolvida numa restricao com var
;;					inserir arco (var-natribuida,var) no fim da lista
		(dolist (variavelNaoAtribuida (psr-variaveis-nao-atribuidas psr))
			(if (not (equal variavel variavelNaoAtribuida))
				(if (intersection (psr-variavel-restricoes psr variavel) (psr-variavel-restricoes psr variavelNaoAtribuida))
					(rotate-left listaArcos (cons variavelNaoAtribuida variavel)))))

;;		Pseudo-codigo:
;;		return lista-arcos
		listaArcos))

;;Pseudo-codigo:
;;function forward-checking(psr,var)
;;Nota:
;;torna-se na function MAC(psr,var) se mac == t, visto a funcao ser praticamente a mesma tirando duas linhas de codigo
(defun fc (psr variavel &optional mac)
	(let (

;;		Pseudo-codigo:
;;		testesTotais = 0
;;		inferencias = {}
;;		lista-arcos = arcos-vizinhos-nao-atribuidos(psr,var)
		(totalTestes 0)
		(inference)
		(listaArcos (arcos-vizinhos-nao-atribuidos psr variavel)))

;;		Pseudo-codigo:
;;		foreach arco (v2,v1) in lista-arcos
		(dolist (arco listaArcos)
			(let (

;;				Nota: variaveis auxiliares variavel2 e variavel1
				(variavel2 (first arco))
				(variavel1 (rest arco)))

;;				Pseudo-codigo:
;;				revise,testes = revise(psr,v2,v1,inferencias)
;;				testesTotais incf testes
;;				if revise
;;					if size(dominio(inferencias,v2)) == 0
;;						return False,testesTotais
				(multiple-value-bind (inferencias testes inf) (revise psr variavel2 variavel1 inference)
				(setf inference inf)
				(incf totalTestes testes)
				(if inferencias
					(progn (if (zerop (length (find-if (lambda (n) (every (lambda (m) (equal variavel2 m)) (rest n))) inference)))
						(values nil totalTestes))

;;						Nota (mac):
;;						As funcoes dentro do if mac sao usadas no function MAC(psr,var)
						(if mac

;;							Pseudo-codigo (mac):
;;							novos-arcos = arcos-vizinhos-nao-atribuidos(psr,v2)
;;							remover (v1,v2) de novos-arcos
;;							adicionar novos-arcos a lista-arcos
							(append  listaArcos
							        (remove (cons variavel1 variavel2)
							                (arcos-vizinhos-nao-atribuidos psr variavel2)))))))))

;;		Pseudo-codigo:
;;		return inferencias,testesTotais
		(values inference totalTestes)))

;;Pseudo-codigo:
;;function MAC(psr,var)
;;Nota:
;;presente no defun fc
(defun mac (psr variavel) (fc psr variavel t))

;;function procura-retrocesso-simples(psr)
;;function procura-retrocesso-grau(psr)
;;function procura-retrocesso-fc-mrv(psr)
;;function procura-retrocesso-mac-mrv(psr)
(defun retrocesso (next psr &optional (mac (lambda (x y) (values nil 0))) modo)
	(block nil
		(let (

;;			Pseudo-codigo:
;;			testesTotais = 0
			(totalTestes 0))

;;			Pseudo-codigo:
;;			if completo(psr)
;;				return psr,testesTotais
			(if (psr-completo-p psr)
				(return (values psr totalTestes)))
			(let (

;;				Pseudo-codigo:
;;				var = primeira(variaveis-nao-atribuidas(psr)) (procura-retrocesso-simples)
;;				var = grau(psr)                               (procura-retrocesso-grau)
;;				var = MRV(psr)                                (procura-retrocesso-fc-mrv procura-retrocesso-mac-mrv)
				(var (funcall next psr)))

				(if (and modo (cdr var))
					(progn (psr-atribuicao-consistente-p psr var 1)
					(return (retrocesso next psr mac modo))))

;;				Pseudo-codigo:
;;				foreach valor in dominio(psr,var)
;;						consistente,testes = psr-atribuicao-consistente-p(psr,var,valor)
;;						testesTotais incf testes
;;						if consistente
;;							adiciona var=valor ao psr
				(dolist (valor (psr-variavel-dominio psr var) (values nil totalTestes)) ;Pseudo-codigo: return NIL,testesTotais
					(let ((sav (psr-dominios psr)))
						(multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p psr var valor)
						(incf totalTestes testes)
						(if consistente
							(progn (psr-adiciona-atribuicao! psr var valor)

;;							Pseudo-codigo (procura-retrocesso-fc-mrv procura-retrocesso-mac-mrv):
;;							inferencias,testes = forward-checking(psr,var,valor)
;;							testesTotais incf testes
;;							if inferencias
;;								adiciona inferencias ao psr
							(multiple-value-bind (inferencias testes) (funcall mac psr var)
							(incf totalTestes testes)
							(if inferencias
								(mapcar (lambda (inf) (psr-altera-dominio! psr (first inf) (rest inf))) inferencias))

;;							Pseudo-codigo :
;;							resultado,testes = procura-retrocesso-fc-mrv(psr)
;;							testesTotais incf testes
;;							if resultado return resultado,testesTotais
;;								remove inferencias do psr
							(multiple-value-bind (resultado testes) (retrocesso next psr mac modo)
							(incf totalTestes testes)
							(if  resultado
								(return (values resultado totalTestes))))))))

;;						Pseudo-codigo:
;;						remove { var = value } and inferences from assignment
						(setf (psr-dominios psr) sav)
						(psr-remove-atribuicao! psr var)))))))

;+====================================================================+
;| Search Functions                                                   |
;+====================================================================+

;;As varias funcoes procura utilizam a mesma funcao retrocesso, com as seguintes diferencas:

;; - procura-retrocesso-simples: a variavel var escolhida e a primeira das variaveis nao atribuidas;
;;                               nao usa as funcoes de inferencia (limita-se a retornar nil 0);
(defun procura-retrocesso-simples (psr)
	(retrocesso (lambda (x) (first (psr-variaveis-nao-atribuidas x))) psr))

;; - procura-retrocesso-grau: a variavel var escolhida e a primeira das variaveis com o maximo numero de restricoes nao preenchidas;
;;                            nao usa as funcoes de inferencia (limita-se a retornar nil 0);
(defun procura-retrocesso-grau (psr)
	(retrocesso (nextf (remove-if (lambda (restricao)  (>= (apply '+ (mapcar (lambda (preenchido) (if (psr-variavel-valor psr preenchido) 1 0)) (restricao-variaveis restricao)))
		                                      (1- (length (restricao-variaveis restricao))))) (psr-variavel-restricoes psr variavel)) 'max) psr))

;; - procura-retrocesso-fc-mrv: a variavel var escolhida e a primeira das variaveis com o tamanho minimo do dominio;
;;                              usa a funcao de inferencia forward-checking(psr, var);
(defun procura-retrocesso-fc-mrv (psr &optional (fc 'fc))
	(retrocesso (nextf (psr-variavel-dominio psr variavel) 'min) psr fc))

;; - procura-retrocesso-mac-mrv: a variavel var escolhida e a primeira das variaveis com o tamanho minimo do dominio;
;;                               usa a funcao de inferencia MAC(psr, var);
(defun procura-retrocesso-mac-mrv (psr)
	(procura-retrocesso-fc-mrv psr 'mac))

;;function resolve-simples(array)
;;Traduz tabela fill-a-pix para psr,
;;resolve o psr com procura-retrocesso-simples
;;e traduz de volta para tabela fill-a-pix preenchida
(defun resolve-simples (array)
	(if array
		(psr->fill-a-pix (procura-retrocesso-simples (fill-a-pix->psr array)) (array-dimension array 0) (array-dimension array 1))))

;;function resolve-simples(array)
;;Traduz tabela fill-a-pix (que, neste caso, evita fazer restricoes desnecessarias e preenche ) para psr,
;;resolve o psr com procura optimizada
;;e traduz de volta para tabela fill-a-pix preenchida
(defun resolve-best (array)
	(if array
		(psr->fill-a-pix (retrocesso (next psr-variavel-dominio min) (fill-a-pix->psr array t) nil t) (array-dimension array 0) (array-dimension array 1))))
