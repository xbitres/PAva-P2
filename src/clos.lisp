;;; Grupo 17 : Guilherme Ramos - 77916 , Goncalo Marques - 78016 , 86318	Aquilino Silva

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 				Grupo 18 						  	                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 						                         	                ;;;
;;; HashTable Auxiliar e funcoes auxiliares  			        ;;;
;;;														                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; HashTable que servira para guardar todas as infromacoes relevantes acerca das classes
(defparameter classInfo (make-hash-table))


;;;	register-class-info: nome atributos offsets superCLasses
;;;	Esta funcao regista na tablea o nome, atributos, offsets e superClasses
(defun register-class-info (nome atributes offsets superClasses)
  (setf (gethash nome classInfo) (make-hash-table))
  (setf (gethash 'atributes (gethash nome classInfo)) atributes)
  (setf (gethash 'offsets (gethash nome classInfo)) offsets)
  (setf (gethash nome (gethash 'offsets (gethash nome classInfo))) 0)
  (setf (gethash 'superclasses (gethash nome classInfo)) superClasses)
  (setf (gethash 'subclasses (gethash nome classInfo)) '())
  (mapcar #'(lambda (class)
    (setf (gethash 'subclasses (gethash class classInfo)) (append (list nome) (gethash 'subclasses (gethash class classInfo)) ))
  )
  superClasses)
)


(defun calculate-attributes (atributes classes)
  (let ((atri '()))
    (mapcar #'(lambda (class)
        (setf atri (append atri (gethash 'atributes (gethash class classInfo)))))
    classes)
    (append atri atributes)
  )
)

;;; get-class-subclasses: class -> lista de subclasses
;;; Esta funcao recebe uma classe, e atraves da informacao na hashtable,
;;; retorna uma lista com todas as subclasses da classe recebida
(defun get-class-subclasses (class)
  (let ((subclassesToExplore (gethash 'subclasses (gethash class classInfo)))
        (subclasses '()))

    (loop while (not (eq nil subclassesToExplore))
      do
      (setf subclasses (append subclasses (list (car subclassesToExplore))))
      (setf subclassesToExplore (cdr subclassesToExplore))
    )
    subclasses
  )
)

(defun inherit-offsets (nomeClass superClasses)
  (mapcar #'(lambda (class) ; Transverse superClasses to be inherited
    (maphash #'(lambda (generalClass value) ; Transverse all classes in classInfo
      (if (gethash class (gethash 'offsets value))
        (setf (gethash nomeclass (gethash 'offsets value)) (gethash class (gethash 'offsets value)))
      )
    ) classInfo)
  ) superClasses)
)

(defun calculate-offsets (nomeClass classes)
  (inherit-offsets nomeClass classes)
  (let (
      (offset 0)
     )
    (mapcar #'(lambda (class)
      (setf (gethash nomeClass (gethash 'offsets (gethash class classInfo))) offset)
      (setf offset (+ (list-length (gethash 'atributes (gethash class classInfo))) offset)))
    classes)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 						                         	  ;;;
;;;	Symbol Constructors  				                  ;;;
;;;														  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-symbol-constructor (nome)
  (intern (concatenate 'string "MAKE-" (symbol-name nome)))
)

(defun make-symbol-getters (nome-classe parametro)
  (intern (concatenate 'string (symbol-name nome-classe) "-" (symbol-name parametro)))
)

(defun make-symbol-setters (nome-classe parametro)
  (intern (concatenate 'string (symbol-name nome-classe) "-" (symbol-name parametro) "!"))
)

(defun make-symbol-recognizer (nome)
  (intern (concatenate 'string (symbol-name nome) "?"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 						                         	  ;;;
;;;	Functions generators  				                  ;;;
;;;														  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-constructor (nome arguments)
  `(defun ,(make-symbol-constructor nome) (&key ,@arguments)
    (vector ',nome ,@arguments)
  )
)

(defun generate-getters (nome atributes)
  (let (
        (functions '())
        (parm-i 1)
       )
    (mapcar #'(lambda (atri)
      (setf functions (append functions (list
        `(defun ,(make-symbol-getters nome atri) (class)
          (let ((offset (gethash (aref class 0) (gethash 'offsets (gethash ',nome classInfo)))))
            (if (not (eq nil offset))
              (aref class (+ ,parm-i offset))
            )
          )
         )
        )))
      (setf parm-i (+ parm-i 1))
    ) atributes)
    functions
  )
)

(defun generate-setters (nome atributes)
  (let (
        (functions '())
        (parm-i 1)
       )
    (mapcar #'(lambda (atri)
      (setf functions (append functions (list
        `(defun ,(make-symbol-setters nome atri) (class param)
          (let ((offset (gethash (aref class 0) (gethash 'offsets (gethash ',nome classInfo)))))
            (if (not (eq nil offset))
              (setf (aref class (+ ,parm-i offset)) param)
            )
          )
         )
        )))
      (setf parm-i (+ parm-i 1))
    ) atributes)
    functions
  )
)

(defun generate-recognizer (nome)
  `(defun ,(make-symbol-recognizer nome) (value)
    (cond ((and (typep value 'vector) (>= (length value) 1) (eq (aref value 0) ',nome)) t)
          (t (let ((subclassesToCall (mapcar #'(lambda (class) (make-symbol-recognizer class)) (get-class-subclasses ',nome)))
                   (res nil)
                  )
                (loop while (not (eq nil subclassesToCall)) do
                  (if (funcall (car subclassesToCall) value)
                    (setf res t)
                  )
                  (setf subclassesToCall (cdr subclassesToCall))
                )
                res
             )
          )
    )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 						                         	  ;;;
;;;	Macro Definition	  				                  ;;;
;;;														  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	def-class
;;;	Esta macro
;;;
(defmacro def-class (nome &body atributes)
  (let ((nomeClass nome)
        (atributesClass atributes)
        (offsets (make-hash-table))
        (superClasses '()))
      (if (listp nome)
          (progn
            (setf superClasses (cdr nome))
            (setf nomeClass (car nome))
            (setf atributesClass (calculate-attributes atributes superClasses))
          )
      )
      (register-class-info nomeClass atributesClass offsets superClasses)
      (if (listp nome)
        (calculate-offsets nomeClass superClasses)
      )
      `(progn
          ,(generate-constructor nomeClass atributesClass)
          ,@(generate-getters nomeClass atributesClass)
          ,@(generate-setters nomeClass atributesClass)
          ,(generate-recognizer nomeClass)
        )
    )
)