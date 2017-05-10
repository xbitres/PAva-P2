(setf classInfo (make-hash-table))

;;;
;
; Symbol constructors
;
;;;

(defun make-symbol-constructor (nome)
  (intern (concatenate 'string "MAKE-" (symbol-name nome)))
)

(defun make-symbol-getters (nome-classe parametro)
  (intern (concatenate 'string (symbol-name nome-classe) "-" (symbol-name parametro)))
)

(defun make-symbol-recognizer (nome)
  (intern (concatenate 'string (symbol-name nome) "?"))
)

;;;
;
; Def-class helpers
;
;;;

(defun generate-constructor (nome arguments)
  `(defun ,(make-symbol-constructor nome) (&key ,@arguments) (vector ,@arguments))
)

(defun generate-getters (nome atributes)
  (let (
        (functions '())
        (parm-i 0)
       )
    (loop for param in atributes do
      (setf functions (append functions (list `(defun ,(make-symbol-getters nome param) (person) (aref person ,parm-i))) ))
      (setf parm-i (+ parm-i 1))
    )
    functions
  )
)

(defun generate-recognizer (nome atributes)
  `(defun ,(make-symbol-recognizer nome) (value) (and (typep value 'vector) (= ,(length atributes) (length value))))
)

(defun registar-class-info (nome atributes offsets)
  (setf (gethash nome classInfo) (make-hash-table))
  (setf (gethash 'atributes (gethash nome classInfo)) atributes)
  (setf (gethash 'offsets (gethash nome classInfo)) offsets)
  (setf (gethash nome (gethash 'offsets (gethash nome classInfo))) 0)
)

(defun calculate-offsets (nomeClass classes)
  (let (
      (offset 0)
     )
    (loop for class in classes do
      (setf (gethash nomeClass (gethash class classInfo)) offset)
      (setf offset (+ (list-length (gethash 'atributes (gethash class classInfo))) offset))
    )
  )
)

(defun calculate-atributes (atributes classes)
  (let ((atri '()))
    (loop for class in classes do
      (setf atri (append atri (gethash 'atributes (gethash class classInfo))))
    )
    (append atri atributes)
  )
)

;;;
;
; def-class definition
;
;;;

(defmacro def-class (nome &body atributes)
  (let ((nomeClass nome)
        (atributesClass atributes)
        (offsets (make-hash-table)))
      (if (listp nome)
          (progn
            (setf nomeClass (car nome))
            (setf atributesClass (calculate-atributes atributes (cdr nome)))
            (setf (gethash (car (cdr nome)) offsets) 0)
          )
      )
      (registar-class-info nomeClass atributesClass offsets)
      (if (listp nome)
        (calculate-offsets nomeClass (cdr nome))
      )
      `(progn
          ,(generate-constructor nomeClass atributesClass)
          ,@(generate-getters nomeClass atributesClass)
          ,(generate-recognizer nomeClass atributesClass)
        )
    )
)

(pprint (macroexpand-1 `(def-class person nome idade)))
(pprint (macroexpand-1 `(def-class worker hours)))
(pprint (macroexpand-1 `(def-class stuworker coffee redbull)))
(pprint (macroexpand-1 `(def-class (student person worker stuworker) grades)))
(pprint (macroexpand-1 `(def-class (studentPro student) sallary)))
