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
  `(defun ,(make-symbol-recognizer nome) (value) (typep value 'vector))
)

;;;
;
; def-class definition
;
;;;

(defmacro def-class (nome &body atributes)
  `(progn
      ,(generate-constructor nome atributes)
      ,@(generate-getters nome atributes)
      ,(generate-recognizer nome atributes)
    )
)

(pprint (macroexpand-1 `(def-class person nome idade)))
