(def-class person nome idade)
(def-class worker hours)
(def-class stuworker coffee redbull)
(def-class (student person worker stuworker) grades)
(def-class (studentPro student) sallary)

(setf p (make-person :nome "Teste" :idade 21))
(setf s (make-student :nome "T" :idade 22 :hours 20 :coffee "black" :redbull 5))
(setf sp (make-studentPro :nome "T" :idade 22 :hours 20 :coffee "black" :redbull 5 :sallary 10.00))

(person-nome p) ; "Teste"
(person-nome! p "Kappa") ; Nil
(person-nome p) ; "Kappa"
(student-nome sp) ; "T"
(person-nome sp) ; "T"
(student-nome! sp "Heranca")
(student-nome sp) ; "Heranca"
(person-nome sp) ; "Heranca"
