(def-class person nome idade)
(def-class worker hours)
(def-class stuworker coffee redbull)
(def-class (student person worker stuworker) grades)
(def-class (studentPro student) sallary)

(setf p (make-person :nome "Teste" :idade 21))
(setf s (make-student :nome "T" :idade 22 :hours 20 :coffee "black" :redbull 5))
(setf sp (make-studentPro :nome "T" :idade 22 :hours 20 :coffee "black" :redbull 5 :sallary 10.00))

(person? p) ; t -
(student? s) ; t -
(student? p) ; nil
(person? s) ; t
(worker? s) ; t
(stuworker? p) ; nil
(stuworker? s) ; t
(worker-hours sp) ; 20
(student-nome sp) ; "T"
(person-nome sp) ; "T"
(stuworker-redbull sp) ; 5
(studentPro-redbull sp) ; 5
