(def-class animal nome idade)

(def-class (mamifero animal) leite)

(def-class (reptil animal) poiquilotermia)

(def-class (aves animal) penas bico)

(def-class (insecto animal) invertebrado)

(def-class (animalcomAsas animal) asas)

(def-class (morcego mamifero animalcomAsas) noctivago)

(def-class (serpente reptil) venenosa)

(def-class (abelha insecto animalcomAsas) mel)

(setf an (make-animal :nome "A1" :idade 5))

(setf m (make-morcego :nome "M1" :idade 6 :leite "leite" :noctivago "noctivago" :asas "asas"))

(setf sp (make-serpente :nome "S1" :idade 7 :poiquilotermia "poiquilotermia" :venenosa "venenosa"))

(setf ab (make-abelha :nome "AB1" :idade 8 :invertebrado "invertebrado" :asas "asas" :mel "mel"))

(setf fake "I am not an animal")


;;; recognizer testing
;; todos t
(print (animal? an))
(print (animal? m))
(print (animal? sp))
(print (animal? ab))

(print (morcego? m))
(print (serpente? sp))
(print (abelha? ab))

(print (insecto? ab))
(print (reptil? sp))
(print (mamifero? m))
(print (animalcomAsas? m))
(print (animalcomAsas? ab))
;;

;; todos NIL
(print (animal? fake))
(print (morcego? fake))
(print (serpente? fake))
(print (abelha? fake))

(print (abelha? sp))
(print (abelha? m))

(print (morcego? ab))
(print (morcego? sp))

(print (animalcomasas? sp))
(print (animalcomasas? fake))
;;


;;; getters testing
(print (animal-nome an))  ;A1
(print (animal-idade an)) ;5)

(print (animalcomasas-nome an)) ;DEU A1 e nao devia dar nada : an é apenas ANIMAL)
(print (animalcomasas-idade an)) ;DEU 5 e  nao devia dar nada : an é apenas ANIMAL)

;;NIL
(print (morcego-nome an))
(print (morcego-idade an))

(print (animalcomasas-nome fake))
(print (animalcomasas-idade fake))

(print (abelha-asas an))
(print (abelha-nome an))
;;
