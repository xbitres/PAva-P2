(def-class animal
	nome
	idade)

(def-class (mamifero animal)
	leite)

(def-class (reptil animal)
	poiquilotermia)

(def-class (aves animal)
	penas
	bico)

(def-class (insecto animal)
	invertebrado)

(def-class (animalcomAsas animal)
	asas)

(def-class (morcego mamifero animalcomAsas)
	noctivago)

(def-class (serpente reptil)
	venenosa)

(def-class (abelha insecto animalcomAsas)
	mel)

(setf an (make-animal :nome "A1" :idade 5))

(setf m (make-morcego :nome "M1" :idade 6 :leite "leite" :noctivago "noctivago" :asas "asas"))

(setf sp (make-serpente :nome "S1" :idade 7 :poiquilotermia "poiquilotermia" :venenosa "venenosa"))

(setf ab (make-abelha :nome "AB1" :idade 8 :invertebrado "invertebrado" :asas "asas" :mel "mel"))

(setf fake "I am not an animal")


;;; recognizer testing
;; todos t
(animal? an)
(animal? m)
(animal? sp)
(animal? ab)

(morcego? m)
(serpente? sp)
(abelha? ab)

(insecto? ab)
(reptil? sp)
(mamifero? m)
(animalcomAsas? m)
(animalcomAsas? ab)
;;

;; todos NIL
(animal? fake)
(morcego? fake)
(serpente? fake)
(abelha? fake)

(abelha? sp)
(abelha? m)

(morcego? ab)
(morcego? sp)

(animalcomasas? sp)
(animalcomasas? fake)
;;


;;; getters testing
(animal-nome an)  ;A1
(animal-idade an) ;5

(animalcomasas-nome an) ;DEU A1 e nao devia dar nada : an é apenas ANIMAL
(animalcomasas-idade an) ;DEU 5 e  nao devia dar nada : an é apenas ANIMAL

;;NIL
(morcego-nome an)
(morcego-idade an)

(animalcomasas-nome fake)
(animalcomasas-idade fake)

(abelha-asas an)
(abelha-nome an)
;;