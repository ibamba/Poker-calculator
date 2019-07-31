all :	cartes.cmo combs.cmo calculetteProbaPoker.cmo ia.cmo lectureFichier.cmo
	ocamlc -o ia cartes.cmo combs.cmo calculetteProbaPoker.cmo ia.cmo #L'exécutable ia(intelligence artificielle)
	ocamlc -o compute cartes.cmo combs.cmo calculetteProbaPoker.cmo lectureFichier.cmo #L'exécutable compute

#Compilation du corps du module Cartes
cartes.cmo :  cartes.ml
	ocamlc -c cartes.ml

#Compilation du corps du module Combs
combs.cmo : combs.ml cartes.cmo
	ocamlc -c combs.ml

#Compilation du corps du module CalculetteProbaPoker
calculetteProbaPoker.cmo : calculetteProbaPoker.ml cartes.cmo combs.cmo
	ocamlc -c calculetteProbaPoker.ml

#Compilation du corps du module LectureFichier
lectureFichier.cmo : lectureFichier.ml cartes.cmo combs.cmo calculetteProbaPoker.cmo
	ocamlc -c lectureFichier.ml


#Compilation du corps du module Ia
ia.cmo : ia.ml cartes.cmo combs.cmo calculetteProbaPoker.cmo
	ocamlc -c ia.ml

#Effacer fichiers auxiliares
clean :
	rm *.cmi *.cmo compute ia
