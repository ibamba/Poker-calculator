open Cartes
open Combs
open CalculetteProbaPoker
open LectureFichier



let main () =
  if Array.length Sys.argv != 2
  then (print_string "Aucun argument passé en paramètre; veuillez ajouter le nom d'un fichier"; print_newline())
  else let s = compute Sys.argv.(1) in 
  print_string s 
;;