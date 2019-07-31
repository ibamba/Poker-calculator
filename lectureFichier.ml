(*#use "calculetteProbaPoker.ml";;*)

open Cartes
open Combs
open CalculetteProbaPoker

let couleur_of_list l = match l with
    'p'::h -> Pique
  | 'c'::'a'::h -> Carreau
  | 'c'::'o'::h -> Coeur
  | 't'::h -> Trefle
  | _ -> failwith ">couleur_of_list : Couleur non valide"
;;

let carte_of_list l = match l with
    'A'::h -> {color = (couleur_of_list h); rank = As}
  | 'R'::h -> {color = (couleur_of_list h); rank = Roi}
  | 'D'::h -> {color = (couleur_of_list h); rank = Dame}
  | 'V'::h -> {color = (couleur_of_list h); rank = Valet}
  | '1'::'0'::h -> {color = (couleur_of_list h); rank = Num(10)}
  | i::h -> {color = (couleur_of_list h); rank = (rang_of_int (int_of_char i - int_of_char '0'))}
  | _ -> failwith ">carte_of_list : Rang non valide"
;;

let rec cartesList_of_list l = match l with
    [] | [['?']] -> []
  | l1::h -> (carte_of_list l1)::(cartesList_of_list h)
;;

let list_of_string s =
  let res = ref[]
  and ll = ref[]
  and taille = (String.length s)-1 in
  let fin = ref(taille) in
  for i=taille downto 0 do
    if s.[i] = ' '
    then
      begin
	for j = !fin downto i+1 do
	  ll := s.[j]::(!ll)
	done;
	res := !ll::(!res);
	fin := i-1;
	ll := [];
      end
  done;
  for j = !fin downto 0 do
    ll := s.[j]::(!ll)
  done;
  res := !ll::(!res);
  !res
;;

let cartesList_of_string s =
  cartesList_of_list (list_of_string s)
;;

let charge_cartes file =
  let in_chanel = (open_in file) in
  let d1 = (cartesList_of_string (input_line in_chanel)) in
  let d2 = (cartesList_of_string (input_line in_chanel)) in
  let t = (cartesList_of_string (input_line in_chanel)) in
  close_in in_chanel;
  d1, d2, t
;;

let compute file =
  let donnees = (charge_cartes file) in
  let proba_d1 = ref(-1.) and proba_d2 = ref(-1.) in
  begin
    match donnees with
      ([], d2, t) -> proba_d2 := (proba_simple d2 t)
    | (d1, [], t) -> proba_d1 := (proba_simple d1 t)
    | (d1, d2, t) ->
       begin
	 let tmp = (proba_double d1 d2 t) in
	 proba_d1 := (fst tmp);
	 proba_d2 := (snd tmp);
       end
  end;
  if !proba_d1 = 1. then print_string "Joueur 1 est gagnant\n"
  else if !proba_d2 = 1. then print_string "Joueur 2 est gagnant\n"
  else
    begin
      if not (!proba_d1 = (-1.))
      then
	begin
	  print_string "Joueur 1 : "; print_float !proba_d1; print_newline()
	end;
      if not (!proba_d2 = (-1.))
      then
	begin
	  print_string "Joueur 2 : "; print_float !proba_d2; print_newline()
	end;
    end
;;

let main () =
  if Array.length Sys.argv != 2
  then (print_string "Aucun argument passé en paramètre; veuillez ajouter le nom d'un fichier"; print_newline())
  else compute Sys.argv.(1)
;;

main ();;
