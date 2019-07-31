type couleur = Trefle|Carreau|Coeur|Pique;; (*Les familles des cartes*)

type rang =
(**
   Le rang d'une carte : de Num(2) à As
*)
    Num of int
  | Valet
  | Dame
  | Roi
  | As
;;


type carte =
(**
   une carte est la donnée d'une couleur et d'un rang
*)
  { rank : rang;
    color : couleur};;

type donne = carte list;; (*Liste de 2 cartes*)

type table = carte list;; (*Liste de 5 cartes*)

type comb = QuinteFlush of rang (*Le plus haut rang du quinte flush*)
    	    | Carre of rang list (*liste de 2 rangs : le rang du Carre et celui de la 5e carte*)
	    | Couleur of rang list (*Liste des rangs des 5 cartes de mm couleur*)
	    | Full of rang list (*Liste de 2 rangs : celui du brelan et celui de la paire*)
            | Suite of rang (*Le plus haut rang de la suite*)
	    | Brelan of rang list (*Liste de 3 rangs : celui du brealan et ceux des 4e et 5e carte*)
	    | DoublePaire of rang list (*Liste de 3 rangs : ceux de la première paire, puis de la 2e et de la 5e carte*)
	    | Paire of rang list (*Liste de 4 rangs : ceux de la paire, et des 3 autres cartes complémentaires*)
	    | CarteHaute of rang list (*Les 5 rangs les plus hauts*)
;;


let stringCarte c =
  let res = ref "" in
  begin
    match c.rank with
      As -> res := !res^"As"
    | Roi -> res := !res^"Roi"
    | Dame -> res := !res^"Dame"
    | Valet -> res := !res^"Valet"
    | Num(a) -> res := !res^(string_of_int a)
  end;
  res := !res^" de ";
  begin
    match c.color with
      Trefle -> res := !res^"Trefle"
    | Pique -> res := !res^"Pique"
    | Coeur -> res := !res^"Coeur"
    | Carreau -> res := !res^"Carreau"
  end;
  !res
;;


let getValeur r = match r with
    As -> 14
  | Roi -> 13
  | Dame -> 12
  | Valet -> 11
  | Num(a) -> a;;

let rec insert c l = match l with
  (**
     insert une carte dans une liste triée par rang en gardant cette liste toujours triée.
     Dans le tri, si deux cartes ont le même rang, on préfère regarder si la carte qui
     les précède est de la mm couleur que l'une d'entre-elles. Dans ce cas,
     on placera la carte de mm couleur en premier : on respecte donc l'ordre des
     couleurs en cas d'égalité de rang.
  *)
  | [] -> [c]
  | [a] -> if (getValeur c.rank)>=(getValeur a.rank) then [c; a]
    else [a; c]
  | a::b::h -> if (getValeur c.rank)>=(getValeur a.rank)
    then
      begin
	if ((getValeur a.rank) = (getValeur b.rank)) && (b.color = c.color) then (c::b::a::h)
	else (c::a::b::h)
      end
    else if (getValeur c.rank)>(getValeur b.rank) then (a::c::b::h)
    else if (getValeur c.rank)<(getValeur b.rank) then a::b::(insert c h)
    else (*c.rank = b.rank*)
      begin
	if c.color = a.color then (a::c::b::h)
	else (a::b::c::h)
      end
;;

(*tri par rang décroissant*)
let rec sort l = match l with
  | [] -> []
  | a::h -> insert a (sort h);;


let affiche_carte_list lc =
  (**
     Affiche une liste de cartes
     @param liste de carte
  *)
  print_newline ();
  for i=0 to (List.length lc)-1 do
    print_string "----------------"
  done;
  print_newline();
  let affiche_carte c =
    print_string ("| "^(stringCarte c)^" ");
  in List.iter affiche_carte lc;
  print_string "|"; print_newline ();
  for i=0 to (List.length lc)-1 do
    print_string "----------------"
  done;
  print_newline ()
;;
