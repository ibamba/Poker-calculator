(*#use "combs.ml";;*)

open Cartes
open Combs

let rang_of_int i =
  (**
     renvoie le rang correspondant à un entier
     Cette fonction est la réciproque de la méthode
     getValeur : rang->int écrite plus haut
  *)
  if i=14 then As
  else if i=13 then Roi
  else if i=12 then Dame
  else if i=11 then Valet
  else if i<11 && i>1 then Num(i)
  else failwith ">rang_of_int : rang non valide"
;;

let cree_paquet52cartes () =
  (**
     Crée le paquet de 52 cartes d'un jeu de carte classique
     sans les jokers
  *)
  let res = ref[] in
  for i=2 to 14 do
    res := {color=Trefle; rank=(rang_of_int i)}::!res
  done;
  for i=2 to 14 do
    res := {color=Pique; rank=(rang_of_int i)}::!res
  done;
  for i=2 to 14 do
    res := {color=Carreau; rank=(rang_of_int i)}::!res
  done;
  for i=2 to 14 do
    res := {color=Coeur; rank=(rang_of_int i)}::!res
  done;
  !res
;;


let proba_double (d1:donne) (d2:donne) (t:table) =
  (**
     Calcul la probabilté des deux joueurs étant donné leur donne respective,
     et 3, 4 ou 5 cartes sur la table
  *)
  
  (* cas "river" *)
  if (List.length t) = 5
  then
    begin
      if (compare_hands d1 d2 t) = 1
      then 1. , 0.
      else if (compare_hands d1 d2 t) = -1
      then 0. , 1.
      else 0. , 0.
    end

  (*cas "flup" et "turn"*)
  else if ((List.length t) = 3 || (List.length t) = 4)
  then
    begin
      let tirables = ref(cree_paquet52cartes ())
      and l = d1@d2@t in
      
      (*Filtrage des cartes*)
      let filtrer c =
	tirables := List.filter (fun s-> not (s = c)) !tirables
      in
      List.iter filtrer l;

      let fins_possibles = ref[] in
      
      (*A/ Génération des fins possibles*)
      
      (* a- cas "flup" *)
      if (List.length t) = 3
      then
	begin
	  let tmp = tirables
	  and tt = ref[] in
	  let snd_membre c1 =
	    let construit_tt c2 =
	      tt := c1::c2::t;
	      fins_possibles := !tt::(!fins_possibles);
	    in tmp := List.tl !tmp;
	    List.iter construit_tt !tmp;
	  in List.iter snd_membre !tirables;
	end
	  
      (* b- cas "turn" *)
      else
	begin
	  let tt = ref[] in
	  let construit_tt c =
	    tt := c::t;
	    fins_possibles := !tt::(!fins_possibles);
	  in List.iter construit_tt !tirables;
	end;

      (*B/ Dénombrement des probabilités des deux cas*)

      let cpt1 = ref 0 and cpt2 = ref 0 and taille = ref 0 in
      let calcul_proba tt =
	taille := !taille+1;
	if(compare_hands d1 d2 tt) = 1 then cpt1 := !cpt1 + 1
	else if (compare_hands d1 d2 tt) = -1 then cpt2 := !cpt2 + 1
      in
      List.iter calcul_proba !fins_possibles;
      
      (float_of_int !cpt1) /. (float_of_int !taille) , (float_of_int !cpt2) /. float_of_int (!taille)
    end 

  (* ce cas n'arrive que s'il y a erreur dans le nombre de cartes sur la table*)
  else failwith "Le nombre de cartes sur la table n'est pas valide !"
;;


let proba_simple d t =
  (**
     Calcul la probabilté d'un joueur étant donné sa donne et ne connaissant
     pas celle de l'adversaire, et 3, 4 ou 5 cartes sur la table
  *)
  
  let tirables = ref(cree_paquet52cartes ())
  and l = d@t in
  (*Filtrage des cartes*)
  let filtrer c =
    tirables := List.filter (fun s-> not (s = c)) !tirables
  in
  List.iter filtrer l;

  let d2_possibles = ref[]
  and d2 = ref[]
  and tmp = tirables in
  let snd_membre c1 =
    let construit_d2 c2 =
      d2 := c1::c2::[];
      d2_possibles := !d2::(!d2_possibles);
    in tmp := List.tl !tmp;
    List.iter construit_d2 !tmp;
  in List.iter snd_membre !tirables;

  let sum_proba = ref 0. and taille = ref 0 in
  let calcul_proba d2 =
    taille := !taille+1;
    sum_proba := !sum_proba +. fst(proba_double d d2 t)
  in
  List.iter calcul_proba !d2_possibles;
  
  !sum_proba /. float_of_int(!taille)
;;
