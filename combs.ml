(*#use "cartes.ml";;*)

open Cartes

let maxRang a b =
  if (getValeur a) < (getValeur b) then b
  else a;;

let estCouleur (d:donne) (t:table) =
  (**
     renvoie la combinaison Couleur avec pour argument la liste triée des 5 rangs 
     max entrant dans une mm couleur si couleur il y a ou une liste vide sinon
  *)
  let types_col = Array.make 4 []
  and card = Array.make 4 0
  and i_col = ref (-1)
  and liste = d@t in
  let sort_coul c =
    let i = match c.color with
	Coeur    -> 0
      | Carreau  -> 1
      | Trefle   -> 2
      | Pique    -> 3
    in
    begin
      types_col.(i)  <- c::types_col.(i);
      card.(i) <- 1 + card.(i);
      if card.(i) > 4 then i_col := i
    end			      
  in
  (List.iter sort_coul liste);
  if !i_col >= 0 then
    match (sort types_col.(!i_col)) with
      c1::c2::c3::c4::c5::_ -> Couleur([c1.rank; c2.rank; c3.rank; c4.rank; c5.rank])
    | _ -> failwith "couleur : erreur de cardinalite"
  else Couleur([])
;;
(*
(*Test couleur*)
let d1 = [{color=Pique; rank=Dame}; {color=Coeur; rank=Dame}];;
let t1 = [{color=Coeur; rank=Num(10)}; {color=Coeur; rank=Num(2)};
	 {color=Coeur; rank=Valet};{color=Coeur;rank=Num(5)};
	 {color=Coeur; rank=As}];;
estCouleur d1 t1;;
*)


let estSuite (d:donne) (t:table) =
  (**
     renvoie la combinaison Suite avec la carte de plus haut rang 
     de la suite si suite y a ou un rang de valeur -1
  *)
  let liste = sort (d@t)
  and suite_5 = Array.make 5 false (*pour savoir si on peut avoir une suite avec 5 comme plus haut rang*) in
  let prec = ref((List.hd liste).rank)
  and cpt = ref 1
  and res = ref(Num(-1)) in  
  let succession c =
    (*suite avec 5 comme plus haut rang*)
    if c.rank = As then suite_5.(0) <- true
    else if getValeur(c.rank) < 6
    then (let ind = (getValeur(c.rank))-1 in suite_5.(ind) <- true);

    if not(getValeur(c.rank) = getValeur(!prec))
    (*On pourait avoir des cartes de mm rang dans la liste trié comme dans cet exemple : 9-8-7-7-6-5-2.
      Dans ce cas, à la rencontre de la deuxieme carte, la condition précédente n'est pas vérifiée.
      Mais pas de panique, on ne remettra pas le compteur à 0 : on ne traite donc plus ce rang à nouveau*)
    then
      begin
	if getValeur(c.rank) = getValeur(!prec)-1
	then
	  begin
	    cpt := !cpt+1;
	    if !res = Num(-1) then res := !prec;
	  end
	else if !cpt < 5 then (cpt := 1; res := Num(-1));
	prec := c.rank
      end
  in
  List.iter succession liste;
  if not(!res = Num(-1)) && !cpt>=5
  then Suite(!res)
  else if suite_5.(0) && suite_5.(1) && suite_5.(2) && suite_5.(3) && suite_5.(4)
  then Suite(Num(5))
  else Suite(Num(-1))
;;
(*
(*Test suite*)
let d2 = [{color=Coeur; rank=Dame}; {color=Coeur; rank=Valet}];;
let t2 = [{color=Coeur; rank=Num(10)}; {color=Coeur; rank=Num(9)};
	 {color=Trefle; rank=Num(4)};{color=Pique;rank=Num(6)};
	 {color=Trefle; rank=Num(8)}];;
estSuite d2 t2;;

let d2p = [{color=Coeur; rank=As}; {color=Coeur; rank=Num(4)}];;
let t2p = [{color=Pique; rank=Num(5)}; {color=Coeur; rank=Num(2)};
	 {color=Trefle; rank=Num(4)};{color=Coeur;rank=Num(8)};
	 {color=Trefle; rank=Num(3)}];;
estSuite d2p t2p;;
*)


let estQuinteFlush (d:donne) (t:table) = 
  (**
     renvoie la combinaison QuinteFlush avec la carte de plus haut rang 
     du quinte flush si quinte flush y a ou un rang de valeur -1
     remarque : cette methode est très proche de celle de la suite
  *)
  let liste = sort (d@t)
  and qf_5 = Array.make 5 (false, Trefle) (*pour savoir si on peut avoir une quinte flush avec 5 comme plus haut rang*) in
  let prec = ref(List.hd liste)
  and cpt = ref 1
  and res = ref(Num(-1)) in  
  let succession_and_col c =
    (*quinte flush avec 5 comme plus haut rang*)
    if c.rank = As then qf_5.(0) <- (true, c.color)
    else if getValeur(c.rank) < 6
    then
      begin
	let ind = (getValeur(c.rank))-1 in
	if not(fst qf_5.(ind)) then qf_5.(ind) <- (true, c.color)
	else if (fst qf_5.(ind)) && (c.color = (snd qf_5.(0))) then qf_5.(ind) <- (true, c.color)
      end;
    if not(getValeur(c.rank) = getValeur((!prec).rank))
    (*On pourait avoir des cartes de mm rang dans la liste trié comme dans cet exemple : 9-8-7-7-6-5-2.
      Dans ce cas, à la rencontre de la deuxieme carte, la condition précédente n'est pas vérifiée.
      Mais pas de panique, on ne remettra pas le compteur à 0 : on ne traite donc plus ce rang à nouveau*)
    then
      begin
	if (getValeur(c.rank) = getValeur((!prec).rank)-1) && (c.color = (!prec).color)
	then
	  begin
	    cpt := !cpt+1;
	    if !res = Num(-1) then res := (!prec).rank;
	  end
	else if !cpt < 5 then (cpt := 1; res := Num(-1));
	prec := c
      end
  in
  List.iter succession_and_col liste;
  if not(!res = Num(-1)) && !cpt>=5
  then QuinteFlush(!res)
  else if (fst qf_5.(0)) && (fst qf_5.(1)) && (fst qf_5.(2)) &&  (fst qf_5.(3)) && (fst qf_5.(4))
      && (((snd qf_5.(0)) = (snd qf_5.(1))) && ((snd qf_5.(0)) = (snd qf_5.(2)))
	  && ((snd qf_5.(0)) =  (snd qf_5.(3))) && ((snd qf_5.(0)) = (snd qf_5.(4))))
  then QuinteFlush(Num(5))
  else QuinteFlush(Num(-1))
;;
(*
(*Test quinteFlush*)
let d03 = [{color=Coeur; rank=Dame}; {color=Coeur; rank=Valet}];;
let t03 = [{color=Coeur; rank=Num(10)}; {color=Coeur; rank=Num(9)};
	 {color=Trefle; rank=Num(4)};{color=Pique;rank=Num(6)};
	 {color=Coeur; rank=Num(8)}];;
estQuinteFlush d03 t03;;

let d03p = [{color=Carreau; rank=As}; {color=Carreau; rank=Num(4)}];;
let t03p = [{color=Carreau; rank=Num(5)}; {color=Carreau; rank=Num(2)};
	 {color=Trefle; rank=Num(4)};{color=Coeur;rank=Num(8)};
	 {color=Carreau; rank=Num(3)}];;
estQuinteFlush d03p t03p;;
*)


let complements_simple r_comb l i =
(**
   Complete une combinaison donnée (sauf Double Paire) avec les cartes de plus haut rang complémentaires
*)
  let new_liste = sort (List.filter (fun c-> not (c.rank=r_comb)) l) in
  match new_liste with
  | c1::h when i=1 -> [c1.rank] (*complement Carre*)
  | c1::c2::h when i=2 -> [c1.rank; c2.rank] (* - - Brelan*)
  | c1::c2::c3::h when i=3 -> [c1.rank; c2.rank; c3.rank] (*- - - Paire*)
  | c1::c2::c3::c4::h when i=4 -> [c1.rank; c2.rank; c3.rank; c4.rank] (*- - - Carte Haute*)
  | _ ->  failwith ">complements_simple : erreur dans le nombre d'elts complementaires demandé"
;;

let complements_double r1_comb r2_comb l =
(**
   Complete la combinaison Double Paire avec la carte de plus haut rang complémentaire
*)
  let new_liste = sort (List.filter (fun c-> not (c.rank=r2_comb)) (List.filter (fun c-> not (c.rank=r1_comb)) l)) in
  match new_liste with
    [] ->  failwith ">complements_double : pas assez d'elts dans la liste"
  | c::h -> [c.rank]
;;

let reste_combs (d:donne) (t:table) =
  (**
     compte le nombre de carte de chaque rang apparaissant dans la donne et la table
     puis renvoie, en fonction du resultat obtenu, un Carre, un full, un brelan,
     une double paire ou une paire si l'une de ces combinaisons y a ou la carte haute sinon
  *)
  let liste = sort (d@t)
  and denombre = Array.make 13 []
  and card = Array.make 13 0
  and i_carre = ref (-1) and i_brel = ref (-1) and i_paire = ref (-1) and i_paire_inf = ref (-1) in
  let trie c = match c.rank with
      As -> begin
	denombre.(12) <- (c.rank)::denombre.(12); card.(12) <- card.(12)+1;
	if card.(12) > 3 then (i_carre := 12; i_brel := -1; i_paire := -1)
	else if card.(12) = 3 then (i_brel := 12; i_paire := -1)
	else if card.(12) = 2 then i_paire := 12
      end
	
    | Roi -> begin
      denombre.(11) <- (c.rank)::denombre.(11); card.(11) <- card.(11)+1;
      if card.(11) > 3 
      then
	begin
	  i_carre := 11;
	  if !i_brel = 11 then i_brel := -1;
	  if !i_paire = 11 then i_paire := -1
	end
      else if card.(11) = 3
      then
	begin
	  if !i_brel = -1 then
	    (i_brel := 11;
	     if !i_paire=11 then i_paire := -1)
	end
      else if card.(11) = 2 && !i_paire = -1
      then i_paire := 11
      else if card.(11) = 2 && !i_paire > -1 (*on a deja une paire*)
      then i_paire_inf := 11
    end
       
    | Dame -> begin
      denombre.(10) <- (c.rank)::denombre.(10); card.(10) <- card.(10)+1;
      if card.(10) > 3 
      then
	begin
	  i_carre := 10;
	  if !i_brel = 10 then i_brel := -1;
	  if !i_paire = 10 then i_paire := -1
	end
      else if card.(10) = 3
      then
	begin
	  if !i_brel = -1 then
	    (i_brel := 10;
	     if !i_paire = 10 then i_paire := -1)
	end
      else if card.(10) = 2 && !i_paire = -1 (*on a deja une paire, mais de rang inf*)
      then i_paire := 10
      else if card.(10) = 2 && !i_paire_inf = -1
      then i_paire_inf := 10
    end
       
    | Valet -> begin
      denombre.(9) <- (c.rank)::denombre.(9); card.(9) <- card.(9)+1;
      if card.(9) > 3 
      then
	begin
	  i_carre := 9;
	  if !i_brel = 9 then i_brel := -1;
	  if !i_paire = 9 then i_paire := -1
	end
      else if card.(9) = 3
      then
	begin
	  if !i_brel = -1 then
	    (i_brel := 9;
	     if !i_paire = 9 then i_paire := -1)
	end
      else if card.(9) = 2 && !i_paire = -1
      then i_paire := 9
      else if card.(9) = 2 && !i_paire_inf = -1
      then i_paire_inf := 9
    end
	
    | Num(i) -> begin
      denombre.(i-2) <- (c.rank)::denombre.(i-2); card.(i-2) <- card.(i-2)+1;
      if card.(i-2) > 3 
      then
	begin
	  i_carre := i-2;
	  if !i_brel = i-2 then i_brel := -1;
	  if !i_paire = i-2 then i_paire := -1
	end
      else if card.(i-2) = 3
      then
	begin
	  if !i_brel < (i-2) && !i_brel > -1 (*si on a deja un brelan de rang plus faible*)
	  then (i_paire := !i_brel; i_brel := i-2) (*alors on le transforme en paire et on le remplace par le nouveau brelan*) 
	  else if !i_brel = -1 then
	    (i_brel := i-2;
	     if !i_paire = (i-2) then i_paire := -1)
	end
      else if card.(i-2) = 2
      then
	begin
	  if !i_paire < (i-2) && !i_paire > -1 (*Idem que prec*)
	  then (i_paire_inf := !i_paire; i_paire := i-2)
	  else if !i_paire = -1 then i_paire := i-2
	  (*on a une paire de rang supp. On passe alors à la paire inf*)
	  else if !i_paire_inf < (i-2)
	  then i_paire_inf := (i-2)
	end
    end
  in 
  List.iter trie liste;

  (*renvoi de la plus haute comb trouvée*)
  if !i_carre > -1
  then begin
    match denombre.(!i_carre), (complements_simple (List.hd denombre.(!i_carre)) liste 1) with
      r1::h, [r2] -> Carre([r1; r2])
    | _ -> failwith ">comb Carre : probleme du card de carte"
  end
  else if !i_brel > -1 && !i_paire > -1
  then begin
    match denombre.(!i_brel), denombre.(!i_paire) with
      r1::h, r2::q -> Full([r1; r2])
    | _ -> failwith ">comb Full : probleme du card de carte"
  end
  else if !i_brel > -1
  then begin
    match denombre.(!i_brel), (complements_simple (List.hd denombre.(!i_brel)) liste 2) with
      r1::h, [r2; r3] -> Brelan([r1; r2; r3])
    | _ -> failwith ">comb Brelan : probleme du card de carte"
  end
  else if !i_paire > -1 && !i_paire_inf > -1
  then begin
    match denombre.(!i_paire),denombre.(!i_paire_inf),(complements_double (List.hd denombre.(!i_paire)) (List.hd denombre.(!i_paire_inf)) liste) with
      r1::h, r2::q, [r3] -> DoublePaire([r1; r2; r3])
    | _ -> failwith ">comb Double Paire : probleme du card de carte"
  end
  else if !i_paire > -1
  then begin
    match denombre.(!i_paire), (complements_simple (List.hd denombre.(!i_paire)) liste 3) with
      r1::h, [r2; r3; r4] -> Paire([r1; r2; r3; r4])
    | _ -> failwith ">comb Paire : probleme du card de carte"
  end
  else
    begin
      let r1 = (List.hd liste).rank in
      match complements_simple r1 liste 4 with
	[r2; r3; r4; r5] -> CarteHaute([r1; r2; r3; r4; r5])
      | _ -> failwith ">comb Carte Haute : probleme du card de carte"
    end
;;

(*
(*Test Carre*)
let d3 = [{color=Coeur; rank=As}; {color=Coeur; rank=As}];;
let t3 = [{color=Coeur; rank=Dame}; {color=Coeur; rank=Num(9)};
	 {color=Trefle; rank=As};{color=Coeur;rank=Valet};
	 {color=Trefle; rank=As}];;
reste_combs d3 t3;;
(*Test Brelan*)
let d4 = [{color=Coeur; rank=Valet}; {color=Pique; rank=Valet}];;
let t4 = [{color=Coeur; rank=Num(2)}; {color=Pique; rank=Roi};
	 {color=Trefle; rank=As};{color=Coeur;rank=Dame};
	 {color=Trefle; rank=Valet}];;
reste_combs d4 t4;;
(*Test Full*)
let d5 = [{color=Coeur; rank=Valet}; {color=Trefle; rank=Valet}];;
let t5 = [{color=Carreau; rank=Valet}; {color=Coeur; rank=As};
	 {color=Trefle; rank=As};{color=Coeur;rank=Num(10)};
	 {color=Pique; rank=As}];;
reste_combs d5 t5;;
(*Test Paire*)
let d6 = [{color=Coeur; rank=Valet}; {color=Coeur; rank=Num(10)}];;
let t6 = [{color=Pique; rank=Num(5)}; {color=Coeur; rank=As};
	 {color=Trefle; rank=Dame};{color=Coeur;rank=Num(8)};
	 {color=Trefle; rank=As}];;
reste_combs d6 t6;;
(*Test Double Paire*)
let d7 = [{color=Coeur; rank=As}; {color=Pique; rank=Dame}];;
let t7 = [{color=Trefle; rank=Dame}; {color=Carreau; rank=As};
	  {color=Coeur; rank=Roi}; {color=Pique; rank=Num(2)};
	  {color=Coeur; rank=Roi}];;
reste_combs d7 t7;;
*)


let compare_comb c1 c2 = match c1, c2 with
  | QuinteFlush(_), QuinteFlush(_) -> 0
  | QuinteFlush(_), _ -> 1
  | _, QuinteFlush(_) -> -1
     
  | Carre(_), Carre(_) -> 0
  | Carre(_), _ -> 1
  | _, Carre(_) -> -1
     
  | Full(_), Full(_) -> 0
  | Full(_), _ -> 1
  | _, Full(_) -> -1
     
  | Couleur(_), Couleur(_) -> 0
  | Couleur(_), _ -> 1
  | _, Couleur(_) -> -1
     
  | Suite(_), Suite(_) -> 0
  | Suite(_), _ -> 1
  | _, Suite(_) -> -1
     
  | Brelan(_), Brelan(_) -> 0
  | Brelan(_), _ -> 1
  | _, Brelan(_) -> -1
     
  | DoublePaire(_), DoublePaire(_) -> 0
  | DoublePaire(_), _ -> 1
  | _, DoublePaire(_) -> -1
     
  | Paire(_), Paire(_) -> 0
  | Paire(_), _ -> 1
  | _, Paire(_) -> -1
     
  | _, _-> 0
;;


let compute_comb_max (d:donne) (t:table) =
(**
   extrait directement d’une donne et de cinq cartes la plus grande combinaison possible
*)
  let qf = estQuinteFlush d t in
  if not(qf = QuinteFlush(Num(-1)))
  then qf
  else
    begin
      let comb = reste_combs d t in
      match comb with
	Carre(k) -> comb
      | Full(k) -> comb
      | _ ->
	 begin
	   let col = (estCouleur d t) in
	   if not(col = Couleur([])) then col
	   else begin
	     let suit = (estSuite d t) in
	     if not(suit = Suite(Num(-1))) then suit
	     else comb
	   end
	 end
    end
;;


let compare_hands d1 d2 t =
  (**
     Compare les donnes de deux joueurs étant donné 5 cartes sur
     la table
  *)
  let c1 = compute_comb_max d1 t in
  let c2 = compute_comb_max d2 t in
  if (compare_comb c1 c2) = 1
  then 1
  else if (compare_comb c1 c2) = -1
  then -1
  else
    begin
      match c1, c2 with
	
      | Paire(r1::h1), Paire(r2::h2) | Brelan(r1::h1), Brelan(r2::h2) | Carre(r1::h1), Carre(r2::h2)
	-> if (getValeur r1)>(getValeur r2)
	  then 1
	  else if (getValeur r1)<(getValeur r2) then -1
	  else
	    begin
	      match h1, h2 with
		[x1;x2;x3], [y1;y2;y3] (*cas de la paire*)
		  -> if (getValeur x1)>(getValeur y1) then 1
		    else if (getValeur x1)<(getValeur y1) then -1
		    else if (getValeur x2)>(getValeur y2) then 1
		    else if (getValeur x2)<(getValeur y2) then -1
		    else if (getValeur x3)>(getValeur y3) then 1
		    else if (getValeur x3)<(getValeur y3) then -1
		    else 0
	      | [x1;x2], [y1;y2] (*cas du brelan*)
		-> if (getValeur x1)>(getValeur y1) then 1
		  else if (getValeur x1)<(getValeur y1) then -1
		  else if (getValeur x2)>(getValeur y2) then 1
		  else if (getValeur x2)<(getValeur y2) then -1
		  else 0
	      | [x], [y] (*cas du carre*)
		-> if (getValeur x)>(getValeur y) then 1
		  else if (getValeur x)<(getValeur y) then -1
		  else 0
	      |_, _ -> failwith ">compare_hands : paire,brelan,carre : cas inexistant"
	    end
	      
      | Suite(r1), Suite(r2) | QuinteFlush(r1), QuinteFlush(r2)
	-> if (getValeur r1)>(getValeur r2) then 1
	  else if (getValeur r1)<(getValeur r2) then -1
	  else 0
	    
      | CarteHaute([x1; x2; x3; x4; x5]), CarteHaute([y1; y2; y3; y4; y5]) | Couleur([x1; x2; x3; x4; x5]), Couleur([y1; y2; y3; y4; y5])
	-> if (getValeur x1)>(getValeur y1) then 1
	  else if (getValeur x1)<(getValeur y1) then -1
	  else if (getValeur x2)>(getValeur y2) then 1
	  else if (getValeur x2)<(getValeur y2) then -1
	  else if (getValeur x3)>(getValeur y3) then 1
	  else if (getValeur x3)<(getValeur y3) then -1
	  else if (getValeur x4)>(getValeur y4) then 1
	  else if (getValeur x4)<(getValeur y4) then -1
	  else if (getValeur x5)>(getValeur y5) then 1
	  else if (getValeur x5)<(getValeur y5) then -1
	  else 0
	    
      | Full([x1; x2]), Full([y1; y2])
	-> if (getValeur x1)>(getValeur y1) then 1
	  else if (getValeur x1)<(getValeur y1) then -1
	  else if (getValeur x2)>(getValeur y2) then 1
	  else if (getValeur x2)<(getValeur y2) then -1
	  else 0
	    
      | DoublePaire([x1; x2; x3]), DoublePaire([y1; y2; y3])
	-> if (getValeur x1)>(getValeur y1) then 1
	  else if (getValeur x1)<(getValeur y1) then -1
	  else if (getValeur x2)>(getValeur y2) then 1
	  else if (getValeur x2)<(getValeur y2) then -1
	  else if (getValeur x3)>(getValeur y3) then 1
	  else if (getValeur x3)<(getValeur y3) then -1
	  else 0
	    
      | _, _ (*cas qui n'arrivera jamais*)
	-> failwith ">compare_hands : cas inexistant"
	 
    end
;;
