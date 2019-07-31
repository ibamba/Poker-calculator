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

type couleur = Trefle|Carreau|Coeur|Pique;; (*Les familles des cartes*)

type carte =
(**
   une carte est la donnée d'une couleur et d'un rang
*)
  {color : couleur;
   rank : rang};;

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

let getValeur r = match r with
    As -> 14
  | Roi -> 13
  | Dame -> 12
  | Valet -> 11
  | Num(a) -> a;;

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

let l = [{color=Coeur; rank=As}; {color=Pique; rank=Dame};
	 {color=Pique; rank=Num(10)}; {color=Pique; rank=Num(2)};
	 {color=Trefle; rank=Valet};{color=Coeur;rank=Num(2)};
	 {color=Pique; rank=Roi}; {color=Coeur;rank=Dame}];;
sort l;;

let maxRang a b =
  if (getValeur a) < (getValeur b) then b
  else a;;

let estCouleur d t =
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
(*Test couleur*)
let d1 = [{color=Pique; rank=Dame}; {color=Coeur; rank=Dame}];;
let t1 = [{color=Coeur; rank=Num(10)}; {color=Coeur; rank=Num(2)};
	 {color=Coeur; rank=Valet};{color=Coeur;rank=Num(5)};
	 {color=Coeur; rank=As}];;
estCouleur d1 t1;;


let estSuite d t =
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


let estQuinteFlush d t = 
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

let reste_combs d t =
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
let t7 = [{color=Trefle; rank=Dame}; {color=Carreau; rank=Roi};
	  {color=Coeur; rank=Roi}; {color=Pique; rank=Num(2)};
	  {color=Coeur; rank=Roi}];;
reste_combs d7 t7;;
(*****************************************************************************)

let compute_comb_max d t =
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

compute_comb_max d1 t1;;
compute_comb_max d2 t2;;
compute_comb_max d03p t03p;;
compute_comb_max d3 t3;;
compute_comb_max d4 t4;;
compute_comb_max d5 t5;;
compute_comb_max d6 t6;;
compute_comb_max d7 t7;;

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

compare_hands d1 d2 t1;;
compare_hands d1 d2 t2;;
compare_hands d5 d4 t1;;
compare_hands d1 d1 t1;;

(******************************* CALCUL DE PROBABILITES DE VICTOIRE **************************)

let donne1 = [{color=Pique; rank=Num(3)}; {color=Coeur; rank=Num(10)}];;
let donne2 = [{color=Coeur; rank=Num(8)}; {color=Trefle; rank=Num(3)}];;
let table1 = [{color=Pique; rank=Num(6)}; {color=Carreau; rank=Num(8)};
	     {color=Pique; rank=Num(9)}];;
let table2 = [{color=Pique; rank=Num(6)}; {color=Carreau; rank=Num(8)};
	     {color=Pique; rank=Num(9)}; {color=Carreau;rank=Valet}];;
let table3 = [{color=Pique; rank=Num(6)}; {color=Carreau; rank=Num(8)};
	     {color=Pique; rank=Num(9)}; {color=Carreau;rank=Valet};
	     {color=Trefle; rank=Dame}];;

compare_hands donne1 donne2 table3;;

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


let proba_double d1 d2 t =
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


(*All tests validates by poker-news*)
proba_double donne1 donne2 table1;;
proba_double donne1 donne2 table2;;
proba_double donne1 donne2 table3;;


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

(*proba_simple donne1 table1;;*)
proba_simple donne1 table2;;
proba_simple donne1 table3;;
proba_double donne1 donne2 table1;;
proba_double donne1 donne2 table2;;
proba_double donne1 donne2 table3;;


(*************************************************LECTURE FICHIER***************************************************************************)

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

(compute "test1.txt");;
(compute "test2.txt");;
(compute "test3.txt");;
