open Cartes
open Combs
open CalculetteProbaPoker

type joueur =
  (**
     Un joueur est la donnée d'une donne, des jetons,
     une probabilité de victoire, et un boolean disant
     s'il est le donneur ou non
  *)
  {mutable gives : donne;
   mutable jetons : int;
   mutable proba_victoire : float;
   mutable donneur : bool}
;;


let afficheJoueur blind j =
  (**
     Affiche la donne et la probabilité du joueur si on n'est au moins au flop
     @arg blind : boolean disant si on est encore au blind ou non
     @arg j : le joueur à afficher
  *)
  print_string "Votre donne : ";
  affiche_carte_list j.gives;
  print_string ("Jetons : "^string_of_int j.jetons);
  print_newline ();
  print_string "Votre probabilité de gagner est de : ";
  if blind then print_string "INCONNUE !\n"
  else print_float j.proba_victoire;
  print_newline ()
;;


let croupier n paquet =
  (**
     Le croupier distrinue les cartes parmi le paquet
     Choisi aléatoirement un nombre donné de cartes parmi le paquet
     @arg n : le nombre de carte à sélectionner
     @arg paquet : le paquet de carte
     @return liste de n cartes sélectionnées aléatoirement dans le paquet
  *)
  let res = ref [] in
  let rec aux i =
    if i<=0 then !res
    else
      begin
	let ind = Random.int(List.length !paquet) in
	let c = List.nth !paquet ind in
	res := c::(!res);
	paquet := List.filter (fun s-> not (s = c)) !paquet;
	aux (i-1)
      end;
  in
  aux n
;;
  

let perd joueurs =
  joueurs.(0).jetons <= 0
;;


let gagne joueurs =
  joueurs.(1).jetons <= 0
;;


let getNonDonneur joueurs =
  if joueurs.(0).donneur then 1
  else 0
;;

let comptabilise joueurs ind_miseur mise_demande mise =
  (**
     Facture au joueur sa mise demandée si possible et l'ajoute à la mise sur la table
     @arg joueurs : tableau des deux joueurs
     @arg ind_miseur : l'indice du miseur dans le tableau
     @arg mise_demande : valeur de la mise demandée par le miseur
     @arg mise : la mise présente sur la table
     @return true si la mise a pu être effectuée et false sinon
  *)
  if joueurs.(ind_miseur).jetons - mise_demande < 0 then false
  else begin
    joueurs.(ind_miseur).jetons <- joueurs.(ind_miseur).jetons - mise_demande;
    mise := !mise + mise_demande;
    true
  end
;;


let rec misePossibleIa joueurs mise_souhaite grosseBlind mise =
  (**
     Fonction qui renvoie une mise possible pour l'ia en partant d'une mise de depart :
     l'ia demande à faire une mise de depart. Si cette mise est supérieure à ses jetons,
     cette fonction diminue la mise de la moitié jusqu'à atteindre une mise possible
     @param joueurs : tableau des deux joueurs
     @param mise_souhaite : la mise de depart
     @param grosseBlind : la grosse blind en jeu
     @param mise : la mise présente sur la table
     @return la mise de l'ia
  *)
  if joueurs.(1).jetons < grosseBlind
  then begin
    let res = joueurs.(1).jetons in
    mise := !mise + res;
    joueurs.(1).jetons <- 0;
    res
  end
  else if comptabilise joueurs 1 mise_souhaite mise
  then mise_souhaite
  else misePossibleIa joueurs (mise_souhaite/2) grosseBlind mise
;;


let blind joueurs ind_donneur mise =
  (**
     Exécute et déroule le tour de mise blind en demandant à chaque joueur de s'acquiter de sa blind :
     petite et grosse
     @param joueurs : le tableau de joueurs
     @param ind_donneur : l'indice du donneur
     @param mise : la mise sur la table
     @return la grosse blind et si allin y a
  *)
  let rec misePetiteBlind () =
    (**
       Demande au joueur devant faire la petite blind de s'acquiter de sa blind
    *)
    if ind_donneur = 0 (*C'est le joueur qui mise*)
    then
      begin
	try
	  print_string "Vous êtes la petite blind : veuillez faire votre mise obligatoire :"; print_newline ();
	  print_string ">Votre mise : ";
	  let miseJoueur = read_int () in
	  if not (comptabilise joueurs 0 miseJoueur mise)
	  then (print_string "Vous n'avez pas assez de jetons pour effectuer cete mise\n"; misePetiteBlind ())
	  else joueurs.(0).jetons <= 0 || joueurs.(1).jetons <= 0
	with
	  Failure s -> (print_string ("Probleme : "^s^"!\n"); misePetiteBlind ())
      end
    else
      begin
	let miseIa = misePossibleIa joueurs 500 1 mise in
	print_string "\nVotre adversaire est la petite blind.\nSa mise est de ";
	print_int miseIa; print_newline ();
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	joueurs.(0).jetons <= 0 || joueurs.(1).jetons <= 0
      end
  in
  
  let rec miseGrosseBlind () =
    (**
       Demande au joueur devant faire la grosse blind de s'acquiter de sa blind
       @return la grosse blind et si allin y a    
    *)
    if ind_donneur = 1 (*C'est le joueur qui mise*)
    then begin
      try
	if joueurs.(0).jetons < (!mise)*2
	then begin
	  print_string ("Vous êtes la grosse blind. Mais vous n'avez pas assez de jetons pour vous acquitter de cette blind.\n"^
			"Vous faites donc une \"All-in\"\n");
	  let m = joueurs.(0).jetons in
	  mise := !mise + m;
	  joueurs.(0).jetons <- 0;
	  m, true
	end
	else begin
	  print_string "Vous êtes la grosse blind : veuillez faire votre mise obligatoire :\n";
	  print_string "Petite blind : "; print_int !mise; print_newline ();
	  print_string ">Votre mise : ";
	  let miseJoueur = ref (read_int ()) in
	  if !miseJoueur < !mise * 2
	  then begin
	    print_string "Grosse blind insuffisante. Elle doit être au moins le double de la petite blind\n";
	    miseGrosseBlind ()
	  end
	  else if not (comptabilise joueurs 0 !miseJoueur mise)
	  then (print_string "Vous n'avez pas assez de jetons pour effectuer cete mise\n"; miseGrosseBlind ())
	  else !miseJoueur, joueurs.(0).jetons <= 0 || joueurs.(1).jetons <= 0
	end
      with
	Failure s -> (print_string ("Probleme : "^s^"!\n"); miseGrosseBlind ())
    end
    else
      begin
	let allinIa = ref false
	and miseIa = misePossibleIa joueurs (!mise*2) 0 mise in
	print_string "\nVotre adversaire est la grosse blind.\nSa mise est de ";
	print_int miseIa; print_newline ();
	if joueurs.(1).jetons <= 0 then (print_string "Il fait une \"All-in\"\n"; allinIa := true);
	miseIa, !allinIa
      end
  in
  let pb = misePetiteBlind ()
  and gb = miseGrosseBlind () in
  fst gb, (pb || snd gb)
;;

let rec miseJoueur joueurs grosseBlind mise mise_prec =
  (**
     Tour de mise du joueur
     @param joueurs : le tableau de joueurs
     @param grosseBlind : la grosse blind
     @param mise : la mise sur la table
     @param mise_prec : valeur de la mise précédente
     @return -1 si le joueur se couche
              0 s'il suit la mise de son adversaire
              la valeur de sa mise sinon
  *)
  try
    print_string "Votre tour de mise : Apuuyer :\n";
    print_string "-1- Se coucher / To fold\n";
    if mise_prec = (-1) (*Le joueur est le premier miseur ou l'ia a checker*)
    then print_string "-2- Miser\n-3- Checker / Parler\n"
    else (*l'ia a misé*)
      print_string "-2- Suivre / To call\n-3- Relancer / To raise\n";
    print_string "\n>Votre choix : ";
    let choix = read_int () in
    if choix = 1 then -1 (*Le joueur se couche*)
    else if (choix = 3 && mise_prec = (-1)) then 0 (*Le joueur checke*)
    else if joueurs.(0).jetons < grosseBlind
    then begin
      print_string ("Vous n'avez pas assez de jetons pour une mise à la hauteur de la grosse blind\n"^
		    "Vous faites donc une \"All-in\"\n");
      let m = joueurs.(0).jetons in
      mise := !mise + m;
      joueurs.(0).jetons <- 0;
      0
    end
    else if (choix = 2 && mise_prec = (-1))
    then
      begin
	print_string ">Votre mise : ";
	let m = ref (read_int ()) in
	if !m < grosseBlind
	then begin
	  print_string "Mise insuffiasante : votre mise doit valoir au moins la grosse blind qui est de ";
	  print_int grosseBlind; print_newline ();
	  miseJoueur joueurs grosseBlind mise mise_prec
	end
	else if not (comptabilise joueurs 0 !m mise)
	then (print_string "Vous n'avez pas assez de jetons pour effectuer cete mise\n"; miseJoueur joueurs grosseBlind mise mise_prec)
	else !m
      end
    else if (choix = 2 && mise_prec != (-1))
    then
      begin
	if comptabilise joueurs 0 mise_prec mise
	then (print_string "Vous misez "; print_int mise_prec; print_newline ())
	else begin
	  let tmp = joueurs.(0).jetons in
	  mise := !mise + tmp;
	  joueurs.(0).jetons <- 0;
	  print_string "Vous faites une \"All-in\"\n";
	end;
	0
      end
    else if (choix = 3 && mise_prec != (-1))
    then
      begin
	print_string ">Votre mise : ";
	let m = read_int () in
	if m <= mise_prec
	then (print_string "Renchérissement insuffisant !\n"; miseJoueur joueurs grosseBlind mise mise_prec)
	else if not (comptabilise joueurs 0 m mise)
	then (print_string "Vous n'avez pas assez de jetons pour effectuer cete mise\n"; miseJoueur joueurs grosseBlind mise mise_prec)
	else m
      end
    else (print_string "Choix indisponible !\n"; miseJoueur joueurs grosseBlind mise mise_prec);
  with
    Failure s -> (print_string ("Probleme : "^s^"!\n"); miseJoueur joueurs grosseBlind mise mise_prec)
;;


let miseIa preflop joueurs grosseBlind mise mise_prec ia =
  (**
     Tour de mise de l'ia
     @param preflop : boolean disant si on est au preflop ou non
     @param joueurs : le tableau de joueurs
     @param grosseBlind : la grosse blind
     @param mise : la mise sur la table
     @param mise_prec : valeur de la mise précédente
     @param ia : le type d'ia en jeu
     @return  1 si l'ia se couche
              0 s'il suit la mise du joueur
              la valeur de sa mise sinon
  *)

  let miseIaFacile () =
    (**
       Mise de l'ia débutante
    *)
    if preflop
    (*On est au pre-flop, proba non calculé*)
    then begin
      if (List.hd joueurs.(1).gives).color = (List.nth joueurs.(1).gives 1).color
      || Pervasives.abs ((getValeur (List.hd joueurs.(1).gives).rank) - (getValeur (List.nth joueurs.(1).gives 1).rank)) = 1
	|| ((getValeur (List.hd joueurs.(1).gives).rank) >= 10 && (getValeur (List.nth joueurs.(1).gives 1).rank) >= 10)
      then begin
	let m = ref 0 in
	if mise_prec = 0 || mise_prec = (-1)
	(*L'ia est le premier à miser*)
	then begin
	  m := misePossibleIa joueurs (grosseBlind + (joueurs.(1).jetons-grosseBlind)/5) grosseBlind mise;
	  print_string "Votre adversaire fait une mise de "
	end
	else begin (*Le joueur a misé*)
	  m := misePossibleIa joueurs (mise_prec + (joueurs.(1).jetons-mise_prec)/5) grosseBlind mise;
	  print_string "Votre adversaire relance de "
	end;
	print_int !m; print_newline ();
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	!m
      end
	
      else
	begin
	  if mise_prec = 0 || mise_prec = (-1)
	  then begin
	    let m = misePossibleIa joueurs grosseBlind grosseBlind mise in
	    print_string "Votre adversaire fait une mise de "; print_int m; print_newline ();
	    if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	    m
	  end
	  else begin
	    print_string "Votre adversaire suit\n";
	    let m = misePossibleIa joueurs mise_prec grosseBlind mise in
	    if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	    m-m
	  end
	end
    end
      
    (*Probabilité de gagner de l'ia < 50% : *)
    else if joueurs.(1).proba_victoire < 0.5
    then begin
      (*Dans ce cas, l'ia checke si le joueur précédent a checké*)
      if mise_prec = 0 then (print_string "Votre adversiare checke\n"; 0)
      (*Et se couche dans tout autre cas*)
      else (print_string "Votre adversaire se couche\n"; (-1))
    end
    else begin
      let m = ref 0 in (*l'ia mise*)
      (*Probabilité de gagner de l'ia compris entre 50% et 60% : *)
      if joueurs.(1).proba_victoire > 0.5 && joueurs.(1).proba_victoire < 0.6
      (*Dans ce cas, l'ia misera le quart de ses jetons + la blind si le joueur prec a checké ou relancera du 1/4 sinon.*)
      then begin
	if mise_prec = 0 || mise_prec = (-1)
	then begin
	  m :=  misePossibleIa joueurs (grosseBlind + (joueurs.(1).jetons-grosseBlind)/4) grosseBlind mise;
	  print_string "Votre adversaire fait une mise de "; print_int !m; print_newline ();
	  if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	  !m
	end
	else begin
	  print_string "Votre adversaire suit\n";
	  let m = misePossibleIa joueurs mise_prec grosseBlind mise in
	  if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	  m-m
	end
      end
	
      (*Probabilité de gagner de l'ia compris > 60% : *)
      else begin
      (*Dans ce cas, l'ia misera la moitié de ses jetons + la blind si le joueur prec a checké ou relancera de la moitié sinon.*)
	if mise_prec = 0 || mise_prec = (-1)
	then begin
	  m := misePossibleIa joueurs (grosseBlind + (joueurs.(1).jetons-grosseBlind)/2) grosseBlind mise;
	  print_string "Votre adversaire fais une mise de ";
	end
	else begin
	  m := misePossibleIa joueurs (mise_prec + (joueurs.(1).jetons-mise_prec)/2) grosseBlind mise;
	  print_string "Votre adversaire relance de ";
	end;
	print_int !m; print_newline ();
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	!m
      end
    end
  in

  let miseIaBluffeur () =
    (**
       Mise de l'ia bluffeur
    *)
    if preflop
    (*On est au pre-flop, proba non calculé*)
    then begin
      let m = ref 0 in
      if mise_prec = 0 || mise_prec = (-1)
      (*L'ia est le premier à miser*)
      then begin
	m := misePossibleIa joueurs (grosseBlind + ((joueurs.(1).jetons-grosseBlind)/5)) grosseBlind mise;
	print_string "Votre adversaire fait une mise de "
      end
      else begin (*Le joueur a misé*)
	m := misePossibleIa joueurs (mise_prec + ((joueurs.(1).jetons-grosseBlind)/5)) grosseBlind mise;
	print_string "Votre adversaire relance de "
      end;
      print_int !m; print_newline ();
      if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
      !m
    end
      
    else if joueurs.(1).proba_victoire <= 0.1
    then begin
      (*Dans ce cas, l'ia checke si le joueur précédent a checké*)
      if mise_prec = 0 then (print_string "Votre adversiare checke\n"; 0)
      (*Et se couche dans tout autre cas*)
      else (print_string "Votre adversaire se couche\n"; (-1))
    end
    else if joueurs.(1).proba_victoire < 0.4 (*compris entre 0.1 et 0.4*)
    then begin
      let m = ref 0 in
      if mise_prec = 0 || mise_prec = (-1)
      then begin
	m :=  misePossibleIa joueurs (grosseBlind + ((joueurs.(1).jetons-grosseBlind)/4)) grosseBlind mise;
	print_string "Votre adversaire fait une mise de "; print_int !m; print_newline ();
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	!m
	end
      else begin
	print_string "Votre adversaire suit\n";
	m := misePossibleIa joueurs mise_prec grosseBlind mise;
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	0
      end
    end
    else begin (*proba >= 0.4*)
      let m = ref 0 in
      if joueurs.(1).proba_victoire < 0.6
      then m := misePossibleIa joueurs (grosseBlind + ((joueurs.(1).jetons-grosseBlind)/2)) grosseBlind mise
      else m :=  misePossibleIa joueurs joueurs.(1).jetons grosseBlind mise;
      if mise_prec = 0 || mise_prec = (-1)
      then (print_string "Votre adversaire fait une mise de "; print_int !m; print_newline ())
      else (print_string "Votre adversaire relance de "; print_int !m; print_newline ());
      if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
      !m
    end
  in

  let miseIaPro () =
    (**
       Mise de l'ia pro
    *)
    if preflop
    then begin
      if mise_prec = 0 || mise_prec = (-1)
      then begin
	let m = misePossibleIa joueurs grosseBlind grosseBlind mise in
	print_string "Votre adversaire fait une mise de ";
	print_int m; print_newline ();
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	m
      end
      else if mise_prec >= ((joueurs.(0).jetons+mise_prec)/3)
      then (print_string "Votre adversaire se couche\n"; (-1))
      else (print_string "Votre adversaire suit\n"; 0)
    end
      
    else if joueurs.(1).proba_victoire < 0.2
    then begin
      if mise_prec = (-1) || mise_prec = 0
      then (print_string "Votre adversaire checke\n"; 0)
      else if mise_prec <= grosseBlind+1000
      then begin (*Petit coup de bleuf*)
	let m = misePossibleIa joueurs (mise_prec*2) grosseBlind mise in
	print_string ("Votre adversaire relance de "^(string_of_int m)^"\n");
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	m
      end
      else (print_string "Votre adversaire se couche\n"; (-1))
    end
    else if joueurs.(1).proba_victoire < 0.4 (*compris entre 0.2 et 0.4*)
    then begin
      if mise_prec = (-1) || mise_prec = 0
      then (print_string "Votre adversaire checke\n"; 0)
      else if mise_prec >= (joueurs.(1).jetons+mise_prec)/2
      then (print_string "Votre adversaire se couche\n"; (-1))
      else begin
	let m = misePossibleIa joueurs mise_prec grosseBlind mise in
	print_string "Votre adversaire suit\n";
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	m-m (*0*)
      end
    end
    else if joueurs.(1).proba_victoire < 0.6 (*compris entre 0.4 et 0.6*)
    then begin
      if mise_prec = 0 || mise_prec = (-1)
      then (print_string "Votre adversaire checke\n"; 0)
      else begin
	let m = misePossibleIa joueurs mise_prec grosseBlind mise in
	print_string "Votre adversaire suit\n";
	if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
	m-m (*0*)
      end
    end
    else if joueurs.(1).proba_victoire < 0.8 (*compris entre 0.6 et 0.8*)
    then begin
      let m = ref 0 in
      if mise_prec = 0 || mise_prec = (-1)
      then begin
	m := misePossibleIa joueurs (grosseBlind + (joueurs.(1).jetons-grosseBlind)/2) grosseBlind mise;
	print_string "Votre adversaire mise ";
      end
      else begin
	m := misePossibleIa joueurs (mise_prec + (joueurs.(1).jetons-mise_prec)/2) grosseBlind mise;
	print_string "Votre adversaire relance de  ";
      end;
      print_int !m; print_newline ();
      if joueurs.(1).jetons <= 0 then print_string "Il fait une \"All-in\"\n";
      !m
    end
    else begin(* proba > 0.8 *)
      let m = misePossibleIa joueurs joueurs.(1).jetons grosseBlind mise in
      if mise_prec = 0 || mise_prec = (-1)
      then print_string "Votre adversaire mise "
      else print_string "Votre adversaire relance de ";
      print_int m; print_newline ();
      print_string "Il fait une \"All-in\"\n";
      m
    end

  in
  if ia = 1 then miseIaFacile ()
  else if ia = 2 then miseIaBluffeur ()
  else miseIaPro ()
;;

let rec tour preflop joueurs miseur grosseBlind mise mise_prec ia =
  (**
     Un tour de mise
     @param preflop : boolean disant si on est au preflop ou non
     @param joueurs : le tableau de joueurs
     @param miseur : le miseur
     @param grosseBlind : la grosse blind
     @param mise : la mise sur la table
     @param mise_prec : valeur de la mise précédente
     @param ia : le type d'ia en jeu
     @return un entier qui vaut
                               1 si l'ia se couche
                               -1 si le joueur se couche
                               0 s'il se sont entendu sur une mise
             et un boolean disant si allin y a ou non
  *)
  if miseur = 0
  then
    begin
      let resJ = miseJoueur joueurs grosseBlind mise mise_prec in
      if resJ = (-1) then (-1), false
      else if (resJ = 0 && mise_prec = (-1)) || resJ != 0
      then tour preflop joueurs 1 grosseBlind mise resJ ia
      else begin
	print_string "\n\t\t|Valeur de la mise sur la ta table : ";
	print_int !mise; print_string "|\n";
	0, joueurs.(0).jetons <= 0 || joueurs.(1).jetons <= 0
      end
    end
  else
    begin
      let resIa = miseIa preflop joueurs grosseBlind mise mise_prec ia in
      if resIa = (-1) then 1, false
      else if (resIa = 0 && mise_prec = (-1)) || resIa != 0
      then tour preflop joueurs 0 grosseBlind mise resIa ia
      else begin
	print_string "\n\t\t|Valeur de la mise sur la ta table : ";
	print_int !mise; print_string "|\n";
	0, joueurs.(0).jetons <= 0 || joueurs.(1).jetons <= 0
      end
    end
;;


let miseAJourProbas joueurs (t:table) =
  (**
     Calcule la nouvelle probabilité de chaque joueur
     @param le tableau des joueurs
     @param t : la table
  *)
  joueurs.(0).proba_victoire <- proba_simple joueurs.(0).gives t;
  joueurs.(1).proba_victoire <- proba_simple joueurs.(1).gives t;
;;

let showdown joueurs (t:table) =
  (**
     Déroule la phase showdown du jeu
     @param le tableau des joueurs
     @param la table
     @return le vainqueur si vainqueur y a ou 0 en cas d'égalité
  *)
  print_string ("\n------------\n"^
                   "| Showdown |\n"^
                   "------------\n\n");
  print_string "LA TABLE :\n";
  affiche_carte_list t;
  print_string "\nVOTRE DONNE :\n";
  affiche_carte_list joueurs.(0).gives;
  print_string "\nDONNE DE VOTRE ADVERSAIRE :\n";
  affiche_carte_list joueurs.(1).gives;
  let probas = proba_double joueurs.(0).gives joueurs.(1).gives t in
  joueurs.(0).proba_victoire <- fst probas;
  joueurs.(1).proba_victoire <- snd probas;
  if joueurs.(0).proba_victoire = 1.
  then 1
  else if joueurs.(1).proba_victoire = 1.
  then -1
  else 0
;;

let coup joueurs ind_donneur mise ia =
  (**
     Déroule un coup de jeu, de la blind au showdown si aucun joueur ne se couche jusque là
     @param joueurs : la table de joueurs
     @param ind_donneur : l'indice du donneur
     @param mise : la mise sur la table
     @param ia : le type de l'ia
     @return le vainqueur du coup
  *)
  print_string ("\n------------\n"^
                  "| Le blind |\n"^
                  "------------\n\n");
  let grosseBlind, allin_b = blind joueurs ind_donneur mise
  and paquet = ref (cree_paquet52cartes ()) in
  joueurs.(0).gives <- croupier 1 paquet;
  joueurs.(1).gives <- croupier 1 paquet;
  joueurs.(0).gives <- (List.hd (croupier 1 paquet))::joueurs.(0).gives;
  joueurs.(1).gives <- (List.hd (croupier 1 paquet))::joueurs.(1).gives;
  print_string ("\n--------------\n"^
                  "| Pré - flop |\n"^
                  "--------------\n\n");
  afficheJoueur true joueurs.(0);
  let pre_flop = ref (0, allin_b) in
  if not(allin_b)
  then pre_flop := tour true joueurs (getNonDonneur joueurs) grosseBlind mise (-1) ia;
  if fst !pre_flop != 0 then fst !pre_flop
  else
    begin
      let table = ref (croupier 3 paquet) in
      miseAJourProbas joueurs !table;
      print_string ("\n--------\n"^
                      "| Flop |\n"^
                      "--------\n\n");
      print_string "LA TABLE :\n";
      affiche_carte_list !table;
      afficheJoueur false joueurs.(0);
      let flop = ref (0, snd !pre_flop) in
      if not(snd !pre_flop)
      then flop := tour false joueurs (getNonDonneur joueurs) grosseBlind mise (-1) ia;
      if fst !flop != 0 then fst !flop
      else
	begin
	  table := List.hd (croupier 1 paquet)::(!table);
	  miseAJourProbas joueurs !table;
	  print_string ("\n--------\n"^
                          "| Turn |\n"^
                          "--------\n\n");
	  print_string "LA TABLE :\n";
	  affiche_carte_list !table;
	  afficheJoueur false joueurs.(0);
	  let turn = ref (0, snd !flop) in
	  if not(snd !flop)
	  then turn := tour false joueurs (getNonDonneur joueurs) grosseBlind mise (-1) ia;
	  if fst !turn != 0 then fst !turn
	  else
	    begin
	      table := List.hd (croupier 1 paquet)::(!table);
	      miseAJourProbas joueurs !table;
	      print_string ("\n--------\n"^
                              "| Rver |\n"^
                              "--------\n\n");
	      print_string "LA TABLE :\n";
	      affiche_carte_list !table;
	      afficheJoueur false joueurs.(0);
	      let river = ref (0, snd !turn) in
	      if not(snd !turn)
	      then river := tour false joueurs (getNonDonneur joueurs) grosseBlind mise (-1) ia;
	      if fst !river != 0 then fst !river
	      else
		showdown joueurs !table
	    end
	end
    end
;;


let rec jeu i joueurs donneur ia =
  (**
     Exécute le jeu de Texas hold'em jusqu'à la victoire d'un joueur
     @param le i-ème coup
     @param le tableau de joeurs
     @param l'indice du donneur
     @param le type de l'ia
  *)
  if gagne joueurs
  then (print_string "Félicitations, vous avez remportez la partie !!!\n";
	print_string "Attention, jouer comprend des risques. Pour vous faire aider, appelez le ...\n")
  else if perd joueurs
  then (print_string "Dommage, vous avez perdu !!!\n";
	print_string "Attention, jouer comprend des risques. Pour vous faire aider, appelez le ...\n")
  else
    begin
      print_string ("\n<<<<<<<<<< COUP "^(string_of_int i)^" >>>>>>>>>>\n");
      print_string ("\n-------------------------------------\n"^
		    "| Vos jetons : "^(string_of_int joueurs.(0).jetons)^
		    "\n-------------------------------------\n"^
		    "| Jetons de l'adversaire : "^(string_of_int joueurs.(1).jetons)^
		    "\n-------------------------------------\n\n");
      let mise = ref 0
      and next_dealer = getNonDonneur joueurs in
      let gagnant = coup joueurs donneur mise ia in
      if gagnant = 1 (*le joueur gagne*)
      then (print_string "Vous remportez ce coup\n"; joueurs.(0).jetons <- joueurs.(0).jetons + !mise)
      else if gagnant = (-1) (*l'ia gagne*)
      then (print_string "Votre adversaire remporte ce coup\n";joueurs.(1).jetons <- joueurs.(1).jetons + !mise)
      else (*egailite*)
	begin
	  print_string "Egalité ! Partage de la mise\n";
	  joueurs.(0).jetons <- joueurs.(0).jetons + (!mise/2);
	  joueurs.(1).jetons <- joueurs.(1).jetons + (!mise/2)
	end;
      if donneur = 0
      then
	begin
	  joueurs.(0).donneur <- false;
	  joueurs.(1).donneur <- true
	end
      else
	begin
	  joueurs.(0).donneur <- true;
	  joueurs.(1).donneur <- false
	end;
      jeu (i+1) joueurs next_dealer ia
    end
;;
  
let rec texas_holdem () =
  try
    let joueurs = [|{gives = []; jetons = 50000; proba_victoire = 0.; donneur = true}; (*le joueur*)
		    {gives = []; jetons = 50000; proba_victoire = 0.; donneur = false}|] (*l'ia*)
    in
    print_string "\n\nBienvenue dans le jeu de texas hold'em textuel basique\n";
    print_string "\n\nVous allez affronter une intelligence artificielle\nVous avez le choix entre 3 types d'adversaires :\n";
    print_string "\n-1- Amy Pond : Une adversaire débutante, serrée passive qui joue la prudence\n";
    print_string "-2- Coca Cappuccino : Un adversaire bluffeur\n";
    print_string "-3- Nova Savov : Une adversaire professionnelle, pour qui les mathématiques n'ont plus de secret\n";
    print_string "\n>Votre choix : ";
    let choix = read_int () in
    if choix < 1 || choix > 3
    then (print_string "Choix indisponible !!!\n"; texas_holdem ())
    else begin
      if choix = 1
      then print_string ("-----------------------------------------------------------\n"^
			 "| Votre adversaire                                        |\n"^
			 "-----------------------------------------------------------\n"^
			 "| Vous allez affronter Amy POND,                          |\n"^
			 "| Une adversaire débutante qui fait ses débuts dans le    |\n"^
			 "| jeu. Elle est facilement battable car elle joue plus    |\n"^
			 "| la prudence donc se couche rapidement. Cependant, peu   |\n"^
			 "| expérimentée, elle n'est pas réceptive au bluff.        |\n"^
			 "-----------------------------------------------------------\n\n")
      else if choix = 2
      then  print_string ("-----------------------------------------------------------\n"^
			  "| Votre adversaire                                        |\n"^
			  "-----------------------------------------------------------\n"^
			  "| Vous allez affronter Coca CAPPUCCINO,                   |\n"^
			  "| Un adversaire bluffeur pour qui le bluff n'a plus de    |\n"^
			  "| secret. Son jeu tout entier est basé sur le bluf.       |\n"^
			  "| A vous donc de savoir détecter quand il bluf ou quand   |\n"^
			  "| il ne le fait pas.                                      |\n"^
			  "-----------------------------------------------------------\n\n")
      else print_string ("-----------------------------------------------------------\n"^
			  "| Votre adversaire                                        |\n"^
			  "-----------------------------------------------------------\n"^
			  "| Vous allez affronter Nova SAVOV,                        |\n"^
			  "| Une adversaire professionnelle et coriace qui maitrise  |\n"^
			  "| presque toutes les astuces du Texas hold'em.            |\n"^
			  "| Sachant \"compter les cartes\", son jeu est plus basé     |\n"^
			  "| sur les mathétiques.                                    |\n"^
			  "-----------------------------------------------------------\n\n");
      print_string (" -1- Continuer                     -Autres chiffres- Sortir\n"^
		    ">Votre choix : ");
      let r = read_int () in
      if r = 1
      then (print_string "\nDebut du jeu...\n\n"; jeu 1 joueurs 0 choix)
    end
  with
    Failure s -> (print_string ("Probleme : "^s^"!\n"); texas_holdem ())
;;


texas_holdem ();;
