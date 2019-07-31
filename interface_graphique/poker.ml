open Cartes
open Combs
open CalculetteProbaPoker
open LectureFichier

let mode = ref 0
let j1c = ref ""
let j2c = ref "?"
let tbc = ref ""
let choice = ref "joueur1"
let j1=ref true
let j2= ref true 
let tb= ref true
let final = ref ""


(**************************Cards function*********************************)
let xpm_label_box ~file  ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and label and pack *)
  let box = GPack.hbox ~width:29 ~height:43  ~packing () in

  (* Now on to the image stuff and pack into box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  GMisc.pixmap pixmap ~width:29 ~height:43 ~packing:(box#pack) ()
(*   Create a label for the button and pack into box*)
 
(*************************************************************************)
  
(*************************************************************************)


  let action_on_card but f c ()= 
  match !choice with
   |"joueur1"->(match !mode with
                | 0 -> j1c:=!j1c^c^" "      ;mode:=!mode+1;f#move but#coerce  ~x:200 ~y:50 ;();
                | 1 ->j1c:=!j1c^c;mode:=!mode+1;f#move but#coerce  ~x:270 ~y:50  ;();
                |_->();)
   |"joueur2"-> (match !mode with
                | 2|6|7  ->j2c:=""^c^" " ;mode:=!mode+1;f#move but#coerce  ~x:200 ~y:450;mode:=8;();
                | 8  ->j2c:=!j2c^c ;mode:=!mode+1;f#move but#coerce  ~x:270 ~y:450;();
                |_->();)
   |"table"-> (match !mode with             

                | 2|9 ->tbc:=!tbc^c^" ";mode:=!mode+1;f#move but#coerce  ~x:120 ~y:250 ;mode:=3;();
                | 3 ->tbc:=!tbc^c^" ";mode:=!mode+1;f#move but#coerce  ~x:200 ~y:250 ;();
                | 4 ->tbc:=!tbc^c;mode:=!mode+1;f#move but#coerce  ~x:270 ~y:250;();
                | 5 ->tbc:=!tbc^" "^c;mode:=!mode+1;f#move but#coerce  ~x:350 ~y:250;();
                | 6 ->tbc:=!tbc^" "^c;mode:=!mode+1;f#move but#coerce  ~x:430 ~y:250;();
                |_->();)
   |_->()

let print_choice () = 
let str = (!j1c)^"\n"^(!j2c)^"\n"^(!tbc)
in
let canal_sortie = open_out "gui.txt" in 
output_string canal_sortie str;
flush canal_sortie
(*************************************************************************)
 (*compute button *)
  let cmpt  () =  
  print_choice ();
  let (a,b)=compute_gui "gui.txt" in 
    final:="Joueur 1 : "^(string_of_float !a)^"   "^"Joueur 2 : "^(string_of_float !b)

(*************************************************************************)

 

(*************************************************************************)
(* Create a Buttn Box with the specified parameters *)
let create_bbox direction  layout c t   =
  
let bbox = GPack.button_box direction  ~layout ~child_height:43 ~child_width:29  () in
 let carte_p = GButton.toggle_button ~relief:`NONE ~packing:bbox#add () in
 ( carte_p#connect#clicked ~callback:(action_on_card bbox t  c ));   
  xpm_label_box ~file: (c^".xpm")  ~packing:carte_p#add (); 
  bbox#coerce
(*************************************************************************)

let count = ref 0

let push_item context () =
  try
  cmpt();
  context#push ( Printf.sprintf "%s" !final);
  ()
  with
  | _ -> context#push ( Printf.sprintf "Err:Completez la configuration:1 ou 2 donnes,3,4 ou 5cartes sur table" ); failwith "Faudra finir la combinaison avant"
 

let pop_item context () =
  context#pop ();
  ()
 
 

let main () =

  (*************************************************************************)
  let window = GWindow.window ~title:"Poker Hold'em"  ~width:700 ~height:800 () in
  window#connect#destroy ~callback:GMain.Main.quit;
  let fixed = GPack.fixed ~width:700 ~height:800 ~packing:window#add () in
  let choice_box = GPack.hbox ~spacing:10 ~border_width:10 ~packing:(fixed#put ~x:500 ~y:0) () in
  let joueur1 = GButton.radio_button ~label:"J1" ~active:true ~packing:choice_box#add () in
      joueur1#connect#clicked ~callback:( fun () ->  choice:="joueur1");
  let joueur2 = GButton.radio_button ~group:joueur1#group ~label:"J2" ~packing:choice_box#add () in
                joueur2#connect#clicked ~callback:( fun () -> if !j2=true then choice:="joueur2";j2:=false);
  let table = GButton.radio_button
      ~group:joueur1#group ~label:"table" ~packing:choice_box#add () in
      table#connect#clicked ~callback:( fun () ->  if !tb=true then choice:="table";tb:=false);


  (***************************Cards creation***************************************)    
  (*p co t ca*)
  let ap = (create_bbox `HORIZONTAL `EDGE "Ap" fixed) in
  let aco = (create_bbox `HORIZONTAL `EDGE "Aco" fixed) in
  let at = (create_bbox `HORIZONTAL `EDGE "At" fixed) in
  let aca = (create_bbox `HORIZONTAL `EDGE "Aca" fixed) in
  let rp = (create_bbox `HORIZONTAL `EDGE "Rp" fixed) in
  let rco = (create_bbox `HORIZONTAL `EDGE "Rco" fixed) in
  let rt = (create_bbox `HORIZONTAL `EDGE "Rt" fixed) in
  let rca = (create_bbox `HORIZONTAL `EDGE "Rca" fixed) in
  let dp = (create_bbox `HORIZONTAL `EDGE "Dp" fixed) in
  let dco = (create_bbox `HORIZONTAL `EDGE "Dco" fixed) in
  let dt = (create_bbox `HORIZONTAL `EDGE "Dt" fixed) in
  let dca = (create_bbox `HORIZONTAL `EDGE "Dca" fixed) in
  let vp = (create_bbox `HORIZONTAL `EDGE "Vp" fixed) in
  let vco = (create_bbox `HORIZONTAL `EDGE "Vco" fixed) in
  let vt = (create_bbox `HORIZONTAL `EDGE "Vt" fixed) in
  let vca = (create_bbox `HORIZONTAL `EDGE "Vca" fixed) in
  let tenp = (create_bbox `HORIZONTAL `EDGE "10p" fixed) in
  let tenco = (create_bbox `HORIZONTAL `EDGE "10co" fixed) in
  let tent = (create_bbox `HORIZONTAL `EDGE "10t" fixed) in
  let tenca = (create_bbox `HORIZONTAL `EDGE "10ca" fixed) in
  let ninep = (create_bbox `HORIZONTAL `EDGE "9p" fixed) in
  let nineco = (create_bbox `HORIZONTAL `EDGE "9co" fixed) in
  let ninet = (create_bbox `HORIZONTAL `EDGE "9t" fixed) in
  let nineca = (create_bbox `HORIZONTAL `EDGE "9ca" fixed) in
  let eightp = (create_bbox `HORIZONTAL `EDGE "8p" fixed) in
  let eightco = (create_bbox `HORIZONTAL `EDGE "8co" fixed) in
  let eightt = (create_bbox `HORIZONTAL `EDGE "8t" fixed) in
  let eightca = (create_bbox `HORIZONTAL `EDGE "8ca" fixed) in
  let sevenp = (create_bbox `HORIZONTAL `EDGE "7p" fixed) in
  let sevenco = (create_bbox `HORIZONTAL `EDGE "7co" fixed) in
  let sevent = (create_bbox `HORIZONTAL `EDGE "7t" fixed) in
  let sevenca = (create_bbox `HORIZONTAL `EDGE "7ca" fixed) in
  let sixp = (create_bbox `HORIZONTAL `EDGE "6p" fixed) in
  let sixco = (create_bbox `HORIZONTAL `EDGE "6co" fixed) in
  let sixt = (create_bbox `HORIZONTAL `EDGE "6t" fixed) in
  let sixca = (create_bbox `HORIZONTAL `EDGE "6ca" fixed) in
  let fivep = (create_bbox `HORIZONTAL `EDGE "5p" fixed) in
  let fiveco = (create_bbox `HORIZONTAL `EDGE "5co" fixed) in
  let fivet = (create_bbox `HORIZONTAL `EDGE "5t" fixed) in
  let fiveca = (create_bbox `HORIZONTAL `EDGE "5ca" fixed) in
  let fourp = (create_bbox `HORIZONTAL `EDGE "4p" fixed) in
  let fourco = (create_bbox `HORIZONTAL `EDGE "4co" fixed) in
  let fourt = (create_bbox `HORIZONTAL `EDGE "4t" fixed) in
  let fourca = (create_bbox `HORIZONTAL `EDGE "4ca" fixed) in
  let threep = (create_bbox `HORIZONTAL `EDGE "3p" fixed) in
  let threeco = (create_bbox `HORIZONTAL `EDGE "3co" fixed) in
  let threet = (create_bbox `HORIZONTAL `EDGE "3t" fixed) in
  let threeca = (create_bbox `HORIZONTAL `EDGE "3ca" fixed) in
  let twop = (create_bbox `HORIZONTAL `EDGE "3p" fixed) in
  let twoco = (create_bbox `HORIZONTAL `EDGE "2co" fixed) in
  let twot = (create_bbox `HORIZONTAL `EDGE "2t" fixed) in
  let twoca = (create_bbox `HORIZONTAL `EDGE "2ca" fixed) in


(*p co t ca*)
(*************** putting each card in its position ****)
  fixed#put ap  ~x:500 ~y:50;
  fixed#put aco ~x:550 ~y:50;
  fixed#put at  ~x:600 ~y:50;
  fixed#put aca ~x:650 ~y:50;


  fixed#put rp ~x:500 ~y:100 ;
  fixed#put rco ~x:550 ~y:100;
  fixed#put rt  ~x:600 ~y:100;
  fixed#put rca ~x:650 ~y:100;
  

  fixed#put dp ~x:500 ~y:150 ;
  fixed#put dco ~x:550 ~y:150;
  fixed#put dt  ~x:600 ~y:150;
  fixed#put dca ~x:650 ~y:150;


  fixed#put vp ~x:500 ~y:200 ;
  fixed#put vco ~x:550 ~y:200;  
  fixed#put vt  ~x:600 ~y:200;
  fixed#put vca ~x:650 ~y:200;



  fixed#put tenp ~x:500 ~y:250 ;
  fixed#put tenco ~x:550 ~y:250;  
  fixed#put tent  ~x:600 ~y:250;
  fixed#put tenca ~x:650 ~y:250;





  fixed#put ninep ~x:500 ~y:300 ;
  fixed#put nineco ~x:550 ~y:300;  
  fixed#put ninet  ~x:600 ~y:300;
  fixed#put nineca ~x:650 ~y:300;



  fixed#put eightp ~x:500 ~y:350 ;
  fixed#put eightco ~x:550 ~y:350;  
  fixed#put eightt  ~x:600 ~y:350;
  fixed#put eightca ~x:650 ~y:350;



  fixed#put sevenp ~x:500 ~y:400 ;
  fixed#put sevenco ~x:550 ~y:400;  
  fixed#put sevent  ~x:600 ~y:400;
  fixed#put sevenca ~x:650 ~y:400;


  fixed#put sixp ~x:500 ~y:450 ;
  fixed#put sixco ~x:550 ~y:450;  
  fixed#put sixt  ~x:600 ~y:450;
  fixed#put sixca ~x:650 ~y:450;




  fixed#put fivep ~x:500 ~y:500 ;
  fixed#put fiveco ~x:550 ~y:500;  
  fixed#put fivet  ~x:600 ~y:500;
  fixed#put fiveca ~x:650 ~y:500;




  fixed#put fourp ~x:500 ~y:550 ;
  fixed#put fourco ~x:550 ~y:550;  
  fixed#put fourt  ~x:600 ~y:550;
  fixed#put fourca ~x:650 ~y:550;


  fixed#put threep ~x:500 ~y:600 ;
  fixed#put threeco ~x:550 ~y:600;  
  fixed#put threet  ~x:600 ~y:600;
  fixed#put threeca ~x:650 ~y:600;



  fixed#put twop ~x:500 ~y:650 ;
  fixed#put twoco ~x:550 ~y:650;  
  fixed#put twot  ~x:600 ~y:650;
  fixed#put twoca ~x:650 ~y:650;
  
  (*******************************************************************************)

  (**********************Maniputalitng frames,statubar and buttons**********************************)

  let frame_joueur1 = GBin.frame ~width:500 ~height:150  ~label:"Joueur 1" ~packing:(fixed#put ~x:1 ~y:0) ()in
  let frame_donne = GBin.frame ~label:"la table" ~width:500 ~height:200 ~packing:(fixed#put ~x:1 ~y:170) () in 
  let frame_joueur2 =GBin.frame ~label:"Joueur 2" ~width:500 ~height:150 ~packing:(fixed#put ~x:1 ~y:390) () in  
  let frame_res =GBin.frame ~label:"Resultat" ~width:500 ~height:150 ~packing:(fixed#put ~x:1 ~y:550) () in  
 
  let vbox = GPack.vbox ~spacing:10 ~packing:frame_res#add () in

  let statusbar = GMisc.statusbar ~packing:vbox#add () in
  let context = statusbar#new_context ~name:"Statusbar example" in
  (*******************************************************************************)
  let lbox = GPack.button_box `HORIZONTAL ~layout:`EDGE  ~spacing:10  ~child_height:43 ~child_width:29 ~packing:(vbox#add ) () in 
  let compute = GButton.button  ~label:"Compute" ~packing:(lbox#add ) () in
      compute#connect#clicked ~callback:(push_item context); 
      (* Create "Quit" button *)
  let quit = GButton.button ~label:"Quit" ~packing:(lbox#add ) () in
  quit#connect#clicked ~callback:GMain.Main.quit;



  

  
  
  (*******************************************************************************)

   window#show ();
  (* Enter the event loop *)
  GMain.Main.main ()

let _ = Printexc.print main ()


