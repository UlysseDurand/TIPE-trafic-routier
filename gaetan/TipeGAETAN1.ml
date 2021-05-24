(* Les modeles utilises pour decrire
l'arrete, on peut aussi mettre des
arretes de different modele *)
type cap = Cap of float * float;;
type lin = Lin of float * float;;
type capLin = CapLin of float * float * float;;
type capInv = CapInv of float * float * float;;

(* Structure de donnee representant un graphe
 - arretes : chaque element contient un point de
 			depart, un point d'arrivee, et un
			modele
 - points : points.(i) contient la liste des
 			elements de arretes partant de celui-ci *)
 			
(* structure *)

(*
type 'a graphe = {
	nombre_points : int ;
	points : (int * int) list array;
	arretes : (int * int * 'a) array;
};;
*)

type arrete = { depart : int ; arrivee : int ; tempsMin : float ; acroissement : float};;

type graphe = {
	nombre_points : int ;
	points : int list array;
	arretes : arrete array;
};;

type chemin = (int * int) list;;

(* djikstra classique *)

let rec ajoute c t acc = match acc with
	| [] -> [(c,t)]
	| (x,y)::a when t < y -> (c,t)::acc
	| (x,y)::a -> (x,y)::ajoute c t a;;
	
let rec ajouteralaccumulateur g c tc arretes acc visiter = match arretes with
	| [] -> acc;
	| (arrete)::a when visiter.(g.arretes.(arrete).arrivee) -> ajouteralaccumulateur g c tc a (ajoute ((g.arretes.(arrete).arrivee,arrete)::c) (tc +. g.arretes.(arrete).tempsMin) acc) visiter
	| (arrete)::a -> ajouteralaccumulateur g c tc a acc visiter;; 

let rec djikstra_aux g b acc visiter = match acc with
		|[]->failwith "plus de chemin ou pas de chemin"
		|(c,tc)::cs -> let (dernier,_) ::_ = c in if dernier = b then (c,tc) else (visiter.(dernier) <- false ; (djikstra_aux g b (ajouteralaccumulateur g c tc g.points.(dernier) cs visiter) visiter)) ;;

let djikstra g a b = djikstra_aux g b [([a,-1],0.)] (Array.make 3 true);; 

(* djikstra prepetitif lazy *)

let rec ajoute_lazy c t visiter acc = match acc with
	| [] -> [(c,t,visiter)]
	| (x,y,_)::a when t < y -> (c,t,visiter)::acc
	| (x,y,v)::a -> (x,y,v)::(ajoute_lazy c t visiter a);;

let rec ajouteralaccumulateur_lazy g c tc arretes acc visiter = match arretes with
	| [] -> ();
	| (arrete)::a when visiter.(g.arretes.(arrete).arrivee) -> acc := ajoute_lazy ((g.arretes.(arrete).arrivee,arrete)::c) (tc +. g.arretes.(arrete).tempsMin) visiter !acc ; ajouteralaccumulateur_lazy g c tc a acc visiter
	| (arrete)::a -> ajouteralaccumulateur_lazy g c tc a acc visiter;; 

let rec djikstra_lazy g b acc = match !acc with
		|[]->failwith "plus de chemin ou pas de chemin"
		|(c,tc,visiter)::cs -> let (dernier,_) ::_ = c in if dernier = b then( acc := cs ; (c,tc)) else (visiter.(dernier) <- false ; acc := cs; ajouteralaccumulateur_lazy g c tc g.points.(dernier) acc visiter ; djikstra_lazy g b acc) ;;
		
let creer_acc g a = ref ([([a,-1],0.,Array.make g.nombre_points true)]);;

(* repartition *)

let rec intrique g sys n arrete liste intrication = match liste with 
	| [] -> intrication.(arrete) <- n::intrication.(arrete)
	| x::l -> sys.(x).(n + 1) <- sys.(x).(n + 1) +. g.arretes.(arrete).acroissement; sys.(n).(x + 1) <- sys.(n).(x + 1) +. g.arretes.(arrete).acroissement;;

let rec ajouterchemin g sys n nouveau_chemins intrication = match nouveau_chemins with 
	| [] -> sys
	| (_,-1)::nc -> sys
	| (x, a)::nc -> sys.(n).(1) <- g.arretes.(a).tempsMin ; intrique g sys n a intrication.(a) intrication ; ajouterchemin g sys n nc intrication;;
	
let resoud sys = (* TODO *);;
	
let rec reparti_aux g b acc derniereRepartition dernierSys intrication n dernierChemin  = 
	let sys = (* TODO *) in ajouter g sys n dernierChemin intrication; 
	let (repartition,temps) = resoud sys in 
	let (nouveauChemin,tempsNouveauChemin) = djikstra_lazy g b acc in 
		if temps < tempsNouveauChemin
			then repartition
			else reparti_aux g b acc repartition sys intrication (n+1) chemin;;

(* test *)

let a dp ar t ac = { depart = dp ; arrivee = ar ; tempsMin = t ; acroissement = ac};;

let test_graphe = {nombre_points = 3 ; points = [| [0;1] ; [2;3] ; [4;5] |] ; arretes = [| a 0 1 4. 0. ; a 0 2 8. 0. ; a 1 0 4. 0. ; a 1 2 16. 0. ; a 2 0 8. 0.  ; a 2 1 16. 0. |]};;

let acc = creer_acc test_graphe 0;;

djikstra_lazy test_graphe 2 acc ;;
!acc;;

djikstra test_graphe 0 2;;
