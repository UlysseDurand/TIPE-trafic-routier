type arrete = { depart : int ; arrivee : int ; tempsMin : float ; acroissement : float};;

type graphe = {
	nombre_noeuds : int ;
	nombre_arretes : int ;
	noeuds : int list array;
	arretes : arrete array;
};;

type parcours = int list;;
type chemin = (parcours) * int ;; (*liste des arretes , dernier noeud*)

type fonction = float array;;(* une fonction de temps corespondant a un chemin*)
type repartition = float array;;(* tableau des débit sur chaque chemin*)

let rec ajoute_arretes_indirect i fonctions liste arrete = match liste with
	| [] -> ()
	| j::l -> (fonctions.(j).(i) <- fonctions.(j).(i) +. arrete.acroissement; fonctions.(i).(j) <- fonctions.(i).(j) +. arrete.acroissement; ajoute_arretes_indirect i fonctions l arrete);;

let rec ajoute_arretes_direct parcours i fonctions usages graphe n= match parcours with 
	| [] -> ()
	| a::p -> let arrete = graphe.arretes.(a) in (
		fonctions.(i).(n) <- fonctions.(i).(n) +. arrete.tempsMin;
		fonctions.(i).(i) <- fonctions.(i).(i) +. arrete.acroissement;
		ajoute_arretes_indirect i fonctions usages.(a) arrete;
		usages.(a) <- i::(usages.(a)) );;

let parcours_vers_fonctions parcours graphe n= 
	let fonctions = Array.make_matrix n (n+1) 0.0 in 
	let usages = Array.make graphe.nombre_arretes [] in 
	for i = 0 to n-1 do 
		ajoute_arretes_direct parcours.(i) i fonctions usages graphe n;
	done;fonctions;;

let calcule fonction repartition n = 
	let temps = ref fonction.(n) in 
	for i = 0 to n-1 do 
		temps := !temps +. fonction.(i) *. repartition.(i);
	done; !temps;;
	
let gradient fonction n = 
	let grad = Array.make n 0. in 
	for i = 0 to n-1 do 
		grad.(i) <- fonction.(i);
	done;grad;;
	
let gradient_max fonctions repartition n= 
	let max = ref 0. in 
	let grad = ref (Array.make n 0.) in 
	for i = 0  to n-1 do 
		let valeur = (calcule fonctions.(i) repartition n) in
			if valeur > !max 
				then (max := valeur; grad := gradient fonctions.(i) n);
	done;!grad;;

let distance a b n = 
	let distance = ref 0.0 in 
	for i = 0 to n-1 do 
		distance := !distance +. (a.(i) -. b.(i)) *. (a.(i) -. b.(i));
	done; !distance;;

	
let nouvelle_reparition fonctions repartition pas n= 
	let grad = gradient_max fonctions repartition n in
	let composante_max = ref 0. in 
	let indice_max = ref (-1) in
	for i = 0 to n-1  do 
		if grad.(i) > !composante_max 
			then (composante_max := grad.(i);indice_max := i);	
	done; 
	let delta_max = !composante_max *. pas in
	repartition;;
	
let descente fonctions debit n = 
	let pas = 1. in
	let repartion = ref (Array.make n 0.) in 
		(!repartion).(0) <- debit ;
		let nouvelle = ref (nouvelle_reparition fonctions !repartion pas n) in
		while ((distance  !repartion !nouvelle n) > 1.) do 
			repartion := !nouvelle;
			nouvelle := (nouvelle_reparition fonctions !repartion pas n);
		done;!nouvelle;;
			
	
		


