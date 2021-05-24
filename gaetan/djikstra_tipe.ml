type arrete = { depart : int ; arrivee : int ; tempsMin : float ; acroissement : float};;

type graphe = {
	nombre_noeuds : int ;
	nombre_arretes : int ;
	noeuds : int list array;
	arretes : arrete array;
};;

type chemin = (int list) * int ;; (*liste des arretes , dernier noeud*)

let rec ajoute chemin temps file = match file with
	| [] -> [(chemin,temps)]
	| (c,t)::f when t < temps -> (c,t)::(ajoute chemin temps f)
	| _ -> (chemin,temps)::file;;
	
let rec ajoute_nouveau_chemin arretes (parcours,fin) temps file graphe = match arretes with 
	| [] -> file
	| a::l -> let arrete = graphe.arretes.(a) in
		ajoute_nouveau_chemin l (parcours,fin) temps (ajoute (a::parcours,arrete.arrivee) (temps +. arrete.tempsMin) file) graphe;;
	
let rec djikstra_aux file non_visiter arrivee graphe= match file with 
	| [] -> failwith "pas de chemin"
	| ((parcours,fin),temps)::f when fin = arrivee -> parcours
	| ((parcours,fin),temps)::f when non_visiter.(fin) -> (
		non_visiter.(fin) <- false ;
		djikstra_aux 
			(ajoute_nouveau_chemin graphe.noeuds.(fin) (parcours,fin) temps file graphe) 
			non_visiter arrivee graphe)
	| ((parcours,fin),temps)::f -> djikstra_aux f non_visiter arrivee graphe;;
	
let djikstra depart arrivee graphe = 
	let non_visiter = Array.make graphe.nombre_noeuds true in 
		non_visiter.(depart) <- false ;
		djikstra_aux [(([],depart),0.)] non_visiter arrivee graphe;;

