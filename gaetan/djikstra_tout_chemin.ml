type arrete = { depart : int ; arrivee : int ; tempsMin : float ; acroissement : float};;

type graphe = {
	nombre_noeuds : int ;
	nombre_arretes : int ;
	noeuds : int list array;
	arretes : arrete array;
};;

type chemin = (int list) * int ;; (*liste des arretes , dernier noeud*)

let rec ajoute chemin temps non_visiter file = match file with
	| [] -> [(chemin,temps,non_visiter)]
	| (c,t,nv)::f when t < temps -> (c,t,nv)::(ajoute chemin temps non_visiter f)
	| _ -> (chemin,temps,non_visiter)::file;;
	
let rec ajoute_nouveau_chemin arretes (parcours,fin) temps non_visiter file graphe = match arretes with 
	| [] -> file
	| a::l when non_visiter.(a) -> let arrete = graphe.arretes.(a) in
		let nv = Array.copy non_visiter in  
		nv.(arrete.arrivee) <- false ;
		ajoute_nouveau_chemin l (parcours,fin) temps non_visiter (ajoute (a::parcours,arrete.arrivee) (temps +. arrete.tempsMin) nv file) graphe
	| a::l -> ajoute_nouveau_chemin l (parcours,fin) temps non_visiter file graphe;;
	
let rec djikstra_aux file arrivee graphe= match file with 
	| [] -> []
	| ((parcours,fin),temps,non_visiter)::f when fin = arrivee -> parcours::djikstra_aux f arrivee graphe
	| ((parcours,fin),temps,non_visiter)::f -> (djikstra_aux 
			(ajoute_nouveau_chemin graphe.noeuds.(fin) (parcours,fin) temps non_visiter file graphe)
			arrivee graphe);;
	
let djikstra depart arrivee graphe = 
	let non_visiter = Array.make graphe.nombre_noeuds true in 
		non_visiter.(depart) <- false ;
		djikstra_aux [(([],depart),0.,non_visiter)] arrivee graphe;;

