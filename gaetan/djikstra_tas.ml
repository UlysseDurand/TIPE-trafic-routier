type poid = Inf | P of int;;

type 'a tas = { mutable taille : int ; valeurs : 'a};;

let ( <! ) p1 p2 = match (p1,p2) with 
	| (_,Inf) -> true
	| (Inf,_) -> false
	| (P(x),P(y)) -> x < y ;;
	
let ( +! ) p1 p2 = match (p1,p2) with 
	| (_,Inf) -> Inf

	| (Inf,_) -> Inf

	| (P(x),P(y)) -> P(x + y);;
	
let valeurs tas i = if i <= tas.taille then tas.valeurs.(i) else (Inf,-1);;  

let rec down tas i =
	let fg = 2 * i and fd = 2 * i + 1 in 
	let (x,s1) = (valeurs tas fg) and (y,s2) = (valeurs tas fd) and (z,s3) = (valeurs tas i) in 
		if fg <= tas.taille
			then if (x <! y) || fd <= tas.taille
				then if z <! x
					then ()
					else (tas.valeurs.(i) <- (x,s1) ; tas.valeurs.(fg) <- (z,s3) ; down tas fg) 
				else if z <! y
					then ()
					else (tas.valeurs.(i) <- (y,s2) ; tas.valeurs.(fd) <- (z,s3) ; down tas fd)
			else ();;
				
let rec up tas i = 
	if i > 1 
		then let parent = i / 2 in 
			let (x,s1) = tas.valeurs.(parent) and (y,s2) = tas.valeurs.(i) in 
			if x <! y 
				then ()
				else (tas.valeurs.(parent) <- (y,s2) ; tas.valeurs.(i) <- (x,s1) ; up tas parent)
		else ();;

let estVide tas = tas.taille < 1;;

let sommet tas = 
	if estVide tas
		then failwith"le tas est vide"
		else let sommet = tas.valeurs.(1) in (
			tas.valeurs.(1) <- tas.valeurs.(tas.taille);
			tas.taille <- tas.taille - 1;
			down tas 1;
			sommet);;
			
let creerTas tab i = 
	let n = Array.length(tab) in 
	let tableau = Array.make n (P(0),0) in 
		for j = 0 to (n-1) do 
			tableau.(j) <- (tab.(j),j);
		done; 
		tableau.(i) <- tableau.(0);
		let tas = {taille = (n-1) ; valeurs = tableau} in 
			for j = n downto 1 do 
				down tas j;
			done; tas;;

let graph = [| 
	[| Inf ; P(2) ; Inf ; P(5)	; P(3)|];
	[| P(2) ; Inf ; P(4) ; P(3); P(1)|];
	[| Inf ; P(4) ; Inf ; P(7)	; Inf |];
	[| P(5) ; P(3) ; P(7) ; Inf; P(3)|];
	[| P(3) ; P(1) ; Inf ; P(3); Inf |] |];;
	
let update distance mat tas i = 
	for j = 1 to tas.taille do 
		let (p,s) = tas.valeurs.(i) in
		let q = distance.(i) +! mat.(i).(s) in 
			if q <! p
				then (tas.valeurs.(i) <- (q,s); up tas j)
				else ()
	done;;
	
let djikstra mat i = 
	let n = Array.length(mat) in 
	let tas = creerTas mat.(i) i in 
	let distance = Array.make n Inf in 
		distance.(i) <- P(0); 
		while not (estVide tas) do 
			let (p,s) = sommet tas in 
				(distance.(s) <- p ; update distance mat tas s ; print_int(s))
		done;
		distance;;

djikstra graph 2;;

creerTas graph.(1) 1;;

#trace sommet;;		