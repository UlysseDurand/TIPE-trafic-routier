let rec sum a b f = if b < a then 0. else (f b)+.(sum a (b-1) f);;
let print_matrix fonc m = Array.iter (fun l-> Array.iter (fun e -> fonc e ; print_string " ";) l ; print_newline ();) m

(* ##### *)
type fonction = float -> float;;

(* ##### *)

type sommet = int*(int list);;

(* graph : indice_source, indice_puit, les arretes , les sommets *)
type graph = (int*int*sommet array);;

type arrete = fonction*float;;

let rec deconstruit l = match l with
	|[] -> []
	|x::xs -> x@(deconstruit xs);;


let fst (a,b) = a;;
let scd (a,b) = b;;

(*ce qui est dans a mais pas dans fst b *)
let rec diffsym a b = match a with 
	|[]->[];
	|x::xs-> if List.mem (fst x) b then (diffsym xs b) else x::(diffsym xs b) 

(*liste des chemins de s jusque t de g (un chemin : sommets empruntes, arretes empruntees)*)
let rec trouverchemins g =
	let s,t,listadj = g in
	let res = ref [] in
	let rec aux lesommet dejavu arretesduchemin = let id,voisins = lesommet in
		let nouveaudejavu = id::dejavu in 
		if id = t then res:=(nouveaudejavu,arretesduchemin)::(!res) else
		let idsvoisinspaspris = (diffsym voisins dejavu) in
		List.iter
		(fun (idvoisin,idarrete) -> (aux listadj.(idvoisin) nouveaudejavu (idarrete::arretesduchemin) )) 
		idsvoisinspaspris in
	aux listadj.(s) [] [];
	(!res);;

let ungraph = (0,3,[|(0,[(1,0);(2,1)]) ; (1,[(0,2);(2,3);(3,4)]) ; (2,[(0,5);(1,6);(3,7)]) ; (3,[(1,8);(2,9)])|]);;

trouverchemins ungraph;;

let maxi l comp = let n = Array.length l in
	let ind = ref 0 in 
	let max = ref (l.(0)) in
	for i=0 to n-1 do
        	let letruc=l.(i) in
        	if (comp letruc (!max)) then 
			max:=letruc ; ind:=i
	done;(!ind,!max);;

(*opetermaterm [|1;2;3|] [|4;5;6|] (fun x y -> x+y) -> [|5;7;9|]*)
let opetermaterm a b ope = let n = (Array.length a) in let res = (Array.make n 0.) in
    for i=0 to (n-1) do 
    	res.(i)<-(ope a.(i) b.(i)) 
    done ;res;;
 
(*map [|1;2;3|] (fun x -> x*.x) -> [|1;4;9|]*)
let map v ope = let n = (Array.length v) in let res = (Array.make n 0.) in 
	for i=0 to (n-1) do
		res.(i) <- (ope i v.(i)) 
	done; res;;

(*plie f a b 0 (fun x y -> x+y) -> \sum_{k=a}^b f(k)*)
let rec plie f a b e p = if (a>b) then e else (p (f b) (plie f a (b-1) e p) );;

let plus a b = opetermaterm a b (fun a b -> a+.b);;
let mult v l = (map v (fun i x ->(x *. l)));;

let lafont t = (t.(0)-.1.)*.(t.(0)-.1.)+.(t.(1)-.3.)*.(t.(1)-.3.);;

let derive f i x delta = let n = (Array.length x) in 
	let dx = (Array.make n 0.) in
	dx.(i)<-delta;
	let res = (Array.make n 0.) in 
	res.(i)<-(((f (plus x dx)) -. (f x))/.delta);res;;

let grad f x delta = let n = (Array.length x) in
	(plie (fun i -> derive f i x delta) 0 (n-1) (Array.copy (Array.make n 0.)) plus);;

let rec graddesc f p v delta l = 
	if p = 0 then v
	else let nv = (plus v (mult (grad f v delta) l)) in
	(graddesc f (p-1) nv delta l) ;;

let rec notregraddesc fl p v l delta =
    if p=0 then v else
    	let fmap = map fl (fun i f -> f v) in
    	let maxind,max = maxi fmap (fun x y -> x>y) in
    	let nv = plus v (mult (grad fl.(maxind) v delta) l) in
    	notregraddesc fl (p-1) nv l delta;;

(*leschemins : chemins de A a B dans graph*)
let evaluation graph lescheminsliste fonctions debits =
	let leschemins = lescheminsliste in
	let n = Array.length leschemins in
	let tis = Array.make n 0. in
	let debitschemins = Array.make n 0. in
	for i=0 to n-1 do
		let lessommetsduchemin,lesarretesducheminliste = leschemins.(i) in
		let lesarretesduchemin = Array.of_list lesarretesducheminliste in
		let m = Array.length lesarretesduchemin in
		debitschemins.(i) <- sum 0 (m-1) 
				(fun j->let larrete = lesarretesduchemin.(j) in
				 debits.(larrete) )
	done;
	for i=0 to n-1 do
		let ledebit = debitschemins.(i) in
		let lessommetsduchemin,lesarretesducheminliste = leschemins.(i) in
		let lesarretesduchemin = Array.of_list lesarretesducheminliste in
		let m = Array.length lesarretesduchemin in
		tis.(i) <- sum 0 (m-1)
				(fun j->let larrete = lesarretesduchemin.(j) in
				fonctions.(larrete) ledebit )
	done;
	sum 0 (n-1) (fun i -> debits.(i) *. tis.(i)) 
	;;

grad lafont [|0.2;2.|] 0.01;;
graddesc lafont 50 [|2.;2.|] 0.001 (-.0.1);;

(* #######################GRAPHE SERIE PARALLELE *)

type graph_serie_parallele  =  
	A of int |
	S of graph_serie_parallele*graph_serie_parallele |
	P of graph_serie_parallele*graph_serie_parallele;;

let minimise unef = (graddesc (fun x-> unef x.(0)) 100 [|0.|] 0.1 0.1).(0);;

let deuxiemecouple (a,b) = b;;

let foncnulle x = 0.;;
let fonctutile x = x;;
let fonctionsdesarretes = [|foncnulle;foncnulle;fonctutile;foncnulle;fonctutile;fonctutile;fonctutile;fonctutile;fonctutile;fonctutile;fonctutile;fonctutile;fonctutile|];;

let rec letemps g d lesf = match g with
	|A(x)->d,((lesf.(x)) d);
	|S(x,y)->d,(deuxiemecouple (letemps x d lesf))+.(deuxiemecouple (letemps y d lesf));
	|P(x,y)->
		let ftilde = fun d d1-> (d1/.d)*.(deuxiemecouple (letemps x d1 lesf)) +. ((d-.d1)/.d)*.(deuxiemecouple (letemps y (d-.d1) lesf)) in
		let ds= minimise (ftilde d) in
		let ts = ftilde d ds in
		ds,ts;;

(*ON FAIS DES EXEMPLES #########*) (*une arrete : (sommet destination,indice arrete)*)
let grapheexemple = (0,8,[|
							(0,[(1,0);(2,1);(3,2);(4,3)]);
							(1,[]);
							(2,[(0,4);(6,5)]);
							(3,[(7,6)]);
							(4,[(0,7);(1,8);(7,9)]);
							(5,[(1,10);(7,11);(8,12)]);
							(6,[(2,13);(3,14)]);
							(7,[(6,15);(3,16);(5,17);(8,18)]);
							(8,[])
						|]);;

let graphesgexemple = 
					S(
					  P(
						S(
						  P(
							A(4),
							A(5)
						  ),
						  P(
							A(6),
						  P(
							A(7),
							A(8)
						  )
						  )
						),
						P(
						A(2),
						P(
						  A(11),
						  A(12)
						)  
						)
					  ),
					  P(
						A(9),
						A(10)
					  )
					);;

let leschemins = trouverchemins grapheexemple;;
let unbeauchemin = Array.of_list (List.map (fun (a,b)->Array.of_list a) leschemins);;
(*print_matrix print_int (unbeauchemin);;*)
letemps graphesgexemple 1. fonctionsdesarretes;;