(*#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\interfaces.mli";;*)
(*#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\interfaces.mli";;*)
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/interfaces.mli";;

module HashStringToInt =
struct
	
	module C = CoupleHashMap
	module B = BCTree(C)
	
	type elem =
		| Mot of C.valeur
		| Ensemble of BCTree(C).arbreRN
	
	type couple =
		| Couple of C.clef * elem
	
	type couleur =
		| DoubleNoir
		| Noir
		| Rouge
	
	type hashStringToInt =
		| Vide
		| VideNoir
		| Noeud of couple * couleur * hashStringToInt * hashStringToInt
	
	let mapVide = Vide
	
	let rec appartientA comp e = function
		| Vide | VideNoir -> false
		| Noeud(value, _, _, _) when comp e value = 0 -> true
		| Noeud(value, _, _, arbd) when comp e value = 1 -> appartientA comp e arbd
		| Noeud(value, _, arbg, _) -> appartientA comp e arbg
	
	let estRacine c = function
		| Noeud(_, x, _, _) when (x = c) -> true
		| _ -> false
	
	let colorerRacine c = function
		| Noeud(v, _, g, d) -> Noeud(v, c, g, d)
		| Vide -> Vide
		| VideNoir -> VideNoir
	
	let aFilsRouge = function
		| Noeud(_, _, Noeud(_, Rouge, _, _), _)
		| Noeud(_, _, _, Noeud(_, Rouge, _, _)) -> true
		| _ -> false
	
	let rotationGauche = function
		| Vide -> Vide
		| VideNoir -> VideNoir
		| Noeud(_, _, _, (Vide | VideNoir)) -> failwith ("Erreur rotationGauche")
		| Noeud(m, c1, a1, Noeud(n, c2, a2, a3)) ->
				Noeud(n, c2, Noeud(m, c1, a1, a2), a3)
	
	let rotationDroite = function
		| Vide -> Vide
		| VideNoir -> VideNoir
		| Noeud(_, _, (Vide | VideNoir), _) -> failwith ("Erreur rotationDroite")
		| Noeud(n, c2, Noeud(m, c1, a1, a2), a3) ->
				Noeud(m, c1, a1, Noeud(n, c2, a2, a3))
	
	let equilibrer = function
		| Noeud(vg, Noir, (Noeud(_, Rouge, _, _) as p), (Noeud(_, Rouge, _, _)as f))
		when aFilsRouge p || aFilsRouge f
		-> Noeud(vg, Rouge, colorerRacine Noir p, colorerRacine Noir f)
		| Noeud(vg, Noir, Noeud(vp, Rouge, x, adp), ((Noeud(_, Noir, _, _) | Vide) as f))
		when (estRacine Rouge x)
		-> rotationDroite(Noeud(vg, Noir, Noeud(vp, Rouge, x, adp), f))
		| Noeud(vg, Noir, Noeud(vp, Rouge, agp, x), ((Noeud(_, Noir, _, _) | Vide) as f))
		when (estRacine Rouge x)
		-> (Noeud(vg, Noir, rotationGauche( Noeud(vp, Rouge, agp, x)), f))
		| Noeud(vg, Noir, ((Noeud(_, Noir, _, _) | Vide) as f), Noeud(vp, Rouge, x, adp))
		when (estRacine Rouge x)
		-> (Noeud(vg, Noir, f, rotationDroite( Noeud(vp, Rouge, x, adp))))
		| Noeud(vg, Noir, ((Noeud(_, Noir, _, _) | Vide) as f), Noeud(vp, Rouge, agp, x))
		when (estRacine Rouge x)
		-> rotationGauche(Noeud(vg, Noir, f, Noeud(vp, Rouge, agp, x)))
		| x -> x
	
	let insertion arbre word =
		let rec insertionAux xWord xHashed funComp = function
			| Vide | VideNoir -> Noeud(Couple(xHashed, Mot(xWord)), Rouge, Vide, Vide)
      (* On rencontre un couple avec mot *)
			| Noeud (Couple(yHashed, Mot(yWord)) as v, c, g, d) ->
					if funComp xHashed yHashed = 0 then
						(* ligne ou on creer lensemble car deux hash pareil et on ajoute les 2 mots*)
						Noeud(Couple(xHashed, Ensemble(B.insertionSimple xWord (B.insertionSimple yWord B.arbreVide))), c, g, d)
					else if funComp xHashed yHashed = -1 then
						equilibrer(Noeud(v, c, insertionAux xWord xHashed funComp g, d))
					else
						equilibrer(Noeud(v, c, g , insertionAux xWord xHashed funComp d))
			(* On rencontre un couple avec ensemble *)
			| Noeud (Couple(yHashed, Ensemble(yEns)) as v, c, g, d) ->
					if funComp xHashed yHashed = 0 then
						(* ligne ou on creer lensemble car deux hash pareil et on ajoute les 2 mots*)
						Noeud(Couple(xHashed, Ensemble(B.insertionSimple xWord yEns)), c, g, d)
					else if funComp xHashed yHashed = -1 then
						equilibrer(Noeud(v, c, insertionAux xWord xHashed funComp g, d))
					else
						equilibrer(Noeud(v, c, g , insertionAux xWord xHashed funComp d))
			| _ -> failwith("Erreur insertion")
		in colorerRacine Noir(equilibrer(insertionAux word (CoupleHashMap.hash word) CoupleHashMap.clef_comp arbre))
		
		let rec maxAb = function
  	| Vide -> failwith "Erreur_max"
  	| VideNoir -> failwith "Erreur_max2"
  	| Noeud(x, _, _, Vide) -> x
  	| Noeud(x, _, _, VideNoir) -> x
  	| Noeud(_, _, _, abd) -> maxAb abd
		
	  let plusPoidNoir = function
  	| Vide -> VideNoir
  	| Noeud(x, Noir, ar1, ar2) -> Noeud(x, DoubleNoir, ar1, ar2)
  	| Noeud(x, Rouge, ar1, ar2) -> Noeud(x, Noir, ar1, ar2)
  	| _ -> failwith "error_PPN"

  let moinsPoidNoir = function
  	| VideNoir -> Vide
  	| Noeud(x, DoubleNoir, ar1, ar2) -> Noeud(x, Noir, ar1, ar2)
  	| Noeud(x, Noir, ar1, ar2) -> Noeud(x, Rouge, ar1, ar2)
  	| _ -> failwith "error_MPN"

  let rec suppMax = function
  	| Noeud(_, Noir, Vide, Vide) -> VideNoir
  	| Noeud(_, Noir, ag, Vide) -> plusPoidNoir ag
  	| Noeud(x, c, ag, ad) -> Noeud(x, c, ag, (suppMax ad))
  	| _ -> failwith"error_supp_du_max"
    
	
  let rec equ_aux = function
  	| Noeud(vp, pc, ((Noeud(_, DoubleNoir, _, _) | VideNoir) as x),
  	(Noeud(vf, Noir, (Noeud(_, Noir, _, _) | Vide), (Noeud(_, Noir, _, _) | Vide)) as f)) ->
  			plusPoidNoir (Noeud(vp, pc, moinsPoidNoir x, colorerRacine Rouge f))
  	| Noeud(vp, pc, (Noeud(vf, Noir, (Noeud(_, Noir, _, _) | Vide), (Noeud(_, Noir, _, _) | Vide)) as f),
  	((Noeud(_, DoubleNoir, _, _) | VideNoir) as x)) ->
  			plusPoidNoir (Noeud(vp, pc, colorerRacine Rouge f, moinsPoidNoir x))
  	| Noeud(a, b, ag, ad) -> Noeud(a, b, equ_aux ag, equ_aux ad)
  	| Vide -> Vide
  	| VideNoir -> Vide


  let equilibrerSupp = function
  	(* DEF 2 f est le fils droit f noir d est rouge *)
  	| Noeud(vp, pc, ((Noeud(_, DoubleNoir, _, _) | VideNoir) as x),
  	Noeud(vf, Noir, g, ((Noeud(_, Rouge, _, _)) as d))) ->
  			equ_aux (rotationGauche (Noeud(vp, Noir, moinsPoidNoir x, Noeud(vf, pc, g, (colorerRacine Noir d)))))
  	(* g est rouge *)
  	| Noeud(vp, pc, ((Noeud(_, DoubleNoir, _, _) | VideNoir) as x),
  	Noeud(vf, Noir, ((Noeud(_, Rouge, _, _)) as g), (((Noeud(_, Noir, _, _) | Vide) as d)))) ->
  			equ_aux (Noeud(vp, pc, x, rotationDroite (Noeud(vf, Rouge, colorerRacine Noir g, d))))
  	(* f rouge *)
  	| Noeud(vp, pc, ((Noeud(_, DoubleNoir, _, _) | VideNoir) as x), (Noeud(_, Rouge, _, _) as f)) ->
  			equ_aux (rotationGauche(Noeud(vp, Rouge, x, colorerRacine Noir f)))
  	(* f est le fils gauche f noir g est rouge *)
  	| Noeud(vp, pc, Noeud(vf, Noir, ((Noeud(_, Rouge, _, _)) as g), d),
  	((Noeud(_, DoubleNoir, _, _) | VideNoir) as x)) ->
  			equ_aux (rotationDroite (Noeud(vp, Noir, Noeud(vf, pc, (colorerRacine Noir g), d), moinsPoidNoir x)))
  	(* d est rouge *)
  	| Noeud(vp, pc, Noeud(vf, Noir, (((Noeud(_, Noir, _, _) | Vide) as g)), ((Noeud(_, Rouge, _, _)) as d)),
  	((Noeud(_, DoubleNoir, _, _) | VideNoir) as x)) ->
  			equ_aux (Noeud(vp, pc, rotationGauche (Noeud(vf, Rouge, g, colorerRacine Noir d)), x))
  	(* f rouge *)
  	| Noeud(vp, pc, (Noeud(_, Rouge, _, _) as f), ((Noeud(_, DoubleNoir, _, _) | VideNoir) as x)) ->
  			equ_aux (rotationDroite(Noeud(vp, Rouge, colorerRacine Noir f, x)))
  	| test -> equ_aux test
		
		
		let ensCheck = function
			| Noeud(Couple(yHashed, Ensemble(Noeud(xWord,_,Vide,Vide))), c, g, d) ->  Noeud(Couple(yHashed, Mot(xWord)), c, g ,d)
			| arbre -> arbre
		
		let suppression arbre word =
      let rec suppAux xWord xHashed funCompHash funCompVal = function
				| Noeud(Couple(yHashed, Ensemble(yEns)), c, g, d)  when funCompHash xHashed yHashed = 0 ->
					  ensCheck (Noeud(Couple(yHashed, Ensemble(B.suppSimple xWord yEns)), c, g, d))
      	| Noeud(Couple(yHashed, Mot(yWord)), Rouge, Vide, Vide)  when funCompVal xWord yWord = 0 -> Vide
				| Noeud(Couple(yHashed, Mot(yWord)), Noir, Vide, Vide)  when funCompVal xWord yWord = 0 -> VideNoir
      	| Noeud(Couple(yHashed, Mot(yWord)), DoubleNoir, Vide, Vide) -> failwith "supp_double_noir"
				| Noeud(Couple(yHashed, Mot(yWord)), Noir, g, Vide)  when funCompVal xWord yWord = 0 ->
					  equilibrerSupp (plusPoidNoir g)
				| Noeud(Couple(yHashed, Mot(yWord)), _, g, Vide)  when funCompVal xWord yWord = 0 -> g
				| Noeud(Couple(yHashed, Mot(yWord)), Noir, Vide, d)  when funCompVal xWord yWord = 0 ->
					  equilibrerSupp (plusPoidNoir d)
				| Noeud(Couple(yHashed, Mot(yWord)), _, Vide, d)  when funCompVal xWord yWord = 0 -> d
				| Noeud(Couple(yHashed, Mot(yWord)), c, g, d)  when funCompHash xHashed yHashed = -1 ->
					  equilibrerSupp (Noeud(Couple(yHashed, Mot(yWord)), c, suppAux xWord xHashed funCompHash funCompVal g, d))
				| Noeud(Couple(yHashed, Mot(yWord)), c, g, d)  when funCompHash xHashed yHashed = 1 ->
					  equilibrerSupp (Noeud(Couple(yHashed, Mot(yWord)), c, g, suppAux xWord xHashed funCompHash funCompVal d))
				| Noeud(Couple(yHashed, Mot(yWord)), c, g, d)  when funCompHash xHashed yHashed = 0 ->
					  equilibrerSupp (Noeud((maxAb g), c, (suppMax g), d))
      	| _ -> Vide
		in equilibrerSupp (suppAux word (C.hash word) C.val_comp C.clef_comp arbre)
		
	let rec estDans arbre word =
		match arbre with
		| Noeud(Couple(_, Mot(word2)), _, _, _) when C.val_comp word word2 = 0 -> true
		| Noeud(Couple(hash, Ensemble(x)), _, _, _) when (C.clef_comp hash (C.hash word)) = 0 ->
			B.appartientA (C.val_comp) word x
		| Noeud(Couple(hash, _), _, ag, _) when (C.clef_comp hash (C.hash word)) > 0 -> estDans ag word
		| Noeud(Couple(hash, _), _, _, ad) when (C.clef_comp hash (C.hash word)) < 0 -> estDans ad word
		| _ -> false

	let hashUnion a1 a2 =
		let rec parcours a1 = function
			| Vide | VideNoir -> a1
			| Noeud(Couple(_, Mot(x)), _, _, _) as abAux -> parcours (insertion a1 x) (suppression abAux x)
			| Noeud(Couple(_, Ensemble(Noeud(x, _, _, _))), _, _, _) as abAux -> parcours (insertion a1 x) (suppression abAux x)
			| _ -> failwith("erreur Union")
			in parcours a1 a2
			
	let hashInter a1 a2 =
		let rec remplir a a1 = function
			| Vide | VideNoir -> a
			| Noeud(Couple(_, Mot(x)), _, _, _) as abAux when estDans a1 x -> remplir (insertion a x) a1 (suppression abAux x)
			| Noeud(Couple(_, Mot(x)), _, _, _) as abAux -> remplir a a1 (suppression abAux x)
			| Noeud(Couple(_, Ensemble(Noeud(x, _, _, _))), _, _, _) as abAux when estDans a1 x -> remplir (insertion a x) a1 (suppression abAux x)
			| Noeud(Couple(_, Ensemble(Noeud(x, _, _, _))), _, _, _) as abAux -> remplir a a1 (suppression abAux x)
			| _ -> failwith("erreur  Inter")
			in remplir mapVide a1 a2
			
	let hashDiff a1 a2 =
		let rec parcours a1 = function
			| Vide | VideNoir -> a1
			| Noeud(Couple(_, Mot(x)), _, _, _) as abAux when estDans a1 x -> parcours (suppression a1 x) (suppression abAux x)
			| Noeud(Couple(_, Mot(x)), _, _ ,_) as abAux-> parcours a1 (suppression abAux x)
			| Noeud(Couple(_, Ensemble(Noeud(x, _, _, _))), _, _, _) as abAux when estDans a1 x -> parcours (suppression a1 x) (suppression abAux x)
			| Noeud(Couple(_, Ensemble(Noeud(x, _, _, _))), _, _, _) as abAux -> parcours a1 (suppression abAux x)
			| _ -> failwith("erreur Diff")
			in parcours a1 a2
			
	let hashDiffSym a1 = function
		| Vide | VideNoir -> a1
		| a2 -> hashUnion (hashDiff a1 a2) (hashDiff a2 a1)
end