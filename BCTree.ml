module BCTree (C : CoupleHashMap) =
struct
  
	type element = C.valeur
	
  type couleur =
  	| DoubleNoir
  	| Noir
  	| Rouge
  
  type arbreRN = 
  	| Vide
  	| VideNoir
    | Noeud of element * couleur * arbreRN * arbreRN

  let arbreVide = Vide

	let rec appartientA comp e = function
  	| Vide | VideNoir -> false
  	| Noeud(value, _, _, _) when comp e value = 0 -> true
  	| Noeud(value, _, _, arbd) when comp e value = 1 -> appartientA comp e arbd
  	| Noeud(value, _, arbg, _) -> appartientA comp e arbg

  let rec maxAb = function
  	| Vide -> failwith "Erreur_max"
  	| VideNoir -> failwith "Erreur_max2"
  	| Noeud(x, _, _, Vide) -> x
  	| Noeud(x, _, _, VideNoir) -> x
  	| Noeud(_, _, _, abd) -> maxAb abd

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

	let pasDeFils = function
		| Noeud(_, _, Vide, Vide) -> true
		| _ -> false

	let valeurTop = function
		| Vide | VideNoir -> failwith("erreur valeur")
		| Noeud(x, _, _, _) -> x
			
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
  
  let insertionSimple v a =
  	let rec insertSAux x comp = function
  		| Vide | VideNoir -> Noeud (x, Rouge, Vide, Vide)
  		| Noeud (x2, c, g, d) ->
  				if comp x x2 = 0 then
  					Noeud(x2, c, g, d)
  				else if comp x x2 = -1 then 
  					equilibrer (Noeud(x2, c, insertSAux x comp g, d))
  				else 
  					equilibrer (Noeud(x2, c, g , insertSAux x comp d))
  	in colorerRacine Noir (equilibrer (insertSAux v C.val_comp a))
	
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
  	| VideNoir -> VideNoir


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
  
	let suppSimple v a =
    let rec suppSAux n comp = function
    	| Noeud(x, c, ar1, ar2) when comp n x = -1 -> equilibrerSupp (Noeud(x, c, suppSAux n comp ar1, ar2))
    	| Noeud(x, c, ar1, ar2) when comp n x = 1 -> equilibrerSupp (Noeud(x, c, ar1, (suppSAux n comp ar2)))
    	| Noeud(x, Rouge, Vide, Vide) when comp n x = 0  -> Vide
    	| Noeud(x, Noir, Vide, Vide) when comp n x = 0 -> VideNoir
    	| Noeud(x, DoubleNoir, Vide, Vide) -> failwith "supp_double_noir"
    	| Noeud(x, Noir, ar1, Vide) when comp n x = 0 -> equilibrerSupp (plusPoidNoir ar1)
    	| Noeud(x, _, ar1, Vide) when comp n x = 0 -> ar1
    	| Noeud(x, Noir, Vide, ar2) when comp n x = 0 -> equilibrerSupp (plusPoidNoir ar2)
    	| Noeud(x, _, Vide, ar2) when comp n x = 0 -> ar2
    	| Noeud(x, c, ar1, ar2) when comp n x = 0 -> equilibrerSupp (Noeud((maxAb ar1), c, (suppMax ar1), ar2))
    	| _ -> Vide
		in equilibrerSupp (suppSAux v C.val_comp a)

end