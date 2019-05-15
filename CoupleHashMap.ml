(*#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\interfaces.mli";;*)
    
(*#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\interfaces.mli";;*)
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/interfaces.mli";;

module CoupleHashMap =
struct
  type clef = int

  let clef_comp a b = compare a b
	
	type valeur = string

  let val_comp a b = compare a b
	
	let hash a = String.length a
end