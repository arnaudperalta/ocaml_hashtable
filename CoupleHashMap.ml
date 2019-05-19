module CoupleHashMap =
struct
  type clef = int

  let clef_comp a b = compare a b
	
	type valeur = string

  let val_comp a b = compare a b
	
	let hash a = String.length a
end