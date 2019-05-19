module type CoupleHashMap =
sig
  (** Définition du type ordonné de la clef. *)
  type clef
  (** Comparaison des clefs*)
  val clef_comp : clef -> clef -> int
  (** Définition du type ordonné de la valeur *)
  type valeur
  (** Comparaison des valeurs *)
  val val_comp : valeur -> valeur -> int
  (** Fonction de hachage *)
  val hash : valeur -> clef
end

(** Module permettant la création d'arbre bicolor simple pour les ensembles*)
module type BCTree =
	functor (C: CoupleHashMap) ->
    sig
    	type element = C.valeur
    	(** Définition du type arbre bicolore. *)
    	type arbreRN
			(** Teste si l'élément est présent *)
			val appartientA : ('a * 'a -> int) * 'a * arbreRN -> bool
			(** Teste si l'arbre n'a pas de fils *)
			val pasDeFils : arbreRN -> bool
			(** Renvoie la première valeur de l'arbre *)
			val valeurTop : arbreRN -> element
    	(** Renvoie un arbre bicolore vide. *)
    	val arbreVide : arbreRN
    	(** Insère une valeur de type CoupleHashMap.valeur dans un arbre bicolore. *)
    	val insertionSimple : C.valeur -> arbreRN -> arbreRN
			(** Supprime une valeur de type CoupleHashMap.valeur dans un arbre bicolore. *)
			val suppSimple : C.valeur -> arbreRN -> arbreRN
    end

module type HashStringToInt =
	functor (B : BCTree) ->
		functor (C : CoupleHashMap) ->
      sig
      	(** Définition du type de la table de hachage. *)
      	type hashStringToInt
        (** Renvoie une table vide. *)
        val mapVide : hashStringToInt
        (** Insère une chaîne dans la table. *)
        val insertion : hashStringToInt * C.valeur -> hashStringToInt
        (** Supprime une chaîne dans la table. *)
        val suppression : hashStringToInt * C.valeur -> hashStringToInt
        (** Teste si la chaîne est présente. *)
        val estDans : hashStringToInt * C.valeur -> bool
				(** Opérateur union *)
				val hashUnion : hashStringToInt * hashStringToInt -> hashStringToInt
				(** Opérateur intersection *)
				val hashInter : hashStringToInt * hashStringToInt -> hashStringToInt
				(** Opérateur différence *)
				val hashDiff : hashStringToInt * hashStringToInt -> hashStringToInt
				(** Opérateur différence symétrique *)
				val hashDiffSym : hashStringToInt * hashStringToInt -> hashStringToInt
				(** Opérateur d'accumulationen *)
			  val hashFold : ('a -> 'b -> 'a) -> 'a -> hashStringToInt -> 'a
      end