module type CoupleHashMap =
sig
  (** D�finition du type ordonn� de la clef. *)
  type clef
  (** Comparaison des clefs*)
  val clef_comp : clef -> clef -> int
  (** D�finition du type ordonn� de la valeur *)
  type valeur
  (** Comparaison des valeurs *)
  val val_comp : valeur -> valeur -> int
  (** Fonction de hachage *)
  val hash : valeur -> clef
end

(** Module permettant la cr�ation d'arbre bicolor simple pour les ensembles*)
module type BCTree =
	functor (C: CoupleHashMap) ->
    sig
    	type element = C.valeur
    	(** D�finition du type arbre bicolore. *)
    	type arbreRN
			(** Teste si l'�l�ment est pr�sent *)
			val appartientA : ('a * 'a -> int) * 'a * arbreRN -> bool
    	(** Renvoie un arbre bicolore vide. *)
    	val arbreVide : arbreRN
    	(** Ins�re une valeur de type CoupleHashMap.valeur dans un arbre bicolore. *)
    	val insertionSimple : C.valeur -> arbreRN -> arbreRN
			(** Supprime une valeur de type CoupleHashMap.valeur dans un arbre bicolore. *)
			val suppSimple : C.valeur -> arbreRN -> arbreRN
    end

module type HashStringToInt =
	functor (B : BCTree) ->
		functor (C : CoupleHashMap) ->
      sig
      	(** D�finition du type de la table de hachage. *)
      	type hashStringToInt
        (** Renvoie une table vide. *)
        val mapVide : hashStringToInt
        (** Ins�re une cha�ne dans la table. *)
        val insertion : hashStringToInt * string -> hashStringToInt
        (** Supprime une cha�ne dans la table. *)
        val suppression : hashStringToInt * string -> hashStringToInt
        (** Teste si la cha�ne est pr�sente. *)
        val estDans : hashStringToInt * string -> bool
				(** Op�rateur union *)
				val hashUnion : hashStringToInt * hashStringToInt -> hashStringToInt
				(** Op�rateur intersection *)
				val hashInter : hashStringToInt * hashStringToInt -> hashStringToInt
				(** Op�rateur diff�rence *)
				val hashDiff : hashStringToInt * hashStringToInt -> hashStringToInt
				(** Op�rateur diff�rence sym�trique *)
				val hashDiffSym : hashStringToInt * hashStringToInt -> hashStringToInt
      end