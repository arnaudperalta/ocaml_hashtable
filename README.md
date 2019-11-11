Projet de licence 2, implémentation d'une structure de table de hashage en Ocaml

# Interface :
## Module CoupleHashMap :
```ocaml
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
```

## Module HashStringToInt :
```ocaml
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
```

# Exemples

## Ajout de deux éléments avec le même hash :
```ocaml
#let toto = H.insertion (H.insertion H.mapVide "toto") "tata"
val toto : H.hashStringToInt =
H.Noeud
    (H.Couple (4,
    H.Ensemble
        (BCTree(H.C).Noeud ("toto", BCTree(H.C).Noir,
        BCTree(H.C).Noeud ("tata", BCTree(H.C).Rouge, BCTree(H.C).Vide,
            BCTree(H.C).Vide),
        BCTree(H.C).Vide))),
    H.Noir, H.Vide, H.Vide)
```

## Ajout et suppression du même élément :
```ocaml
#let tata = H.suppression (H.insertion H.mapVide "tata") "tata"
val tata : H.hashStringToInt = H.Vide
```

## Opération union :
```ocaml
#let toto = H.insertion (H.insertion H.mapVide "toto") "tata"
#let tata = H.insertion H.mapVide "tata"
#let titi = H.hashUnion toto tata
val titi : H.hashStringToInt =
H.Noeud
(H.Couple (4,
    H.Ensemble
    (BCTree(H.C).Noeud ("toto", BCTree(H.C).Noir,
        BCTree(H.C).Noeud ("tata", BCTree(H.C).Rouge, BCTree(H.C).Vide,
        BCTree(H.C).Vide),
        BCTree(H.C).Vide))),
H.Noir, H.Vide, H.Vide)
```

## Opération intersection :
```ocaml
#let toto = H.insertion (H.insertion H.mapVide "toto") "tata"
#let tata = H.insertion H.mapVide "tata"
#let titi = H.hashInter toto tata
val titi : H.hashStringToInt =
H.Noeud (H.Couple (4, H.Mot "tata"), H.Noir, H.Vide, H.Vide)
```

## Opération différence :
```ocaml
#let toto = H.insertion (H.insertion H.mapVide "toto") "tata"
#let tata = H.insertion H.mapVide "tata"
#let titi = H.hashDiff toto tata
val titi : H.hashStringToInt =
H.Noeud (H.Couple (4, H.Mot "tata"), H.Noir, H.Vide, H.Vide)
```

## Opération différence symétrique :
```ocaml
let toto = H.insertion (H.insertion H.mapVide "toto") "tata"
let tata = H.insertion (H.insertion H.mapVide "titi") "toto"
let titi = H.hashDiffSym toto tata
val titi : H.hashStringToInt =
H.Noeud
(H.Couple (4,
    H.Ensemble
    (BCTree(H.C).Noeud ("tata", BCTree(H.C).Noir, BCTree(H.C).Vide,
        BCTree(H.C).Noeud ("titi", BCTree(H.C).Rouge, BCTree(H.C).Vide,
        BCTree(H.C).Vide)))),
H.Noir, H.Vide, H.Vide)
```

## Opération accumulation :
```ocaml
#let toto = H.insertion (H.insertion H.mapVide "toto") "tata"
#let tata = H.hashFold (^) "" toto
val tata : string = "tototata"
```
