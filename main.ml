(*#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\interfaces.mli";;
#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\CoupleHashMap.ml";;
#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\HashStringToInt.ml";;
#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\BCTree.ml";;*)

(*#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\interfaces.mli";;
#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\CoupleHashMap.ml";;
#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\HashStringToInt.ml";;
#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\BCTree.ml";;*)

#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/interfaces.mli";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/CoupleHashMap.ml";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/HashStringToInt.ml";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/BCTree.ml";;

(* Main *)

module B = BCTree(CoupleHashMap)
module H = HashStringToInt

let toto = H.insertion (H.insertion H.mapVide "totiio") "tata"
let tata = H.insertion H.mapVide "tata"

let titi1 = H.hashDiff toto tata
let titi2 = H.hashDiff tata toto


let titi = H.hashDiffSym toto tata