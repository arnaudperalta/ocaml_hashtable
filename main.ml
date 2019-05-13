#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\interfaces.mli";;
#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\CoupleHashMap.ml";;
#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\HashStringToInt.ml";;
#use "C:\\Users\\Jérémy\\Desktop\\Cours\\projet_lsi\\projet\\BCTree.ml";;

(*#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\interfaces.mli";;
#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\CoupleHashMap.ml";;
#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\HashStringToInt.ml";;
#use "C:\\Users\\arnau\\eclipse-workspace\\ocaml_hashtable\\BCTree.ml";;*)

(*#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/interfaces.mli";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/CoupleHashMap.ml";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/HashStringToInt.ml";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/BCTree.ml";;*)

(* Main *)

module B = BCTree(CoupleHashMap)
module H = HashStringToInt

let titi = H.insertion H.mapVide "toto" B.arbreVide