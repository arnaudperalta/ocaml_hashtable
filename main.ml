#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/interfaces.mli";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/CoupleHashMap.ml";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/BCTree.ml";;
#use "/home/l2info/peralarn/eclipse-workspace/ocaml_hashtable/HashStringToInt.ml";;

(* Main *)

module B = BCTree(CoupleHashMap)
module H = HashStringToInt

let toto = H.insertion (H.insertion H.mapVide "toto") "tata"
let tata = H.hashFold (^) "" toto