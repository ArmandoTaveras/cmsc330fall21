open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold(fun x y -> if y = e then true else x) false lst;;

let is_present lst x = fold (fun g y -> if y = x then g @ [1] else g @ [0]) [] lst;;

let count_occ lst target = fold(fun count e -> if target = e then count + 1 else count) 0 lst;;

let uniq lst = fold(fun x y -> if(contains_elem(x)(y)) then x else x @ [y]) [] lst;;

let assoc_list lst = fold(fun x y -> if contains_elem(x)(y, count_occ lst y) then x else x @ [(y, count_occ lst y)]) [] lst;;

let ap fns args = fold(fun x y -> x @ map(y)(args)) [] fns