let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)
;;

let rec foldl f a xs = match xs with
| [] -> a
| x :: xt -> foldl f (f a x) xt
;;

let rec foldr f xs a = match xs with
| [] -> a
| x :: xt -> f x (foldr f xt a) 
;;

let get_even acc y = if (y mod 2) = 0 then y::acc else acc;;

let get_even lst = match lst with
  [] -> []
  | h::t -> foldr (get_even [] lst)
;;

let rec get_first_k lst k acc = match lst with
  [] -> if k > 0 then [] else acc
  | h::t -> if k > 0 get_first_k t k-1 h::acc else acc
;;

let rec get_average lst count sum = match lst with
  [] -> sum / count
  | h::t -> get_average (t) (count + 1)  (sum + h)
;;

let rec all_averages lst acc = match lst with
  [] -> acc
  | h::t -> get_all_averages t (get_average(h)(0)(0)::acc)
;;

let k f g z = f(g z) ;;