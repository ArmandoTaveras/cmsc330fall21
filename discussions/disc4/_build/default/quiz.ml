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

let get_even

  