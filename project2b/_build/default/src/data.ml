open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = match t with
  | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode(val1, Some val2, left, middle, right) ->
      if x = val2 || x = val1 then t
      else if x < val1 then IntNode(val1, Some val2, int_insert (x) (left), middle, right)
      else if x < val2 then IntNode(val1, Some val2, left, int_insert (x) (middle), right)
      else IntNode(val1, Some val2, left, middle, int_insert (x) (right))
  | IntNode(val1, None, left, middle, right) ->
      if x = val1 then t
      else if x < val1 then IntNode(x, Some val1, left, middle, right)
      else IntNode(val1, Some x, left, middle, right)
;;

let rec int_mem x t = match t with 
  | IntLeaf -> false
  | IntNode(val1, Some val2, left, middle, right) ->
    if val1 = x || val2 = x then true
    else int_mem x left || int_mem x middle || int_mem x right
  | IntNode(val1, None, left, middle, right) ->
    if val1 = x then true
    else int_mem x left || int_mem x middle || int_mem x right
;;

let rec int_size t = match t with
  | IntLeaf -> 0
  | IntNode(y, Some z, left, middle, right) ->
    2 + int_size(left) + int_size(middle) + int_size(right)
  | IntNode(y, None, left, middle, right) -> 
    1 + int_size(left) + int_size(middle) + int_size(right)
;;

let rec int_max t = match t with
  | IntLeaf -> invalid_arg("int_max")
  | IntNode(val1, Some val2, left, middle, right) ->
    if right = IntLeaf then val2
    else int_max right
  | IntNode(val1, None, left, middle, right) ->
    if right = IntLeaf then val1
    else int_max right
;;
  

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with
| MapLeaf -> MapNode((k, v), None, MapLeaf, MapLeaf, MapLeaf)
| MapNode((key, value), Some (key2,value2), left, middle, right) ->
    if  k = key || k = key2 then t
    else if k < key then MapNode((key, value), Some (key2, value2), map_put(k)(v)(left), middle, right)
    else if k < key2 then MapNode((key, value), Some (key2, value2), left, map_put(k)(v)(middle), right)
    else MapNode((key, value), Some (key2, value2), left, middle, map_put(k)(v)(right))
| MapNode((key, value), None, left, middle, right) ->
    if k = key then t
    else if k < key then MapNode((k, v), Some (key, value), left, middle, right)
    else MapNode((key, value), Some (k,v), left, middle, right)
;;

let rec map_contains k t = match t with 
| MapLeaf -> false
| MapNode((key, value), Some (key2, value2), left, middle, right) ->
    if k = key || k = key2 then true
    else map_contains k left || map_contains k right || map_contains k middle
| MapNode((key, value), None, left, middle, right) ->
    if k = key then true
    else map_contains k left || map_contains k right || map_contains k middle
;;

let rec map_get k t = match t with 
| MapLeaf -> invalid_arg("map_get")
| MapNode((key, value), Some (key2, value2), left, middle, right) ->
    if k = key then value
    else if k = key2 then value2
    else if k < key then map_get k left
    else if k < key2 then map_get k middle
    else map_get k right
| MapNode((key, value), None, left, middle, right) ->
    if k = key then value
    else map_get k left
;;
(***************************)
(* Part 4: Variable Lookup *)
(***************************)
(* Create list of tuples, (key, list of values value) *)
(* Modify the next line to your intended type *)
(* Once I access the list call a recursive function that pattern matches with the tuple for example (a, b) :: t  -> Work *)
type lookup_table = (string * int) list list
 
let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = [] :: table

let pop_scope (table : lookup_table) : lookup_table = match table with
  | [] -> failwith "No scopes remain!"
  | h::t -> t
;;

let rec exists lst name =  match lst with
| [] -> false
| (key, value) :: t ->  
  if key = name then true
  else exists t name
;;

let rec traverse_add lst name value = match lst with
  |[] -> [(name, value)]
  |(key, val1) :: t ->
    if exists lst name = true then failwith "Duplicate variable binding in scope!"
    else lst @ [(name, value)] 
;;

let add_var name value (table : lookup_table) : lookup_table = match table with
  | [] -> failwith "There are no scopes to add a variable to!"
  | h :: t -> traverse_add h name value :: t
;;

let rec traverse lst name = match lst with
  | [] -> failwith "Error"
  | (key, value) :: t ->  
    if key = name then value
    else traverse t name
;;

let rec lookup name (table : lookup_table) = match table with
  | [] -> failwith "Variable not found!"
  | h :: t -> 
    if exists(h)(name) = true then traverse h name
    else lookup name t
;;