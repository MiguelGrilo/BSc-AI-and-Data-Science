type 'a abp = 
    None
    | No of ('a * 'a abp * 'a abp)
(* 1 *)
let rec lookup = function
    None -> (function _ -> false)
    | (No (key, left, right)) -> (function z -> if z = key then true else lookup (if z < key then left else right) z);;

(* 2 *)
let rec insert = function
    None -> (function x -> No(x, None, None))
    | (No(key, left, right)) -> (function x -> if x = key then No (key, left, right)
        else if x < key then No (key, insert left x, right) else No (key, left, insert right x));;

(* 3 *)
let rec replace_leftmost = function
    | None -> (function x -> x)
    | (No(key, left, right)) -> (function x -> No(key, (replace_leftmost left x), right));;

let rec delete = function
    | None -> (function _ -> None)
    | (No(key, left, right)) -> (function x -> if x < key then No(key, (delete left x), right)
        else if x > key then No(key, left, (delete right x))
        else replace_leftmost right left);;

(* 4 *)
let rec walk : 'a abp -> ('a -> unit) -> unit = function
(* OU  val walk : 'a abp -> ('a -> 'b) -> unit = <fun> *)
    | None -> (function _ -> ())
    | (No(key, left, right)) ->
        (function f -> walk left f;
        f key;
        walk right f);;