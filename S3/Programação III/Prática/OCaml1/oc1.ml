(*  #use "pr6.ml";;  *)
(* 1 *)
let rec membro = function
    [] -> (function _ -> false)
    | x::y -> function z -> if z=x then true else membro y z;;

(* 2 *)
let rec conta x = function
    [] -> 0
    | y::z -> (if x=y then 1 else 0) + conta x z;;

(* 3 *)
let rec clone = function
    [] -> []
    | a::b -> a :: clone b;;

let rec clown = function
    [] -> (function x -> [x])
    | a::b -> (function x -> a+x :: clown b x);;
    
let rec append = function
    [] -> (function x -> x)
    | a::b -> function x -> a :: append b x;;

(* 4 *)
let rec soma_lista = function
    [] -> 0
    | x::y -> x + soma_lista y;;
    
(* 5 *)
let rec remove x = function
    [] -> []
    | y::z -> if y=x then z else y :: remove x z;;
    (* Se a cabeça for igual ao elemento a remover(primeira ocorrência) então 
    continua na continuação da lista, caso não o seja então inicia a recorrência 
    até remover o primeiro elemento*)
    
let rec remove_tudo x = function
    [] -> []
    | y::z -> (if y=x then z else y) :: remove_tudo x z;;

(* 6 *)
let rec contagem = function
    | [] -> []
    | x::y -> (x, (conta x y) + 1) :: (contagem (remove_tudo x y));;