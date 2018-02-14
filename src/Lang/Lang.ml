(* TODO /!\
   - Dans conv_all_var : peut etre amélioré en parcourant avec un
   environment et donc se passer des appels à alpha_conv qui sont
   cher
   - Dans to_string : faire un vrai PP avec les biblihotèque adéquat
*) 

open LangAST

(** Alpha conversion d'une lambda expression *)
(** parcours de l'expression, et echange de nom de variable quand on *)
(** rencontre [x]. s'arreter quand on bind la variable [x] *) 
let rec alpha_conv x y expr = 
  let rec aux = function
    | Var v when v = x -> Var y
    | Var v -> Var v 
    | Abs (v, m) when v = x -> Abs (v, m)
    | Abs (v, m) -> Abs (v, aux m)
    | Let (v, m, n) when v = x -> Let (v, aux m, n)
    | Let (v, m, n) -> Let (v, aux m, aux n)
    | Pair (m, n) -> Pair (aux m, aux n)
    | App (m, n)-> App (aux m, aux n)
    | Const f -> Const f 
  in
  aux expr

(** Creer une nouvelle string unique pour une nouvelle variable *)
let fresh_id = 
  let r = ref 0 in 
  fun unit ->
    incr r; "v_" ^ string_of_int !r

(** convertie l'ensemble des variables liées pour etre sur de ne pas *)
(** avoir des soucis de capture lors de ce travail sur ce terme. *)
let conv_all_var expr =
  let rec aux = function
    | Var x -> Var x
    | Const f -> Const f
    | Pair (m, n) -> Pair (aux m, aux n)
    | App (m, n) -> App (aux m, aux n)
    | Abs (x, m) ->
      let y = fresh_id () in
      let m = alpha_conv x y m in
      Abs(y, aux m)
    | Let (x, m, n) ->
      let y = fresh_id () in
      let n = alpha_conv x y n in
      Let(y, aux m, aux n)
  in
  aux expr

(** parenthese ou non si le niveau de profondeur le demande*)
let parent is_deep str =
  if is_deep then
    "(" ^ str ^ ")"
  else str

(** transforme une expression en string *)
(** pour une application on demande au deux sous-expressions d'etre parenthesé *)
let to_string expr =
  let rec aux is_deep = function
    | Var x -> x
    | Const (Fct f) -> f
    | Const (Bool b) -> string_of_bool b
    | Const (Int n) -> string_of_int n
    | Pair  (m, n) ->
      parent is_deep (aux false m ^ ", " ^ aux false n)
    | App (m, n)     ->
      parent is_deep (aux true m ^ " " ^ aux true n)
    | Abs (x, n)     -> 
      parent is_deep ("λ"^ x ^ "." ^ aux false n)
    | Let (x, m, n)  ->
      let str =
        "let " ^ x ^ " = " ^ aux false m ^ " in " ^ aux false n
      in
      parent is_deep str
  in
  aux false expr
