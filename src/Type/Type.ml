(*TODO /!\
  - dans beautify_ground : pourquoi j'ai pas utilisé un env || alpha_conv ? 
  deux parcours, pas tres optimal...
  - dans to_string : de même que pour Lang devrait etre un PP 
*)

open TypeAST

(** Alpha conversion d'une type *)
(** parcours du type, et echange de nom de variable quand on *)
(** rencontre [x]. s'arreter quand un binder (Rec) bind la *)
(** variable [x] *)
let alpha_conv x y ty =
  let rec aux = function
    | TInt | TBool as t -> t
    | Ground v when v = x -> Ground y
    | Ground v -> Ground v 
    | Cross (x, y) -> Cross (aux x, aux y)
    | Arrow (f, x) -> Arrow (aux f, aux x)
    | Rec (v, f) when v = x -> Rec (v, f)
    | Rec (v, f) -> Rec (v, aux f)
  in
  aux ty

(** check si la variable de type [x] apparait dans [ty] *) 
let occurs x ty =
  let rec aux = function
    | Ground y -> y = x
    | Arrow (t1, t2)
    | Cross (t1, t2) -> aux t1  || aux t2
    | Rec   (y , t ) ->
      if y = x then false
      else aux t
    | TInt | TBool -> false
  in
  aux ty

(** creer une nouvelle string unique pour une nouvelle variable de *)
(** type *)
let fresh_ground =
  let r = ref 0 in
  fun unit ->
    incr r; Ground ("t_" ^ string_of_int !r)

(** applique l'ensemble des substitutions de [sub] au type donné en argument*)
let rec substitute sub ty =
  match ty with 
  | Arrow (t1, t2) -> Arrow (substitute sub t1, substitute sub t2)
  | Cross (t1, t2) -> Cross (substitute sub t1, substitute sub t2)
  | TBool | TInt as t -> t
  | Rec   (x , t ) ->
    (** On enleve x de la liste de substitution à appliquer car Rec *)
    (** bind x pour toutes sous-formules du type à traiter *) 
    Rec (x, substitute (List.remove_assoc x sub) t)
  | Ground v ->
    (** si [v] est une variable à substituer, on applique de nouveau *)
    (** la function de substitution sur le type substitué à [v]. *)
    (** Cela est utile car sub peut ne pas en forme résolue *)
    try
      let ty = List.assoc v sub in
      substitute sub ty
    with _ -> Ground v

(** creer une nouvelle string unique pour une nouvelle variable de *)
(** type. Le nom généré est plus lisible que la fonction *)
(** "fresh_ground" mais limité en nombre (ici max deux lettres 'aa) *)
(** let new_id = clean_id () pour "initialisé" les noms de type *)
(** new_id () pour une nouvelle variable *)
let clean_id () =
  let r1 = ref ((Char.code 'a') -1) in
  let r2 = ref ((Char.code 'a') -1) in
  fun unit ->
    incr r2;
    if (Char.chr !r2) = 'z' then (r2 := (Char.code 'a'); incr r1);
    let letter =
      let first =
        if !r1 >= (Char.code 'a') then String.make 1 (Char.chr !r1)
        else ""
      in
      first ^ (String.make 1 (Char.chr !r2))
    in
    ("'" ^ letter)

(** transforme l'ensemble des variables de type avec clean_id pour *)
(** etre plus lisible (en esperant ne pas dépasser 24 * 24 noms) *)
let beautify_ground t =
  let clean_id = clean_id () in 
  let rec create_sub sub = function
    | TInt  -> []
    | TBool -> []
    | Ground x ->
      if List.mem_assoc x sub then sub else (x, clean_id ())::sub
    | Cross (m, n)
    | Arrow (m, n) ->
      let sub = create_sub sub m in
      create_sub sub n
    | Rec (x,m) ->
      create_sub ((x, clean_id ())::sub) m
  in
  let rec aux sub = function
    | Ground v -> Ground (List.assoc v sub)
    | Arrow (t1, t2) -> Arrow (aux sub t1, aux sub t2)
    | Cross (t1, t2) -> Cross (aux sub t1, aux sub t2)
    | Rec (x, t) -> Rec (List.assoc x sub, aux sub t)
    | TBool | TInt as t -> t
  in
  aux (create_sub [] t) t

(** refolding de quelques cas de rec-type. *)
(** refold tant que le type replié est différent de l'entrée *)
let rec refold t =
  let rec aux = function
    (** (μx.x -> t) -> t *)
    | Arrow (Rec (x, Arrow (Ground y, t1)), t2)
      when x = y && t1 = t2 ->
      Rec (x, Arrow(Ground x, t2))
    (** t->(μx.t -> x) *)
    | Arrow (t1, Rec (x, Arrow (t2,Ground y)))
      when x = y && t1 = t2 ->
      Rec (x, Arrow(t2, Ground x))
    (** (μx.x * t) * t *)
    | Cross (Rec (x, Cross (Ground y, t1)), t2)
      when x = y && t1 = t2 ->
      Rec (x, Cross(Ground x, t2))
    (** t * (μx.x * t) *)
    | Cross (t1, Rec (x, Cross (t2,Ground y)))
      when x = y && t1 = t2 ->
      Rec (x, Cross(t2, Ground x))
    | Arrow (t1, t2) ->
      Arrow (aux t1, aux t2)
    | Cross (t1, t2) ->
      Cross (aux t1, aux t2)
    | _ as t -> t
  in
  let t' = aux t in
  if t' = t then t'
  else refold t'

(** parenthese ou non si le niveau de profondeur le demande*)
let parent is_deep str =
  if is_deep then
    "(" ^ str ^ ")"
  else str

(** transforme un type en string  *)
(** pour une arrow on demande au sous-type de gauche d'etre parenthesé *)
let to_string ty =
  let rec aux is_deep = function
    | TInt    -> "int"
    | TBool   -> "bool"
    | Ground x -> x
    | Cross (m, n) ->
      parent is_deep (aux false m ^ "*" ^ aux false n)
    | Arrow (m, n) ->
      parent is_deep (aux true m ^ " -> " ^ aux false n)
    | Rec (x, m)  ->
      parent is_deep ("μ" ^ x ^ "." ^ aux true m)
  in
  aux false ty
