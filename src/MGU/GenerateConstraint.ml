open LangAST
open TypeAST


(** quand une variable n'est pas liée *)
exception Unbound of var

(** pour pas à ravoir à taper la function à chaques fois*)
let fresh_ground = Type.fresh_ground

(** prototype de type des fonctions/constantes *)
let fct_type =
  [("fst",
    let p1 = fresh_ground () in
    let p2 = fresh_ground () in 
    Arrow (Cross(p1, p2), p1)) ;
   ("snd",
    let p1 = fresh_ground () in
    let p2 = fresh_ground () in 
    Arrow (Cross(p1, p2), p2)) ;
   ("fix",
    let t = fresh_ground () in
    Arrow (Arrow  (t, t),t));
   ("if" ,
    let c = fresh_ground () in
    let t = fresh_ground () in
    Arrow (Cross (c, Cross(t, t)),t));
   ("+"  , Arrow (Cross (TInt, TInt), TInt)) ;
   ("-"  , Arrow (Cross (TInt, TInt), TInt)) ;
   ("/"  , Arrow (Cross (TInt, TInt), TInt)) ;
   ("*"  , Arrow (Cross (TInt, TInt), TInt)) ;
  ]

(** génération de contrainte de type de l'expression *)
(**
   [aux env sys_eq expected expr] 
   [env] : list associative contenant l'ensemble des variables liés dans
   ce scope et les types générés des variables
   [sys_eq] : equations générées jusqu'a maintenant
   [expected] : type attendu de l'expression, venue de "plus haut"
   [expr] : expression dont on veut générer les contraintes

   l'environement de base est les prototypes de type des fonctions.

   Var x : Si x n'est pas lié [raise Unbound] 
         On ajoute la contrainte (x_t ≐ expected)
   Const c : on ajoute la contrainte (STC c ≐ expected)
   Pair(M,N) : 
         on introduit un type M_t (resp. N_t) et on génère les
         contrainte de M (resp. N) avec M_t (resp. N_t)
         on ajoute la contrainte (M_t * N_t ≐ expected)
   App(M, N) : 
        on introduit un type N_t.
        On génère les contraintes de M avec (N_t -> expected).
        on génère les contrainte de N avec N_t.
   Abs(x,M) : 
       on introduit un type x_t que l'on rajoute à l'environement
       (x,x_t), et un type m_t. 
       On génère les contraintes de M avec m_t. 
       On ajoute la contrainte (x_t -> m_t ≐ expected)
   Let(x,M,N) : 
       on introduit x_t.
       on génère les contraintes de M avec m_t. 
       on ajoute (x,x_t) a l'environement (que maintenant au cas ou x
         était déjà bindé precedement)
       on genere les contrainte de N avec expected 
*)
let equation_system p =
  let rec aux env sys_eq expected = function
    | Var x ->
      let x_t =
        try (List.assoc x env)
        with _ -> raise (Unbound x)
      in
      (x_t, expected)::sys_eq
    | Const (Fct f) -> (List.assoc f env, expected)::sys_eq
    | Const (Bool _) -> (TBool, expected)::sys_eq
    | Const (Int _) -> (TInt, expected)::sys_eq
    | Pair (m, n) ->
      let m_t = fresh_ground () in
      let n_t = fresh_ground () in
      let sys_eq = aux env sys_eq m_t m  in
      let sys_eq = aux env sys_eq n_t n  in
      (Cross(m_t, n_t),expected)::sys_eq
    | App(func, arg) ->
      let arg_t = fresh_ground () in
      let func_t = Arrow(arg_t,expected) in
      let sys_eq = aux env sys_eq func_t func in
      aux env sys_eq arg_t arg
    | Abs(x, m) ->
      let x_t = fresh_ground () in
      let m_t = fresh_ground () in
      let env    = (x, x_t)::env        in
      let sys_eq = aux env sys_eq m_t m in
      (Arrow(x_t, m_t), expected)::sys_eq
    | Let (x, m, n)  ->
      let x_t    = fresh_ground ()  in
      let sys_eq = aux env sys_eq x_t m in
      let env    = (x, x_t)::env        in
      aux env sys_eq expected n
  in
  let final_type = fresh_ground () in
  final_type, (aux fct_type [] final_type p)
