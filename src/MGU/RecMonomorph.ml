open TypeAST

exception NoTypable of TypeAST.ty * TypeAST.ty

let substitute = Type.substitute

let upto_alphaconv = function
  | (Rec(x,ty1)),(Rec(y,ty2)) -> ty1 = (Type.alpha_conv y x ty2)
  | _ -> assert false

(**
   [env] : liste contenant l'ensemble des équations (avec top-most =
   Rec) déja rencontrés

   Fonction doublement récursive ou "unify_all" unifie systeme
   d'équation et "unify" unifie une equation.

   L'idée (preuve ?) est la même que pour la relation de subtype vue
   en cours (cf : rec-type-3 slide 30).
   On peut deriver une equation (avec unify) afin d'obtenir un arbre de
   dérivation finie ou circulaire.

   AXIOME
   * substitution vide
   - Egalité syntaxique (t, t)

   - Equalité upto_alphaconv :
     Deux types (top-most = Rec(x,t)) peuvent etre egal mais en ayant
     la variable de recursion différente. On verifie cela en testant
     l'égalité syntaxique sur le membre droit de rec apres avoir
     alpha-convertie l'un des membres de l'equation.

   - équation déjà rencontré :
     Les seuls equations amenant un arbre circulaire sont les
     équations contenu dans [env] avec pour chaque membre (top-most =
     Rec) car celle-ci sont les seuls à pouvoir amener à une
     equation egal à elle-même.
     en mémoire ces équations, nous pouvons donc savoir quand nous
     sommes dans un cycle.
     ex : [(Rec(x,x -> y), Rec(z, z->z))]
          -> [(Rec(x,x->y)->y,Rec(z,z->z))]
          -> [(Rec(x,x->y)->y,Rec(z,z->z)->Rec(z,z->z))]
          -> [(Rec(x,x->y),Rec(z,z->z) ; (y, Rec(z,z->z))]

   * substitution
   - (Ground x, t)
     si x apparait dans t  { x\Rec(x,t) }
     sinon { x\t }
*)
let rec unify_all env = function
  | [] -> [], env
  | (x, y)::t ->
    let sub_tl, env = unify_all env t in
    let sub_hd, env =
      unify env ((substitute sub_tl x),
                 (substitute sub_tl y))
    in
    sub_hd @ sub_tl, env

(** [unify env (ty1, ty2)]  : substitution qui résout la contrainte *)
(** (ty1 ≐ ty2) *)
and unify env (ty1, ty2) =
  match (ty1, ty2) with
  (** AXIOME *)
  (** égalité syntaxique *)
  | (ty1, ty2) when ty1 = ty2 -> [], env
  (** equation déja rencontré *)
  | (ty1, ty2) when List.mem (ty1, ty2) env -> [], env

  (** si rec1 = rec2 à alpha_conv pres on stop *)
  | Rec(_,_), (Rec(_,_) )
    when upto_alphaconv (ty1,ty2) -> [], env

  (** Introduction de substitution si x apparait dans t on introduit
      la substitution (x, Rec(x,t)) *)
  (** sinon la substitution (x,y) *)
  | (Ground x, t) | (t, Ground x) ->
    if (Type.occurs x t) then [(x, Rec (x, t))], env
    else [(x, t)], env

  (** REGLE D'INFERENCE *)
  (** Ajout dans l'environement
      unification de (unfold rec1, rec2) *)
  | Rec(x,ty1) as rec1, (Rec(y, ty2) as rec2) ->
    let env = (rec1, rec2)::env in
    let rec1_unfold = substitute [(x, rec1)] ty1 in
    unify env (rec1_unfold, rec2)

  | Rec (x, x_t), ty
  | ty, Rec (x, x_t) ->
    let x_t = substitute [(x,Rec (x, x_t))] x_t in
    unify env (x_t, ty)

  | (Arrow (x, y), Arrow (x', y')) -> unify_all env [(x, x');(y, y')]
  | (Cross (x, y), Cross (x', y')) -> unify_all env [(x, x');(y, y')]

  (** AXIOME D'ERREUR *)
  | ty1, ty2 -> raise (NoTypable (ty1, ty2))

(** On récupère le mgu, et on applique la substitution une derniere *)
(** fois, pour tout mettre en forme résolu *)
(** mgu ne peut contenir de cycle, car tout cycle sera capturé et *)
(** remplacé pour Rec *)
(** Cette derniere passe de substitution n'est pas réellement utile *)
(** car déjà effectué par la fonction substitute *)
let mgu sys_eq =
  let mgu = fst (unify_all [] sys_eq) in
  let f_map (x,y) = x, (substitute mgu y) in
  List.map f_map mgu
