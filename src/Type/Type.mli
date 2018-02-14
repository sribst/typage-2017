open TypeAST

(** [alpha_conv x y ty] : alpha conversion of [x] to [y] in [ty] *)
val alpha_conv : ground -> ground -> ty -> ty

(** [occurs id t] : check if [id] occurs in [ty] *) 
val occurs : ground -> ty -> bool

(** [fresh_ground ()] : fresh ground type (name of type t_(i) where *)
(** 'i' is a number  *)
val fresh_ground : unit -> ty

(** [substitute sub_l ty] : sustitute in [ty] all substition in *)
(** [sub_l]. *)
(** /!\ [sub_l] dont need to be in resolved form but should not *)
(** contain cycle (ex : [(x, Var y);(y,Var x)])*)
val substitute : (ground * t) list -> ty -> ty

(** [beatify_basic ty] : replace the name of all the ground type [ty] *)
(** to more readable name (ex : t_1,t_14,..., -> 'a,'b,....,'aa,...'zz) *) 
val beautify_ground : ty -> ty

(** [refold ty] : basically fold recursive type in [ty] *)
val refold : ty -> ty

val to_string : ty -> string 
