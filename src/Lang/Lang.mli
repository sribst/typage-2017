open LangAST

(** [alpha_conv x y t] : alpha conversion of variable x to the *)
(** variable y in the term t *)
val alpha_conv : var -> var -> exp -> exp 

(** [fresh_id ()] : fresh identificator of variable *)
val fresh_id : unit -> var

(** [conv_all_var expr] create fresh variable for all binded *)
(** variable in [expr] (and [alpha_conv] them). *)
val conv_all_var : exp -> exp

val to_string : exp -> string
