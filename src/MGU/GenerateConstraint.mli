(** [Unbound x] Var x is free *)
exception Unbound of LangAST.var

(** create the equation system that the mgu has to solve. *)
val equation_system : LangAST.exp -> TypeAST.ty * (TypeAST.ty * TypeAST.ty) list


