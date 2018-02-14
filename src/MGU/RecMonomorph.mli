(** [NoTypable (ty1,ty2)] if the algorithm cannot find a substitution *)
(** for (ty1 ≐ ty2)  *)
exception NoTypable of TypeAST.t * TypeAST.t

(** [mgu sys_eq] : mgu (partial resolved form substitution) with *)
(** recursive type *)  
val mgu : (TypeAST.t * TypeAST.t) list -> (TypeAST.ground * TypeAST.t) list
