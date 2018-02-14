type exp =
  | Var   of var 
  | Const of const
  | Pair  of exp * exp
  | App   of exp * exp
  | Abs   of var * exp
  | Let   of var * exp * exp

and const = 
   | Fct  of string
   | Bool of bool
   | Int  of int 

and var = string 


and t = exp  
