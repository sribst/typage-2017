type ty =
  | TInt | TBool
  | Ground of ground
  | Cross of ty * ty
  | Arrow of ty * ty
  | Rec of ground * ty

and ground = string
             
and t = ty
