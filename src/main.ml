(** PARSEUR *) 
let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:Lexer.token
    ~parser_fun:Parser.program
    ~input

let parse_string = parse Lexing.from_string


(** PRINT *) 
let print_expr_type expr ty =
  Printf.printf "\n%s : %s\n%!"
    (Lang.to_string expr)
    (* (Type.to_string  ty); *)
    (Type.to_string (Type.beautify_ground ty));
  Printf.printf "\n----------------------------------------------\n"

let print_expr expr =
  Printf.printf "______________________________________________\n";
  Printf.printf "\n%s\n\n%!" (Lang.to_string expr)

let print_type ty =
  Printf.printf "\n%s\n%!" (Type.to_string ty)

let print_sys_eq sys_eq final_ty =
  Printf.printf "[DEBUG] CONTRAINTE %s \n"
    (Type.to_string final_ty) ;
  let f_iter (x, y) =
    Printf.printf "    %s ≐ %s ; \n%!" (Type.to_string x) (Type.to_string y)
  in
  Printf.printf ("{\n%!");  
  List.iter f_iter sys_eq;
  Printf.printf ("}\n%!")
    
let print_unification uni =
  Printf.printf ("[DEBUG] SUBSTITUTION\n%!");
  let f_iter (x, y) =
    Printf.printf "    %s \\ %s ; \n%!"  x (Type.to_string y)
  in
  Printf.printf ("{\n%!");
  List.iter f_iter uni;
  Printf.printf ("\n}\n%!")


(**MGU  *) 

let process debug mgu expr =
  print_expr expr;
  let clean_expr = Lang.conv_all_var expr in
  Printf.printf "renaming var done\n%!";

  try
    let final_ty, sys_eq =
      GenerateConstraint.equation_system clean_expr 
    in
    Printf.printf "Constraint generated\n%!";
    if debug then print_sys_eq sys_eq final_ty;

    let sub = mgu sys_eq in
    Printf.printf "substitution generated\n%!";
    if debug then print_unification sub ;

    let final_ty = Type.substitute sub final_ty in
    Printf.printf "substitution done\n%!";
    if debug then print_type final_ty ;

    let final_ty = Type.refold final_ty in
    Printf.printf "folding rec type done\n%!";

    print_expr_type expr final_ty
      
  with
    GenerateConstraint.Unbound x ->
    Printf.printf "Unbound variable %s\n%!" x 
  | RecMonomorph.NoTypable (x,y) ->
    Printf.printf
      "unification algorithm failed at : \n    %s ≐ %s \n%!"
      (Type.to_string x) (Type.to_string y)


(**LOOP interpreter  *)
      
let rec loop debug mgu =
  Printf.printf "=> %!";
  match (input_line stdin) with
  | "exit" -> exit 0
  | "debug" -> loop (not debug) mgu
  | str    ->
    (try
       process debug mgu (parse_string str)
     with Error.Error (positions,msg) ->
       Printf.printf "%s" (Error.print_error positions msg));
    loop debug mgu

(** print help + lance loop *) 
let top ()  =
  Printf.printf "
Language :
   Application  : f x
   Abstraction  : fun x -> y
   let in       : let x = m in y
   pair         : m, n

function :
  --------------------------------
  | \"+\"   | \"-\"   | \"/\"   | \"*\"  |
  --------------------------------
  | \"fst\" | \"snd\" | \"fix\" | \"if\" |
  --------------------------------

toggle debug mode   : \"debug\"\n ";
  loop false RecMonomorph.mgu

let rec from_file debug mgu file =
  try
    let str = Pervasives.input_line file in
    let expr = parse_string str in
    process debug mgu expr;
    from_file debug mgu file
  with End_of_file -> (print_endline "Done";exit 0)

let _ =
  let args = List.tl (Array.to_list Sys.argv) in
  match args  with
  | [] 
  | "-I"::_ -> Error.resume_on_error () ; top ()
  | "-S"::fn::debug::[] ->
    from_file (bool_of_string debug) RecMonomorph.mgu (open_in fn)
  | _ ->
    Printf.printf "
\"-I\" -> top (by defaults)
\"-S\" (rec|mon) filename debug -> use file
the file should have one formula per ligne
debug must be a boolean"
