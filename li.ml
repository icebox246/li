let ifile = "test.li"

type location = {
    line : int;
}

let string_of_location loc = 
    "line:" ^ string_of_int loc.line

let read_words ic =
    let rec read_by_line ic ln =
        match input_line ic with
        | "" -> read_by_line ic (ln+1)
        | line -> List.map (fun x -> (x, {line=ln})) 
            (String.split_on_char ' ' line) @ read_by_line ic (ln+1)
        | exception End_of_file -> []
    in
    read_by_line ic 1

type value =
    | Null
    | Int of int
    | String of string

type operator =
    | Add
    | Mult
    | Sub
    | Div

type token =
    | OpenToken
    | CloseToken
    | VarToken
    | SetToken
    | OpToken of operator
    | InlineOpToken of operator
    | PrintToken
    | ValueToken of value
    | LabelToken of string


exception TokenizationError of string

let rec tokenize l = 
    let rec take_until_string_end b l =
        match l with
        | [] -> raise @@ TokenizationError "got eof before string end!"
        | (s,_) :: r 
            when ((String.length s > 1 || b != "") 
                    && s.[String.length s - 1] = '"') ->
                (b ^ " " ^ s, r)
        | (s,_) :: r -> take_until_string_end (b ^ " " ^ s) r 
        in
    match l with
    | [] -> []
    | ("",_) :: r -> tokenize r
    | (s,l) :: r when (s.[0] = '"') ->
            let (s, r) = take_until_string_end "" ((s,l)::r) in 
            (ValueToken (String (String.sub s 2 (String.length s - 3))) , l) 
            :: tokenize r
    | (v,l) :: r -> ((
        match v with 
        | "(" -> OpenToken
        | ")" -> CloseToken
        | "let" -> VarToken
        | ":=" -> SetToken
        | "+" -> OpToken Add
        | "*" -> OpToken Mult
        | "-" -> OpToken Sub
        | "/" -> OpToken Div
        | "+=" -> InlineOpToken Add
        | "*=" -> InlineOpToken Mult
        | "-=" -> InlineOpToken Sub
        | "/=" -> InlineOpToken Div
        | "print" -> PrintToken
        | _ -> match int_of_string v with
                | n -> ValueToken (Int n)
                | exception _ -> LabelToken v
        ), l)
        :: tokenize r 


exception CompilationError of string

type expression =
    | ValueEx of value
    | LabelEx of string
    | OpEx of expression * operator * expression

let rec compile_expr tokens = 
    let (lhs, tokens) = 
        match tokens with
        | (ValueToken v,_) :: r -> (ValueEx v, r)
        | (LabelToken n,_) :: r -> (LabelEx n, r)
        | (OpenToken, _) :: r -> (let (e,r) = compile_expr r in 
                                        match (e,r) with
                                        | (e, (CloseToken,_) :: r) -> (e,r)
                                        | _ -> raise @@ CompilationError ("missing `)`")
                                 )
        | (_, l) :: _ -> raise @@ CompilationError ("invalid expression: " ^ string_of_location l)
        | [] -> raise @@ CompilationError ("reached eof while compiling expression")
    in
    match tokens with
    | (OpToken o,_) :: r -> 
            let (rhs,tokens) = compile_expr r in (OpEx (lhs, o, rhs),tokens)
    | _ -> (lhs, tokens)

type statement =
    | VarDeclare of string
    | SetVariable of string * expression
    | PrintValue of expression

let rec compile tokens =
   match tokens with
   | [] -> []
   | (VarToken,l) :: (LabelToken name,_) :: (SetToken,_) :: rst -> 
           let (e,rst) = compile_expr rst in
               [VarDeclare name; SetVariable (name,e)] @ compile rst
   | (VarToken,l) :: (LabelToken name,_) :: rst -> 
           VarDeclare name :: compile rst
   | (LabelToken name,l) :: (SetToken,_)  :: rst -> 
           let (e,rst) = compile_expr rst in
               SetVariable (name,e) :: compile rst
   | (LabelToken n1,l) :: (InlineOpToken o,_) :: rst -> 
           let (e,rst) = compile_expr rst in
               SetVariable (n1,OpEx(LabelEx n1,o,e)) :: compile rst
   | (PrintToken,l)  :: rst -> 
           let (e,rst) = compile_expr rst in
               PrintValue e :: compile rst
   | (_,l) :: _ -> 
           raise @@ 
           CompilationError ("unknown expression: " ^ string_of_location l)

exception EvaluationException of string

let evaluate program =
    let vars = Hashtbl.create 128 in
    let get_var name =
        match Hashtbl.find_opt vars name with
        | Some v -> v 
        | None -> raise @@ EvaluationException ("undeclared variable `" ^ name ^ "`")
        in

    let declare_var name = 
        match get_var name with
        | _ -> raise @@ EvaluationException ("redeclaration of `" ^ name ^ "`")
        | exception EvaluationException _ -> Hashtbl.add vars name Null 
        in

    let set_var name v = let _ = get_var name in Hashtbl.replace vars name v 
        in

    let add_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Int (v1 + v2)
        | (String v1, String v2) ->  String (v1 ^ v2)
        | _ -> raise @@ EvaluationException "mismatch types in `add`"
        in

    let mult_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Int (v1 * v2)
        | _ -> raise @@ EvaluationException "mismatch types in `mult"
        in

    let sub_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Int (v1 - v2)
        | _ -> raise @@ EvaluationException "mismatch types in `sub"
        in


    let div_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Int (v1 / v2)
        | _ -> raise @@ EvaluationException "mismatch types in `div"
        in


    let print_value v = 
        match v with
        | Null -> print_string "NULL"; print_newline ()
        | Int v -> print_int v; print_newline ()
        | String  v -> print_string v; print_newline ()
        in

    let rec eval_expr expr =
        match expr with
        | ValueEx v -> v
        | LabelEx n -> get_var n
        | OpEx (l,o,r) ->
                match o with
                | Add -> add_values (eval_expr l) (eval_expr r)
                | Mult -> mult_values (eval_expr l) (eval_expr r)
                | Sub -> sub_values (eval_expr l) (eval_expr r)
                | Div -> div_values (eval_expr l) (eval_expr r)
        in

    let eval stat = 
        match stat with
        | VarDeclare name -> declare_var name
        | SetVariable (name, e) -> set_var name (eval_expr e)
        | PrintValue e -> print_value (eval_expr e)
        in

    List.iter eval program 

(*====================*)   

let () =
    let ic = open_in ifile in
    let words = read_words ic in
    close_in ic;
    let tokens = tokenize words in
    let program = compile tokens in
    evaluate program

