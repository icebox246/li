let ifile = "./test.li"

let string_of_char c =
    String.make 1 c

type location = {
    file : string;
    line : int;
    col : int;
}

let string_of_location loc = 
    loc.file ^ ":" ^
    string_of_int loc.line ^ ":" 
    ^ string_of_int loc.col

type value =
    | Null
    | Int of int
    | String of string
    | Bool of bool

type operator =
    | Add
    | Mult
    | Sub
    | Div
    | Eq

type token =
    | OpenParenToken
    | CloseParenToken
    | OpenCurlyToken
    | CloseCurlyToken
    | VarToken
    | SetToken
    | OpToken of operator
    | InlineOpToken of operator
    | PrintToken
    | ValueToken of value
    | LabelToken of string
    | IfToken
    | ElseToken


exception TokenizationError of string

let string_to_list s =
    String.fold_right (fun c l -> c :: l) s []

let is_numeric c = 
    match c with
    | '0' .. '9' -> true
    | _ -> false

let is_whitespace c =
    match c with
    | ' ' | '\t' | '\n' -> true
    | _ -> false

let is_op_char c = 
    match c with
    | '+' | '-' | '/' | '*' | '=' | ':' -> true
    | _ -> false

let is_paren_token c =
    match c with
    | '(' | ')' -> true
    | _ -> false

let is_word_char c =
    match c with
    | c when not (is_op_char c || is_whitespace c || is_paren_token c) -> true
    | _ -> false

let rec list_to_string l = 
    match l with
    | [] -> ""
    | c :: r -> string_of_char c ^ list_to_string r

let move_location_by_cols loc cols =
    { file=loc.file; line=loc.line; col=loc.col + cols }

let rec parse ic loc =
   match input_line ic with
   | exception End_of_file -> []
   | line -> 
           let rec parse_int b chars =
               match chars with
               | c :: r when is_numeric c ->
                       parse_int (b ^ string_of_char c) r
               | _ -> (ValueToken (Int (int_of_string b)),chars, String.length b)
           in
           let rec parse_word b chars =
               match chars with
               | c :: r when is_word_char c ->
                       parse_word (b ^ string_of_char c) r
               | _ -> ((match b with
                        | "let" -> VarToken
                        | "print" -> PrintToken
                        | "if" -> IfToken
                        | "else" -> ElseToken
                        | word -> LabelToken word
                       ), chars, String.length b)
           in
           let rec parse_op b chars =
               match chars with
               | c :: r when is_op_char c ->
                       parse_op (b ^ string_of_char c) r
               | _ -> ((match b with
                        | ":=" -> SetToken
                        | "+" -> OpToken Add
                        | "*" -> OpToken Mult
                        | "-" -> OpToken Sub
                        | "/" -> OpToken Div
                        | "==" -> OpToken Eq
                        | "+=" -> InlineOpToken Add
                        | "*=" -> InlineOpToken Mult
                        | "-=" -> InlineOpToken Sub
                        | "/=" -> InlineOpToken Div
                        | "===" -> InlineOpToken Eq
                        | op -> raise @@ TokenizationError ("unknown operator `" ^ op ^ "`")
                       ), chars, String.length b) 
           in
           let rec parse_string b chars =
               match chars with
               | '"' :: r -> (ValueToken (String b),r,String.length b + 1)
               | c :: r -> parse_string (b ^ string_of_char c) r
               | [] -> raise @@ TokenizationError "got eof before string end!"
           in
           let rec parse_c chars loc = 
               match chars with
               | [] -> []
               | c :: r when is_whitespace c -> parse_c r (move_location_by_cols loc 1)
               | c :: r when is_op_char c -> let (op,r,len) = parse_op "" chars in 
                                                 (op,loc) :: parse_c r (move_location_by_cols loc len)
               | c :: r when is_numeric c -> let (num,r,len) = parse_int "" chars in 
                                                 (num,loc) :: parse_c r (move_location_by_cols loc len)
               | '"' :: r -> let (str,r,len) = parse_string "" r in (str,loc) :: parse_c r (move_location_by_cols loc len)
               | '(' :: r -> (OpenParenToken, loc) :: parse_c r (move_location_by_cols loc 1)
               | ')' :: r -> (CloseParenToken, loc) :: parse_c r (move_location_by_cols loc 1)
               | '{' :: r -> (OpenCurlyToken, loc) :: parse_c r (move_location_by_cols loc 1)
               | '}' :: r -> (CloseCurlyToken, loc) :: parse_c r (move_location_by_cols loc 1)
               | _ -> let (token,r,len) = parse_word "" chars in 
                          (token, loc) :: parse_c r (move_location_by_cols loc len)
            in
            (parse_c (string_to_list line) loc) @ (parse ic { file=loc.file; line = loc.line + 1; col = 1 })

let string_of_token token = 
    match token with
    | PrintToken -> "<print>"
    | OpenParenToken -> "<(>"
    | CloseParenToken -> "<)>"
    | OpenCurlyToken -> "<{>"
    | CloseCurlyToken -> "<}>"
    | VarToken -> "<var>"
    | ValueToken (String s) -> ("<String \"" ^ s ^ "\">")
    | ValueToken (Int n) -> ("<Int " ^ (string_of_int n) ^ ">")
    | ValueToken (Bool b) -> ("<Int " ^ (string_of_bool b) ^ ">")
    | ValueToken (Null) -> "<Null>"
    | SetToken -> "<set>"
    | OpToken Add -> "<+>"
    | OpToken Sub -> "<->"
    | OpToken Mult -> "<*>"
    | OpToken Div -> "</>"
    | OpToken Eq -> "<==>"
    | InlineOpToken Add -> "<+=>"
    | InlineOpToken Sub -> "<-=>"
    | InlineOpToken Mult -> "<*=>"
    | InlineOpToken Div -> "</=>"
    | InlineOpToken Eq -> "<===>"
    | LabelToken s -> ("<Label " ^ s ^ ">")
    | IfToken -> "<if>"
    | ElseToken -> "<else>"


exception CompilationError of string

type expression =
    | ValueEx of value
    | LabelEx of string
    | OpEx of expression * operator * expression

let rec compile_expr tokens vars = 
    let rec check_var name vars =
        match vars with
        | [] -> raise @@ CompilationError ("undefined variable `" ^ name ^ "`")
        | v :: r -> ( match Hashtbl.find_opt v name with
                      | Some _ -> ()
                      | None -> check_var name r
                    )
    in
    let (lhs, tokens) = 
        match tokens with
        | (ValueToken v,_) :: r -> (ValueEx v, r)
        | (LabelToken n,_) :: r -> check_var n vars; (LabelEx n, r)
        | (OpenParenToken, _) :: r -> (let (e,r) = compile_expr r vars in 
                                        match (e,r) with
                                        | (e, (CloseParenToken,_) :: r) -> (e,r)
                                        | _ -> raise @@ CompilationError ("missing `)`")
                                 )
        | (t, l) :: _ -> raise @@ CompilationError ("invalid expression: " ^ string_of_location l ^ ": " ^ string_of_token t)
        | [] -> raise @@ CompilationError ("reached eof while compiling expression")
    in
    match tokens with
    | (OpToken o,_) :: r -> 
            let (rhs,tokens) = compile_expr r vars in (OpEx (lhs, o, rhs),tokens)
    | _ -> (lhs, tokens)

type statement =
    | VarDeclare of string
    | SetVariable of string * expression
    | PrintValue of expression
    | ScopeBlock of statement list
    | IfStatement of expression * statement list
    | IfElseStatement of expression * statement list * statement list

let (@<) a (b,c) =
    (a @ b, c)

type compile_var_type =
    | Variable
    | Function of int

let compile tokens =
    let push_new_scope vars =
        Hashtbl.create 128 :: vars
    in
    let rec compile tokens is_global single vars =
       let compile_tail rst =
           if single   then ([],rst)
                       else compile rst is_global single vars
       in
       let define_var name typ =
           let vars = List.hd vars in
           match Hashtbl.find_opt vars name with
           | Some _ -> raise @@ CompilationError ("redeclaration of `" ^ name ^ "`")
           | None -> Hashtbl.add vars name typ
       in
       let rec check_var name vars =
           match vars with
           | [] -> raise @@ CompilationError ("undefined variable `" ^ name ^ "`")
           | v :: r -> ( match Hashtbl.find_opt v name with
                         | Some _ -> ()
                         | None -> check_var name r
                       )
       in
       match tokens with
       | [] when is_global -> ([],[])
       | [] -> raise @@ CompilationError ("scope wasn't closed before eof")
       | (CloseCurlyToken,l) :: rst when not is_global ->
               ([],rst)
       | (VarToken,l) :: (LabelToken name,_) :: (SetToken,_) :: rst -> 
               define_var name Variable;
               let (e,rst) = compile_expr rst vars in
                   [VarDeclare name; SetVariable (name,e)] @< compile_tail rst;
       | (VarToken,l) :: (LabelToken name,_) :: rst -> 
               define_var name Variable;
                   [VarDeclare name] @< compile_tail rst
       | (LabelToken name,l) :: (SetToken,_)  :: rst -> 
               check_var name vars;
               let (e,rst) = compile_expr rst vars in
                   [SetVariable (name,e)] @< compile_tail rst
       | (LabelToken n1,l) :: (InlineOpToken o,_) :: rst -> 
               let (e,rst) = compile_expr rst vars in
                   [SetVariable (n1,OpEx(LabelEx n1,o,e))] @< compile_tail rst
       | (PrintToken,l)  :: rst -> 
               let (e,rst) = compile_expr rst vars in
                   [PrintValue e] @< compile_tail rst
       | (OpenCurlyToken,l) :: rst ->
               let (sts,rst) = compile rst false false (push_new_scope vars) in
                   [ScopeBlock sts] @< compile_tail rst
       | (IfToken,l) :: rst ->
               let (e,rst) = compile_expr rst vars in
               let (sts,rst) = compile rst false true (push_new_scope vars) in
               (   match rst with
                   | (ElseToken,_) :: rst -> ( let (ests,rst) = compile rst false true (push_new_scope vars) in
                                               [IfElseStatement (e,sts,ests)] @< compile_tail rst
                                             )
                   | _ -> [IfStatement (e,sts)] @< compile_tail rst
               )
       | (t,l) :: _ -> 
               raise @@ 
                   CompilationError ("unexpected token: " ^ string_of_location l ^ " " ^ string_of_token t)
    in
    let (a,_) = compile tokens true false (push_new_scope []) in a

exception EvaluationException of string

let rec evaluate program vars =
    let vars = Hashtbl.create 128 :: vars in
    let rec get_var name vars =
        match vars with
            | v :: r -> ( match Hashtbl.find_opt v name with
                          | Some v -> v 
                          | None -> get_var name r
                        )
            | [] -> raise @@ EvaluationException ("unreachable! : undeclared variable `" ^ name ^ "`")
        in

    let declare_var name = 
        let vars = List.hd vars in
            Hashtbl.add vars name Null 
        in

    let rec set_var name va vars =  
        match vars with
            | v :: r -> ( match Hashtbl.find_opt v name with
                          | Some _ ->  Hashtbl.replace v name va
                          | None -> set_var name va r
                        )
            | [] -> raise @@ EvaluationException ("unreachable! : undeclared variable `" ^ name ^ "`")
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
        | _ -> raise @@ EvaluationException "mismatch types in `mult`"
        in

    let sub_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Int (v1 - v2)
        | _ -> raise @@ EvaluationException "mismatch types in `sub`"
        in


    let div_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Int (v1 / v2)
        | _ -> raise @@ EvaluationException "mismatch types in `div`"
        in

    let eq_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Bool (v1 = v2)
        | (String v1, String v2) ->  Bool (v1 = v2)
        | (Bool v1, Bool v2) ->  Bool (v1 = v2)
        | _ -> raise @@ EvaluationException "mismatch types in `eq`"
        in


    let print_value v = 
        match v with
        | Null -> print_string "NULL"; print_newline ()
        | Int v -> print_int v; print_newline ()
        | String  v -> print_string v; print_newline ()
        | Bool  b -> print_string @@ string_of_bool b; print_newline ()
        in

    let rec eval_expr expr =
        match expr with
        | ValueEx v -> v
        | LabelEx n -> get_var n vars
        | OpEx (l,o,r) ->
                match o with
                | Add -> add_values (eval_expr l) (eval_expr r)
                | Mult -> mult_values (eval_expr l) (eval_expr r)
                | Sub -> sub_values (eval_expr l) (eval_expr r)
                | Div -> div_values (eval_expr l) (eval_expr r)
                | Eq -> eq_values (eval_expr l) (eval_expr r)
        in

    let eval stat = 
        match stat with
        | VarDeclare name -> declare_var name
        | SetVariable (name, e) -> set_var name (eval_expr e) vars
        | PrintValue e -> print_value (eval_expr e)
        | ScopeBlock sts -> evaluate sts vars
        | IfStatement (e,sts) -> (  match eval_expr e with
                                    | Bool true -> evaluate sts vars
                                    | Bool false -> ()
                                    | _ -> raise @@ EvaluationException "expected bool value in `if`"
                                 )   
        | IfElseStatement (e,sts,ests) -> 
                                 (  match eval_expr e with
                                    | Bool true -> evaluate sts vars
                                    | Bool false -> evaluate ests vars
                                    | _ -> raise @@ EvaluationException "expected bool value in `if`"
                                 )
        in

    List.iter eval program 

(*====================*)   

let () =
    let ic = open_in ifile in
    let tokens = parse ic {file = ifile; line = 1; col = 1} in
    List.iter (fun (token,loc) -> print_endline @@ string_of_location loc ^ ": " ^ string_of_token token ) tokens;
    close_in ic;
    let program = compile tokens in
    evaluate program []

