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
    | NotEq
    | Lt
    | Gt
    | LtEq
    | GtEq

type token =
    | OpenParenToken
    | CloseParenToken
    | OpenCurlyToken
    | CloseCurlyToken
    | VarToken
    | SetToken
    | OpToken of operator
    | InlineOpToken of operator
    | ValueToken of value
    | LabelToken of string
    | IfToken
    | ElseToken
    | FunctionToken
    | ArrowToken
    | DollarToken


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
    | '+' | '-' | '/' | '*' | '=' | ':' | '>' | '<' | '$' | '!' -> true
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
                        (* | "print" -> PrintToken *)
                        (* | "println" -> PrintLnToken *)
                        | "if" -> IfToken
                        | "else" -> ElseToken
                        | "fn" -> FunctionToken
                        | "true" -> ValueToken (Bool true)
                        | "false" -> ValueToken (Bool false)
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
                        | "<" -> OpToken Lt
                        | ">" -> OpToken Gt
                        | "<=" -> OpToken LtEq
                        | ">=" -> OpToken GtEq
                        | "==" -> OpToken Eq
                        | "!=" -> OpToken NotEq
                        | "+=" -> InlineOpToken Add
                        | "*=" -> InlineOpToken Mult
                        | "-=" -> InlineOpToken Sub
                        | "/=" -> InlineOpToken Div
                        | "===" -> InlineOpToken Eq
                        | "!==" -> InlineOpToken NotEq
                        | "=>" -> ArrowToken
                        | "$" -> DollarToken
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

exception DebugException of string

let string_of_token token = 
    match token with
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
    | OpToken NotEq -> "<!=>"
    | OpToken Lt -> "< < >"
    | OpToken Gt -> "< > >"
    | OpToken LtEq -> "< <= >"
    | OpToken GtEq -> "< >= >"
    | InlineOpToken Add -> "<+=>"
    | InlineOpToken Sub -> "<-=>"
    | InlineOpToken Mult -> "<*=>"
    | InlineOpToken Div -> "</=>"
    | InlineOpToken Eq -> "<===>"
    | InlineOpToken NotEq -> "<!==>"
    | InlineOpToken _ -> raise @@ DebugException "unsupported"
    | LabelToken s -> ("<Label " ^ s ^ ">")
    | IfToken -> "<if>"
    | ElseToken -> "<else>"
    | FunctionToken -> "<function>"
    | ArrowToken -> "< => >"
    | DollarToken -> "< $ >"


exception CompilationError of string

type builtin =
    BuiltIn of string * int

type expression =
    | ValueEx of value
    | LabelEx of string
    | CallEx of int * expression list
    | CallBuiltInEx of builtin * expression list
    | OpEx of expression * operator * expression

type compile_var_type =
    | VariableType
    | FunctionType of int * int
    | BuiltInType of builtin

type compile_scope =
    {
        vars : (string, compile_var_type) Hashtbl.t;
    }

let rec range a b =
    if a > b then []
             else a :: range (a+1) b

let rec compile_expr tokens scopes = 
    let rec find_var name scopes =
        match scopes with
        | [] -> raise @@ CompilationError ("undefined variable `" ^ name ^ "`")
        | v :: r -> ( match Hashtbl.find_opt v.vars name with
                      | Some t -> t
                      | None -> find_var name r
                    )
    in
    let (lhs, tokens) = 
        match tokens with
        | (ValueToken v,_) :: r -> (ValueEx v, r)
        | (LabelToken n,_) :: r -> ( match find_var n scopes with
                                     | VariableType -> (LabelEx n, r)
                                     | FunctionType (arity,id) -> 
                                             let (es,r) = List.fold_left (fun (es,r) _ -> let (e,r) = compile_expr r scopes in
                                                                                  (es @ [e],r)) ([],r) (range 1 arity);
                                             in
                                             (CallEx (id,es),r)
                                     | BuiltInType b -> 
                                             let BuiltIn (_,arity) = b in
                                             let (es,r) = List.fold_left (fun (es,r) _ -> let (e,r) = compile_expr r scopes in
                                                                                  (es @ [e],r)) ([],r) (range 1 arity);
                                             in
                                             (CallBuiltInEx (b,es),r)

                                   )
        | (OpenParenToken, _) :: r -> (let (e,r) = compile_expr r scopes in 
                                        match (e,r) with
                                        | (e, (CloseParenToken,_) :: r) -> (e,r)
                                        | _ -> raise @@ CompilationError ("missing `)`")
                                 )
        | (t, l) :: _ -> raise @@ CompilationError ("invalid expression: " ^ string_of_location l ^ ": " ^ string_of_token t)
        | [] -> raise @@ CompilationError ("reached eof while compiling expression")
    in
    match tokens with
    | (OpToken o,_) :: r -> 
            let (rhs,tokens) = compile_expr r scopes in (OpEx (lhs, o, rhs),tokens)
    | _ -> (lhs, tokens)

type statement =
    | VarDeclare of string
    | SetVariable of string * expression
    | ScopeBlock of statement list
    | FuncDeclare of int * string list * statement list
    | IfStatement of expression * statement list
    | IfElseStatement of expression * statement list * statement list
    | LonelyExpression of expression

type func =
    Function of string list * statement list 

let (@<) a (b,c) =
    (a @ b, c)


let compile tokens builtins =
    let current_function_id = ref 0 in
    let push_new_scope scopes =
        {vars=Hashtbl.create 128;} :: scopes
    in
    let push_args_scope args scopes = 
        let scopes = push_new_scope scopes in
        let vars = (List.hd scopes).vars in
        List.iter (fun arg -> Hashtbl.add vars arg VariableType) args;
        Hashtbl.add vars "__return" VariableType;
        scopes
    in
    let rec compile tokens is_global single scopes =
       let compile_tail rst =
           if single   then ([],rst)
                       else compile rst is_global single scopes
       in
       let define_var name typ =
           let vars = (List.hd scopes).vars in
           match Hashtbl.find_opt vars name with
           | Some _ -> raise @@ CompilationError ("redeclaration of `" ^ name ^ "`")
           | None -> Hashtbl.add vars name typ
       in
       let rec check_var name scopes =
           match scopes with
           | [] -> raise @@ CompilationError ("undefined variable `" ^ name ^ "`")
           | v :: r -> ( match Hashtbl.find_opt v.vars name with
                         | Some _ -> ()
                         | None -> check_var name r
                       )
       in
       let rec compile_function_args tokens =
           match tokens with
           | [] -> raise @@ CompilationError "found eof before `=>`"
           | (LabelToken name, _) :: rst -> [name] @< compile_function_args rst
           | (ArrowToken,_) :: rst -> ([],rst)
           | (t,l) :: rst ->
               raise @@ CompilationError ("unexpected token in args: " ^ string_of_location l ^ " " ^ string_of_token t)
       in
       match tokens with
       | [] when is_global -> ([],[])
       | [] -> raise @@ CompilationError ("scope wasn't closed before eof")
       | (CloseCurlyToken,l) :: rst when not is_global ->
               ([],rst)
       | (VarToken,l) :: (LabelToken name,_) :: (SetToken,_) :: rst -> 
               define_var name VariableType;
               let (e,rst) = compile_expr rst scopes in
                   [VarDeclare name; SetVariable (name,e)] @< compile_tail rst;
       | (VarToken,l) :: (LabelToken name,_) :: rst -> 
               define_var name VariableType;
                   [VarDeclare name] @< compile_tail rst
       | (FunctionToken,l) :: (LabelToken name,_) :: rst -> 
               let (args,rst) = compile_function_args rst in
               current_function_id := !current_function_id + 1;
               let id = !current_function_id in
               define_var name @@ FunctionType (List.length args, id);
               let (sts,rst) = compile rst false true (scopes |> push_args_scope args |> push_new_scope) in
               [FuncDeclare (id,args,sts); ] @< compile_tail rst;
       | (LabelToken name,l) :: (SetToken,_)  :: rst -> 
               check_var name scopes;
               let (e,rst) = compile_expr rst scopes in
                   [SetVariable (name,e)] @< compile_tail rst
       | (LabelToken n1,l) :: (InlineOpToken o,_) :: rst -> 
               let (e,rst) = compile_expr rst scopes in
                   [SetVariable (n1,OpEx(LabelEx n1,o,e))] @< compile_tail rst
       | (OpenCurlyToken,l) :: rst ->
               let (sts,rst) = compile rst false false (push_new_scope scopes) in
                   [ScopeBlock sts] @< compile_tail rst
       | (IfToken,l) :: rst ->
               let (e,rst) = compile_expr rst scopes in
               let (sts,rst) = compile rst false true (push_new_scope scopes) in
               (   match rst with
                   | (ElseToken,_) :: rst -> ( let (ests,rst) = compile rst false true (push_new_scope scopes) in
                                               [IfElseStatement (e,sts,ests)] @< compile_tail rst
                                             )
                   | _ -> [IfStatement (e,sts)] @< compile_tail rst
               )
       | (DollarToken,l)  :: rst -> 
               let (e,rst) = compile_expr rst scopes in
                   [LonelyExpression e] @< compile_tail rst
       | (t,l) :: _ -> 
               raise @@ 
                   CompilationError ("unexpected token while compiling statements: " ^ string_of_location l ^ " " ^ string_of_token t)
    in
    let base_scope = { vars=Hashtbl.create 128 } in
    List.iter (fun b -> let BuiltIn (name,_) = b in Hashtbl.add base_scope.vars name @@ BuiltInType b) builtins;
    let (a,_) = compile tokens true false (push_new_scope [base_scope]) in a

exception EvaluationException of string

type evaluation_scope =
    { 
        vars : (string, value) Hashtbl.t list;
        funcs : (int, func) Hashtbl.t;
    }

let rec evaluate program scopes =
    let scopes = {vars=Hashtbl.create 128 :: scopes.vars; funcs=scopes.funcs}  in
    let get_var name scopes =
        let vars = scopes.vars in
        let rec get_var name vars =
            match vars with
                | v :: r -> ( match Hashtbl.find_opt v name with
                              | Some v -> v 
                              | None -> get_var name r
                            )
                | [] -> raise @@ EvaluationException ("unreachable! : undeclared variable `" ^ name ^ "`")
        in
        get_var name vars
    in

    let get_func id scopes =
        match Hashtbl.find_opt scopes.funcs id with
        | Some v -> v 
        | None -> raise @@ EvaluationException ("unreachable! : undeclared function #" ^ string_of_int id)
    in

    let declare_var name scopes = 
        let vars = (List.hd scopes.vars) in
            Hashtbl.add vars name Null 
        in

    let declare_func id args sts = 
        let funcs = scopes.funcs in
            Hashtbl.add funcs id @@ Function (args,sts) 
        in

    let rec set_var name va scopes =  
        let vars = scopes.vars in
        let rec set_var name va vars =  
            match vars with
                | v :: r -> ( match Hashtbl.find_opt v name with
                              | Some _ -> Hashtbl.replace v name va
                              | None -> set_var name va r
                            )
                | [] -> raise @@ EvaluationException ("unreachable! : undeclared variable `" ^ name ^ "`")
            in
            set_var name va vars
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

    let not_eq_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Bool (v1 != v2)
        | (String v1, String v2) ->  Bool (v1 != v2)
        | (Bool v1, Bool v2) ->  Bool (v1 != v2)
        | _ -> raise @@ EvaluationException "mismatch types in `eq`"
        in


    let print_value v = 
        match v with
        | Null -> print_string "NULL"
        | Int v -> print_int v
        | String  v -> print_string v
        | Bool  b -> print_string @@ string_of_bool b
    in

    let lt_values v1 v2 =
        match (v1,v2) with
        | (Int v1, Int v2) ->  Bool (v1 < v2)
        | _ -> raise @@ EvaluationException "mismatch types in `lt`"
    in

    let gt_values v1 v2 =
        match (v1,v2) with
        | (Int v1, Int v2) ->  Bool (v1 > v2)
        | _ -> raise @@ EvaluationException "mismatch types in `gt`"
    in

    let not_value v = 
        match v with
        | Bool true -> Bool false
        | Bool false -> Bool true
        | _ -> raise @@ EvaluationException "mismatch types in `not`"
    in

    let exec_builtin args b = 
        match b with
        | BuiltIn ("print",1) -> print_value (List.hd args); Null
        | BuiltIn ("println",1) -> print_value (List.hd args); print_newline(); Null
        | BuiltIn ("substr",3) -> ( match args with
                                    | [String str; Int b; Int l] -> String (String.sub str b l)
                                    | _ -> raise @@ EvaluationException ("mismatch types in `substr`")
                                  )
        | BuiltIn ("char_at",2) -> ( match args with
                                     | [String str; Int p] -> if String.length str == p then Int 0
                                                                                        else Int (int_of_char (String.get str p))
                                     | _ -> raise @@ EvaluationException ("mismatch types in `char_at`")
                                   )
        | BuiltIn (name,arity) -> raise @@ EvaluationException ("not implemented builtin " ^ name ^ "/" ^ string_of_int arity)
    in

    let rec eval_expr expr =
        match expr with
        | ValueEx v -> v
        | LabelEx n -> get_var n scopes
        | CallEx (id,args) -> let scopes = {vars=Hashtbl.create 128 :: scopes.vars; funcs=scopes.funcs}  in
                              let Function (names, sts) = get_func id scopes in
                                  declare_var "__return" scopes;
                                  List.iter2 (fun name e -> declare_var name scopes; set_var name (eval_expr e) scopes) names args;
                                  evaluate sts scopes;
                                  get_var "__return" scopes
        | CallBuiltInEx (b,args) -> let args = List.map eval_expr args in
                                    exec_builtin args b 
        | OpEx (l,o,r) ->
                match o with
                | Add -> add_values (eval_expr l) (eval_expr r)
                | Mult -> mult_values (eval_expr l) (eval_expr r)
                | Sub -> sub_values (eval_expr l) (eval_expr r)
                | Div -> div_values (eval_expr l) (eval_expr r)
                | Eq -> eq_values (eval_expr l) (eval_expr r)
                | NotEq -> not_eq_values (eval_expr l) (eval_expr r)
                | Lt -> lt_values (eval_expr l) (eval_expr r)
                | Gt -> gt_values (eval_expr l) (eval_expr r)
                | LtEq -> not_value @@ gt_values (eval_expr l) (eval_expr r)
                | GtEq -> not_value @@ lt_values (eval_expr l) (eval_expr r)
    in

    let eval stat = 
        match stat with
        | VarDeclare name -> declare_var name scopes
        | SetVariable (name, e) -> set_var name (eval_expr e) scopes
        (* | PrintValue e -> print_value (eval_expr e) *)
        (* | PrintLnValue e -> print_value (eval_expr e); print_newline () *)
        | ScopeBlock sts -> evaluate sts scopes
        | IfStatement (e,sts) -> (  match eval_expr e with
                                    | Bool true -> evaluate sts scopes
                                    | Bool false -> ()
                                    | _ -> raise @@ EvaluationException "expected bool value in `if`"
                                 )   
        | IfElseStatement (e,sts,ests) -> 
                                 (  match eval_expr e with
                                    | Bool true -> evaluate sts scopes
                                    | Bool false -> evaluate ests scopes
                                    | _ -> raise @@ EvaluationException "expected bool value in `if`"
                                 )
        | FuncDeclare (id,args,sts) -> declare_func id args sts
        | LonelyExpression e -> let _ = eval_expr e in ()
        in

    List.iter eval program 

(*====================*)   

let () =
    let ic = open_in ifile in
    let tokens = parse ic {file = ifile; line = 1; col = 1} in
    (* List.iter (fun (token,loc) -> print_endline @@ string_of_location loc ^ ": " ^ string_of_token token ) tokens; *)
    close_in ic;
    let builtins = [
                BuiltIn ("print",1);
                BuiltIn ("println",1);
                BuiltIn ("substr",3);
                BuiltIn ("char_at",2);
                ] in
    let program = compile tokens builtins in
    evaluate program {vars=[]; funcs=Hashtbl.create 128;}

