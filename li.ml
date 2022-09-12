
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
    | Pair of value * value

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
    | Pairify

type token =
    | OpenParenToken
    | CloseParenToken
    | OpenCurlyToken
    | CloseCurlyToken
    | VarToken
    | OpToken of operator
    | InlineOpToken of operator
    | ValueToken of value
    | LabelToken of string
    | IfToken
    | ElseToken
    | FunctionToken
    | AliasToken
    | ArrowToken
    | DollarToken
    | NullToken
    | ColonToken
    | EqualsToken


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
    | '+' | '-' | '/' | '*' | '=' | ':' | '>' | '<' | '$' | '!' | ',' -> true
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
                        | "alias" -> AliasToken
                        | "true" -> ValueToken (Bool true)
                        | "false" -> ValueToken (Bool false)
                        | "null" -> ValueToken (Null)
                        | word -> LabelToken word
                       ), chars, String.length b)
           in
           let rec parse_op b chars =
               match chars with
               | c :: r when is_op_char c ->
                       parse_op (b ^ string_of_char c) r
               | _ -> ((match b with
                        | ":" -> ColonToken
                        | "=" -> EqualsToken
                        | "+" -> OpToken Add
                        | "*" -> OpToken Mult
                        | "-" -> OpToken Sub
                        | "/" -> OpToken Div
                        | "<" -> OpToken Lt
                        | ">" -> OpToken Gt
                        | "<=" -> OpToken LtEq
                        | ">=" -> OpToken GtEq
                        | "==" -> OpToken Eq
                        | "," -> OpToken Pairify
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
               (* Comment *)
               | '/' :: '/' :: r -> [] 
               | c :: r when is_whitespace c -> parse_c r (move_location_by_cols loc 1)
               | c :: r when is_op_char c -> let (op,r,len) = parse_op "" chars in 
                                                 (op,loc) :: parse_c r (move_location_by_cols loc len)
               | c :: r when is_numeric c -> let (num,r,len) = parse_int "" chars in 
                                                 (num,loc) :: parse_c r (move_location_by_cols loc len)
               | '\'' :: c :: '\'' :: r -> (ValueToken (Int (int_of_char c)), loc) :: parse_c r (move_location_by_cols loc 3)
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
    | ValueToken (Pair (_,_)) -> raise @@ DebugException "unsupported"
    | ColonToken -> "<:>"
    | EqualsToken -> "<=>"
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
    | OpToken Pairify -> "< , >"
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
    | AliasToken -> "<alias>"
    | ArrowToken -> "< => >"
    | DollarToken -> "< $ >"
    | NullToken -> "< null >"


exception CompilationError of string

type builtin =
    BuiltIn of string * int

type value_type = 
    | IntType
    | StringType
    | BooleanType
    | NullType
    | PairType of value_type * value_type
    | Alias of string

type expression =
    | ValueEx of value
    | LabelEx of string
    | CallEx of int * expression list
    | CallBuiltInEx of builtin * expression list
    | OpEx of expression * operator * expression

type compile_var_type =
    | VariableType of value_type
    | FunctionType of value_type list * value_type * int
    | BuiltInType of builtin
    | AliasType of value_type

type compile_scope =
    {
        vars : (string, compile_var_type) Hashtbl.t;
    }

let rec range a b =
    if a > b then []
             else a :: range (a+1) b

let rec find_var name scopes =
    match scopes with
    | [] -> raise @@ CompilationError ("undefined variable `" ^ name ^ "`")
    | v :: r -> ( match Hashtbl.find_opt v.vars name with
                  | Some t -> t
                  | None -> find_var name r
                )
let rec dealias_type scopes t = 
        match t with
        | Alias name -> (   match find_var name scopes with
                            | AliasType t -> dealias_type scopes t   
                            | _ -> raise @@ CompilationError "provided custom type name must be a type alias"
                        )
        | _ -> t

let rec compare_types scopes a b =
    let a = dealias_type scopes a in
    let b = dealias_type scopes b in
    match (a,b) with
    | (NullType, _) -> true
    | (_, NullType) -> true
    | (a,b) when a == b -> true
    | (PairType (a,b),PairType (c,d)) -> compare_types scopes a c && compare_types scopes b d
    | _ -> false

let rec compile_expr tokens scopes = 
    let value_type_of_value v =
        match v with
        | Int _ -> IntType
        | String _ -> StringType
        | Bool _ -> BooleanType
        | Null -> NullType
        | _ -> raise @@ CompilationError "unsupported type"
    in
    let typecheck_op a o b =
        match (a,o,b) with
        | (StringType, Add, StringType) -> StringType
        | (IntType, Add, IntType) -> IntType
        | (IntType, Sub, IntType) -> IntType
        | (IntType, Mult, IntType) -> IntType
        | (IntType, Div, IntType) -> IntType
        | (a, Pairify, b) -> PairType (a,b)
        | (_, (Eq | NotEq | Lt | Gt | LtEq | GtEq),_) -> BooleanType
        | _ -> raise @@ CompilationError "unsupported types in binary operator"
    in
    let dealias_type = dealias_type scopes
    in
    let typecheck_builtin b ets =
        let ets = List.map dealias_type ets in
        match b with
        | BuiltIn ("println",1) 
        | BuiltIn ("print", 1) ->  (match ets with
                            | [IntType] | [StringType] | [BooleanType] | [NullType] | [PairType _] -> NullType
                            | _ -> raise @@ CompilationError "mismatch type in builtin `println`"
                           )
        | BuiltIn ("substr",3) -> (
           match ets with
           | [StringType; IntType; IntType] -> StringType
           | _ -> raise @@ CompilationError "type mismatch in `substr`"
        )
        | BuiltIn ("char_at",2) -> (
           match ets with
           | [StringType; IntType] -> IntType
           | _ -> raise @@ CompilationError "type mismatch in `char_at"
        )
        | BuiltIn ("string_of_char",1) -> (
           match ets with
           | [IntType] -> StringType
           | _ -> raise @@ CompilationError "type mismatch in `string_of_char`"
        )
        | BuiltIn ("read_line",0) -> StringType
        | BuiltIn ("fst",1) -> (
           match ets with
           | [PairType (t1,_)] -> t1
           | _ -> raise @@ CompilationError "type mismatch in `fst`"
        )
        | BuiltIn ("snd",1) -> (
           match ets with
           | [PairType (_,t2)] -> t2
           | _ -> raise @@ CompilationError "type mismatch in `snd`"
        )
        | BuiltIn (name,arity) -> raise @@ CompilationError ("unsupported builtin in typechecking: " ^ name  ^ "/" ^ string_of_int arity)
    in
    let ((lhs, lvt, loc), tokens) = 
        match tokens with
        | (ValueToken v,l) :: r -> ((ValueEx v,value_type_of_value v,l), r)
        | (LabelToken n,l) :: r -> ( match find_var n scopes with
                                     | VariableType vt -> ((LabelEx n,vt,l), r)
                                     | FunctionType (ats,rt,id) -> 
                                             let (es,r) = List.fold_left (fun (es,r) t -> 
                                                             let ((e,et,l),r) = compile_expr r scopes in
                                                             if compare_types scopes t et then (es @ [e],r)
                                                             else 
                                                                 raise @@ CompilationError 
                                                                 (string_of_location l ^ ": type mismatch in function argument")) 
                                             ([],r) ats;
                                             in
                                             ((CallEx (id,es),rt,l),r)
                                     | BuiltInType b -> 
                                             let BuiltIn (_,arity) = b in
                                             let ((es,ets),r) = List.fold_left (fun ((es,ets),r) _ -> let ((e,t,_),r) = compile_expr r scopes in
                                                                     ((es @ [e],ets @ [t]),r)) (([],[]),r) (range 1 arity);
                                             in
                                             let rt = typecheck_builtin b ets
                                             in
                                             ((CallBuiltInEx (b,es),rt,l),r)

                                     | AliasType _ -> raise @@ CompilationError (string_of_location l ^ ": cannot use type names in expressions")
                                   )
        | (OpenParenToken, l) :: r -> (let ((e,t,_),r) = compile_expr r scopes in 
                                        match ((e,t),r) with
                                        | ((e,t), (CloseParenToken,_) :: r) -> ((e,t,l),r)
                                        | _ -> raise @@ CompilationError ("missing `)`")
                                 )
        | (t, l) :: _ -> raise @@ CompilationError ("invalid expression: " ^ string_of_location l ^ ": " ^ string_of_token t)
        | [] -> raise @@ CompilationError ("reached eof while compiling expression")
    in
    match tokens with
    | (OpToken o,_) :: r -> 
            let ((rhs,rvt,_),tokens) = compile_expr r scopes in
            let t = typecheck_op lvt o rvt in
            ((OpEx (lhs, o, rhs),t, loc),tokens)
            (* else raise @@ CompilationError (string_of_location loc ^ ": type mismatch in binary expression") *)
    | _ -> ((lhs,lvt,loc), tokens)

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

exception ArgumentException

let rec take_all_but_last_2 lst = 
    match lst with
    | [_;_] | [_] | [] -> []
    | x :: xs -> x :: take_all_but_last_2 xs

let rec take_last_2 lst =
    match lst with
    | [] | [_] -> raise ArgumentException
    | [x;y] -> [x;y]
    | x :: xs -> take_last_2 xs

let compile tokens builtins =
    let current_function_id = ref 0 in
    let push_new_scope scopes =
        {vars=Hashtbl.create 128;} :: scopes
    in
    let push_args_scope args ats rt scopes = 
        let scopes = push_new_scope scopes in
        let vars = (List.hd scopes).vars in
        List.iter2 (fun arg at -> Hashtbl.add vars arg (VariableType at)) args ats;
        Hashtbl.add vars "__return" (VariableType rt);
        scopes
    in
    let create_function_scopes args ats rt scopes =
        let non_global = take_all_but_last_2 scopes in
        let global = take_last_2 scopes in
        (* Hashtbl.iter (fun n _ -> print_endline n; ) global.vars; *)
        let without_local_vars = 
            (List.map 
                (fun s -> 
                    let vs = (Hashtbl.copy s.vars) in
                    Hashtbl.filter_map_inplace 
                        (fun n v -> match v with
                                    | VariableType _ -> None
                                    | other -> Some other
                        ) vs;
                    {vars=vs}
                )
            non_global
            ) @ global
        in
        push_args_scope args ats rt without_local_vars
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
       (* let rec check_var name scopes = *)
       (*     match scopes with *)
       (*     | [] -> raise @@ CompilationError ("undefined variable `" ^ name ^ "`") *)
       (*     | v :: r -> ( match Hashtbl.find_opt v.vars name with *)
       (*                   | Some _ -> () *)
       (*                   | None -> check_var name r *)
       (*                 ) *)
       (* in *)
       let rec compile_type tokens = 
            match tokens with
            | (OpenParenToken, l) :: rst -> let (t,rst) = compile_type rst in (
                match rst with
                | (CloseParenToken, _) :: rst -> (t,rst)
                | _ -> raise @@ CompilationError (string_of_location l ^ ": missing `)`")
                )
            | (LabelToken "int",_) :: rst -> (IntType,rst)
            | (LabelToken "string",_) :: rst -> (StringType,rst)
            | (LabelToken "bool",_) :: rst -> (BooleanType,rst)
            | (ValueToken Null, _) :: rst -> (NullType,rst)
            | (LabelToken "pair",_) :: rst -> let (lt,rst) = compile_type rst in
                                              let (rt,rst) = compile_type rst in 
                                                    (PairType (lt,rt),rst)
            | (LabelToken name,_) :: rst -> (Alias name,rst)
            | (t,l) :: rst -> raise @@ CompilationError (string_of_location l ^ ": unexpected token in type: " ^ string_of_token t)
            | [] -> raise @@ CompilationError "found eof instead of type" 
       in
       let rec compile_function_args tokens =
           match tokens with
           | [] -> raise @@ CompilationError "found eof before `=>`"
           | (ArrowToken,_) :: rst -> let (t,rst) = compile_type rst  in
                                       (match t with
                                       | _ -> ([],[],t,rst)
                                       )
           | (LabelToken name, _) :: (ColonToken,_) :: rst -> 
                   let (t,rst) = compile_type rst in 
                   (match t with
                   | NullType -> raise @@ CompilationError "functions cannot have `null` type arguments"
                   | _ -> let (args,ats,rt,rst2) = compile_function_args rst in (name :: args, t :: ats, rt, rst2)
                   )
           | (t,l) :: rst ->
               raise @@ CompilationError ("unexpected token in args: " ^ string_of_location l ^ " " ^ string_of_token t)
       in
       match tokens with
       | [] when is_global -> ([],[])
       | [] -> raise @@ CompilationError ("scope wasn't closed before eof")
       | (CloseCurlyToken,l) :: rst when not is_global ->
               ([],rst)
       | (VarToken,l) :: (LabelToken name,_) :: (EqualsToken,_) :: rst -> 
               let ((e,t,_),rst) = compile_expr rst scopes in (
                   match t with
                   | NullType -> 
                           raise @@ CompilationError (string_of_location l ^ ": cannot create variables of `null` type")
                   | _ -> define_var name (VariableType t);
                           [VarDeclare name; SetVariable (name,e)] @< compile_tail rst;
               )

                 
       | (VarToken,l) :: (LabelToken name,_) :: (ColonToken, _) :: rst -> 
               let (t,rst) = compile_type rst in
               (
                   match rst with
                   | (EqualsToken, _) :: rst -> 
                           let ((e,et,_),rst) = compile_expr rst scopes in (
                               if compare_types scopes t et then 
                                   (define_var name (VariableType t);
                                   (match t with
                                   | NullType -> 
                                           raise @@ CompilationError (string_of_location l ^ ": cannot create variables of `null` type")
                                   | _ ->                            
                                           [VarDeclare name; SetVariable (name,e)] @< compile_tail rst;)
                                   )
                               else raise @@ CompilationError (string_of_location l ^ ": type mismatch in variable declaration")
                           )
                   | rst ->    
                           define_var name (VariableType t);
                           (match t with
                           | NullType -> 
                                   raise @@ CompilationError (string_of_location l ^ ": cannot create variables of `null` type")
                           | _ ->                            
                                   [VarDeclare name] @< compile_tail rst
                           )               
               )
       | (FunctionToken,l) :: (LabelToken name,_) :: rst -> 
               let (args,ats,rt,rst) = compile_function_args rst in
               current_function_id := !current_function_id + 1;
               let id = !current_function_id in
               define_var name @@ FunctionType (ats, rt, id);
               let (sts,rst) = compile rst false true (scopes |> create_function_scopes args ats rt |> push_new_scope) in
               [FuncDeclare (id,args,sts); ] @< compile_tail rst;
       | (LabelToken name,l) :: (EqualsToken,_)  :: rst -> (
               match find_var name scopes with
               | VariableType vt -> 
                   let ((e,t,_),rst) = compile_expr rst scopes in 
                       if compare_types scopes t vt then
                           [SetVariable (name,e)] @< compile_tail rst
                       else
                            raise @@ CompilationError (string_of_location l ^ ": mismatch types in variable assignement")
               | _ -> raise @@ CompilationError (string_of_location l ^ ": trying to assign value to function name")
       )
       | (LabelToken n1,l) :: (InlineOpToken o,_) :: rst -> 
               let ((e,t,_),rst) = compile_expr rst scopes in
                   [SetVariable (n1,OpEx(LabelEx n1,o,e))] @< compile_tail rst
       | (OpenCurlyToken,l) :: rst ->
               let (sts,rst) = compile rst false false (push_new_scope scopes) in
                   [ScopeBlock sts] @< compile_tail rst
       | (IfToken,l) :: rst ->
               let ((e,t,_),rst) = compile_expr rst scopes in
               let (sts,rst) = compile rst false true (push_new_scope scopes) in
               (   match rst with
                   | (ElseToken,_) :: rst -> ( let (ests,rst) = compile rst false true (push_new_scope scopes) in
                                               [IfElseStatement (e,sts,ests)] @< compile_tail rst
                                             )
                   | _ -> [IfStatement (e,sts)] @< compile_tail rst
               )
       | (DollarToken,l)  :: rst -> 
               let ((e,_,_),rst) = compile_expr rst scopes in
                   [LonelyExpression e] @< compile_tail rst
       | (AliasToken,l) :: (LabelToken name, _) :: (ColonToken,_) :: rst ->
               let (t,rst) = compile_type rst
               in  define_var name @@ AliasType t;
                   compile_tail rst
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
        | (Null, Null) ->  Bool true
        | _ -> raise @@ EvaluationException "mismatch types in `eq`"
        in

    let not_eq_values v1 v2 = 
        match (v1,v2) with
        | (Int v1, Int v2) ->  Bool (v1 != v2)
        | (String v1, String v2) ->  Bool (v1 != v2)
        | (Bool v1, Bool v2) ->  Bool (v1 != v2)
        | (Null, Null) ->  Bool false
        | _ -> Bool true
    in


    let rec print_value v = 
        match v with
        | Null -> print_string "NULL"
        | Int v -> print_int v
        | String  v -> print_string v
        | Bool  b -> print_string @@ string_of_bool b
        | Pair (v1,v2) -> print_string "("; print_value v1; print_string ", "; print_value v2; print_string ")"
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

    let pairify_values v1 v2 =
        Pair (v1,v2)
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
        | BuiltIn ("string_of_char",1) -> ( match args with
                                            | [Int c] -> String (String.make 1 (char_of_int c))
                                            | _ -> raise @@ EvaluationException ("mismatch types in `string_of_char")
                                          )
        | BuiltIn ("read_line",0) -> String (input_line stdin)
        | BuiltIn ("fst",1) -> ( match args with
                                 | [Pair (v1,_)] -> v1
                                 | _ -> raise @@ EvaluationException ("mismatch types in `fst`")
                                )
        | BuiltIn ("snd",1) -> ( match args with
                                 | [Pair (_,v2)] -> v2
                                 | _ -> raise @@ EvaluationException ("mismatch types in `snd`")
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
                | Pairify -> pairify_values (eval_expr l) (eval_expr r)
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

let run_file ifile =
    let ic = open_in ifile in
    let tokens = parse ic {file = ifile; line = 1; col = 1} in
    (* List.iter (fun (token,loc) -> print_endline @@ string_of_location loc ^ ": " ^ string_of_token token ) tokens; *)
    close_in ic;
    let builtins = [
                BuiltIn ("print",1);
                BuiltIn ("println",1);
                BuiltIn ("substr",3);
                BuiltIn ("char_at",2);
                BuiltIn ("string_of_char",1);
                BuiltIn ("read_line",0);
                BuiltIn ("fst",1);
                BuiltIn ("snd",1);
                ] in
    let program = compile tokens builtins in
    evaluate program {vars=[]; funcs=Hashtbl.create 128;}

let () =
    if Array.length Sys.argv < 2 
        then (print_endline ("Usage: " ^ Sys.argv.(0) ^ " <file>");
             exit 1)
        else run_file Sys.argv.(1)

