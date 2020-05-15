(* Type for the parsing error *)
exception ParseError of string

(* Type for arguments to the commands *)
type const = Int of int 
    | Bool of string
    | Error
    | String of string
    | Name of string 
    | Unit of string
    | Closure of string * string * instruction list * environment * bool

(* Type for parsed instructions *)
and instruction = PushS of const
    | PushI of const
    | PushB of const
    | PushN of const
    | Push of const
    | Pop
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Neg
    | Swap
    | Quit
    | Cat
    | And
    | Or
    | Not
    | LessThan
    | Equal
    | If 
    | Bind
    | Let
    | End
    | Fun of string * string
    | InOutFun of string * string
    | Return
    | FunEnd
    | Call

(* Type for a binding of name to data *)
and binding = string * const

(* Type for visibility scope *)
and environment = BaseEnv of binding list |
    ChildEnv of const list * binding list * environment

(* Reads file data and converts it to a list of characters *)
let read_file_to_list (filePath : string) : char list = 
    (* Resulting list of characters *)
    let result = ref [] in
    (* Open chanel to read from file *)
    let fileChannel = open_in filePath in
    (* Read all characters *)
    try
        while true; do
            result := input_char fileChannel :: !result
        done; !result
    with End_of_file ->
        close_in fileChannel;
    (* Reverse the list, as it's in the oposite order *)
    List.rev !result;;

(* Helper function that recursively prints the stack to the chanel *)
let rec print_stack_helper ( (fileChannel : out_channel),
        (stack : const list) ) : unit =
    match stack with
        | stackHead :: stackTail ->
            (* Print element at head *)
            let () = 
                match stackHead with
                    | Int(value) -> 
                        output_string fileChannel (string_of_int value)
                    | Bool(value) -> output_string fileChannel value
                    | Error -> output_string fileChannel ":error:"
                    | String(value) -> output_string fileChannel value
                    | Name(value) -> output_string fileChannel value
                    | Unit(_) -> output_string fileChannel ":unit:"
                    | Closure(_, _, _, _, _) -> output_string fileChannel ":unit:"
                (* | _ -> output_string fileChannel "Test" *)
            in
                (* Print newline after head, and recur *)
                output_string fileChannel "\n";
                print_stack_helper (fileChannel, stackTail)
        | _ -> ()
    ;;

(* Prints the stack content to the output file *)
let print_stack_to_file ( (filePath : string), (stack : const list) ) =
    (* Open chanel to write to file *)
    let fileChannel = open_out filePath in
    (* Print the stack *)
    print_stack_helper (fileChannel, stack);
    (* Close channel *)
    close_out fileChannel;;

(* Recursively consumes characters from the list and produces int token *)
let rec parse_int ( (chars : char list), (acc : string) ) : const list =
    match chars with
        | charsHead :: charsTail -> (
            (* Handle situation based on the next character *)
            match charsHead with
                (* Stop when reached whitespace *)
                | ' ' | '\n' | '\r' | '\t' -> 
                    Int(int_of_string acc) :: (parse_token charsTail)
                (* Continue parsing integer *)
                | '0' .. '9' -> 
                    parse_int (charsTail, (acc ^ String.make 1 charsHead))
                (* Error on non-digit character *)
                | _ -> 
                    parse_name (charsTail, (acc ^ String.make 1 charsHead))
            )
        | [] -> Int(int_of_string acc) :: []

(* Recursively consumes characters from the list and produces special token *)
and parse_special ( (chars : char list), (acc : string) ) : const list =
    match chars with
        | charsHead :: charsTail -> (
            (* Handle situation based on the next character *)
            match charsHead with
                (* Stop parsing and produce token *)
                | ':' -> (let specialStr = acc ^ String.make 1 charsHead in
                    match specialStr with
                        | ":true:" | ":false:" -> 
                            Bool(specialStr) :: parse_token charsTail
                        | ":error:" -> 
                            Error :: parse_token charsTail
                        | ":unit:" ->
                            Unit(specialStr) :: parse_token charsTail
                        | _ ->
                            raise (ParseError ("Unknown special " ^ specialStr))
                    )
                (* Continue parsing token *)
                | _ -> 
                    parse_special (charsTail, (acc ^ String.make 1 charsHead))
            )
        | [] -> raise (ParseError "Unexpected end of special token without :")

(* Recursively consumes characters from the list and produces string token *)
and parse_string ( (chars : char list), (acc : string) ) : const list =
    match chars with
        | charsHead :: charsTail -> (
            (* Handle situation based on the next character *)
            match charsHead with
                (* Stop parsing and produce token *)
                | '"' ->
                    String(acc) :: parse_token charsTail
                (* Continue parsing token *)
                | _ -> 
                    parse_string (charsTail, (acc ^ String.make 1 charsHead))
            )
        | [] -> raise (ParseError "Unexpected end of string token without \"")

(* Recursively consumes characters from the list and produces name token *)
and parse_name ( (chars : char list), (acc : string) ) : const list =
    match chars with
        | charsHead :: charsTail -> (
            (* Handle situation based on the next character *)
            match charsHead with
                (* Continue parsing name *)
                | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> 
                    parse_name (charsTail, (acc ^ String.make 1 charsHead))
                (* Stop when reached whitespace *)
                | ' ' | '\n' | '\r' | '\t' -> 
                    Name(acc) :: parse_token charsTail
                (* Error on non-digit character *)
                | _ -> raise (ParseError "Unexpected character in name")
            )
        | [] -> Name(acc) :: []

(* Converts a list of characters to the list of tokens *)
and parse_token ( chars : char list ) : const list = 
    match chars with
        (* Parse apropriate token based on first character *)
        | charsHead :: charsTail -> (
            match charsHead with
                (* Whitespaces are simply ignored *)
                | ' ' | '\n' | '\r' | '\t' -> parse_token charsTail
                (* Parse integer *)
                | '0' .. '9' -> parse_int (chars, "")
                (* Parse integer with heading - *)
                | '-' -> parse_int (charsTail, "-")
                (* Parse special token bool, error or unit *)
                | ':' -> parse_special (charsTail, ":")
                (* Parse string *)
                | '"' -> parse_string (charsTail, "")
                (* Parse name *)
                | '_' | 'A' .. 'Z' | 'a' .. 'z' -> parse_name (chars, "")
                (* Any other characters are error *)
                | _ -> raise (ParseError "Unexpected starting character")
            )
        (* Stop parsing on the end of characters *)
        | [] -> []

(* Converts a list of tokens to the list of instructions *)
let rec parse_com ( tokens : const list ) : instruction list = 
    match tokens with
        (****** Part 1 commands ******)
        (* Parse integer push *)
        | Name("pushi") :: arg :: tokensTail ->
            PushI(arg) :: parse_com tokensTail
        (* Parse string push *)
        | Name("pushs") :: arg :: tokensTail ->
            PushS(arg) :: parse_com tokensTail
        (* Parse name push *)
        | Name("pushn") :: arg :: tokensTail ->
            PushN(arg) :: parse_com tokensTail
        (* Parse boolean push *)
        | Name("pushb") :: arg :: tokensTail ->
            PushB(arg) :: parse_com tokensTail
        (* Parse error or unit push *)
        | Name("push") :: arg :: tokensTail ->
            Push(arg) :: parse_com tokensTail
        (* Parse pop *)
        | Name("pop") :: tokensTail ->
            Pop :: parse_com tokensTail
        (* Parse add *)
        | Name("add") :: tokensTail ->
            Add :: parse_com tokensTail
        (* Parse sub *)
        | Name("sub") :: tokensTail ->
            Sub :: parse_com tokensTail
        (* Parse mul *)
        | Name("mul") :: tokensTail ->
            Mul :: parse_com tokensTail
        (* Parse div *)
        | Name("div") :: tokensTail ->
            Div :: parse_com tokensTail
        (* Parse rem *)
        | Name("rem") :: tokensTail ->
            Rem :: parse_com tokensTail
        (* Parse neg *)
        | Name("neg") :: tokensTail ->
            Neg :: parse_com tokensTail
        (* Parse swap *)
        | Name("swap") :: tokensTail ->
            Swap :: parse_com tokensTail
        (* Parse quit *)
        | Name("quit") :: tokensTail ->
            Quit :: parse_com tokensTail
        (****** Part 2 commands ******)
        | Name("cat") :: tokensTail ->
            Cat :: parse_com tokensTail
        | Name("and") :: tokensTail ->
            And :: parse_com tokensTail
        | Name("or") :: tokensTail ->
            Or :: parse_com tokensTail
        | Name("not") :: tokensTail ->
            Not :: parse_com tokensTail
        | Name("equal") :: tokensTail ->
            Equal :: parse_com tokensTail
        | Name("lessThan") :: tokensTail ->
            LessThan :: parse_com tokensTail
        | Name("bind") :: tokensTail ->
            Bind :: parse_com tokensTail
        | Name("if") :: tokensTail ->
            If :: parse_com tokensTail
        | Name("let") :: tokensTail ->
            Let :: parse_com tokensTail
        | Name("end") :: tokensTail ->
            End :: parse_com tokensTail
        (****** Part 3 commands ******)
        | Name("fun") :: Name(name1) :: Name(name2) :: tokensTail ->
            Fun(name1, name2) :: parse_com tokensTail
        | Name("inOutFun") :: Name(name1) :: Name(name2) :: tokensTail ->
            InOutFun(name1, name2) :: parse_com tokensTail
        | Name("funEnd") :: tokensTail ->
            FunEnd :: parse_com tokensTail
        | Name("return") :: tokensTail ->
            Return :: parse_com tokensTail
        | Name("call") :: tokensTail ->
            Call :: parse_com tokensTail
        (****** General commands ******)
        (* End of instructions *)
        | [] -> []
        (* Fallback *)
        | _ -> raise (ParseError "Unknown instruction parse reached")

(* Resolves name to value in binding *)
let rec resolve_name_in_list ( (name : string), (bindings : binding list)) : const =
    match bindings with
        (* Value found *)
        | (bName, bValue) :: bindingsTail when bName = name -> bValue
        (* Value not found, but it's not the end of list *)
        | _ :: bindingsTail -> resolve_name_in_list (name, bindingsTail)
        (* Value not found, but it's the end *)
        | _ -> Error
    ;;

(* Resolves name to value in environment *)
let rec resolve_name ( (name : string), (env : environment) ) : const = 
    match env with
        (* If in base environment, this is final answer *)
        | BaseEnv(bindings) -> (
            let currResult = resolve_name_in_list (name, bindings) in
                match currResult with
                    | Name(unitName) -> resolve_name (unitName, env)
                    | _ -> currResult
        )
        (* Not base environment, check this first, then base *)
        | ChildEnv(_, bindings, baseEnv) -> (
            let currResult = resolve_name_in_list (name, bindings) in
                match currResult with
                    | Name(unitName) -> resolve_name (unitName, env)
                    | Error -> resolve_name (name, baseEnv)
                    | _ -> currResult
        )
    ;;

(* Provides stack with top N entities resolved to their final values *)
let rec resolve_stack ( (n : int), (stack : const list), 
        (env : environment) ) : const list =
    if n <= 0 then 
        stack
    else
        match stack with
            (* If requested more elements that can provide, simply return *)
            | [] -> []
            (* If requested is name, resolve it to value *)
            | Name(unitName) :: stackTail -> 
                resolve_name (unitName, env) :: 
                    resolve_stack (n - 1, stackTail, env)
            (* Else, simply copy value *)
            | stackHead :: stackTail ->
                stackHead :: resolve_stack (n - 1, stackTail, env)
    ;;

(* Provides stack with top M enties not touched, and then N entities resolved *)
let rec resolve_stack_skip ( (m : int), (n : int), (stack : const list), 
        (env : environment) ) : const list =
    if m <= 0 then 
        resolve_stack (n, stack, env)
    else
        match stack with
            (* If requested more elements that can provide, simply return *)
            | [] -> []
            (* Else, simply copy value and decrease M *)
            | stackHead :: stackTail ->
                stackHead :: resolve_stack_skip (m - 1, n, stackTail, env)
    ;;

(* Adds new binding to the bindings list. Replaces if already exists *)
let rec add_binding ( (name : string ), (value : const), 
        (bindings : binding list) ) : binding list =
    match bindings with
        (* Same name, should be overwritten *)
        | (bName, bValue) :: bindingsTail when bName = name -> 
            (bName, value) :: bindingsTail
        (* Different name, check others *)
        | bindingsHead :: bindingsTail ->
            bindingsHead :: add_binding (name, value, bindingsTail)
        (* This name not found, add to list *)
        | [] -> (name, value) :: []
    ;;

(* Adds new binding to the bindings list of environment, replaces if exists *)
let add_binding_to_env ( (name : string), (value : const), (env : environment) )
        : environment =
    match env with
        | BaseEnv(bindings) -> 
            BaseEnv(add_binding (name, value, bindings))
        | ChildEnv(oldStack, bindings, parentEnv) -> 
            ChildEnv(oldStack, add_binding (name, value, bindings), parentEnv)
    ;;

(* Recursively consumes instructions until reaches funEnd *)
(* Returns 1) list of function instructions, 2) list of other instructions *)
let rec consume_func( (instructions : instruction list), 
        (acc : instruction list), (innerDepth : int)) : instruction list * instruction list =
    match instructions with
        (* End instruction *)
        | FunEnd :: instrTail when innerDepth = 0 ->
            ((List.rev (FunEnd :: acc)), instrTail)
        | FunEnd :: instrTail when innerDepth != 0 ->
            consume_func (instrTail, FunEnd :: acc, innerDepth - 1)
        (* Function start instruction *)
        | Fun(funName, funArg) :: instrTail ->
            consume_func (instrTail, Fun(funName, funArg) :: acc, innerDepth + 1)
        | InOutFun(funName, funArg) :: instrTail ->
            consume_func (instrTail, InOutFun(funName, funArg) :: acc, innerDepth + 1)
        (* Other instructions *)
        | instrHead :: instrTail ->
            consume_func (instrTail, instrHead :: acc, innerDepth)
        | [] -> (acc, [])
    ;;

(* Executes a list of commands, returns stack at the end  *)
let rec run_instructions ( (instructions : instruction list), 
        (stack : const list), (env : environment) ) : const list * environment = 
    match instructions with
        (****** Part 1 commands ******)
        (* Execute integer push *)
        | PushI(Int(intVal)) :: instrTail ->
            run_instructions (instrTail, Int(intVal) :: stack, env)
        | PushI(_) :: instrTail ->
            run_instructions (instrTail, Error :: stack, env)
        (* Execute string push *)
        | PushS(String(strVal)) :: instrTail ->
            run_instructions (instrTail, String(strVal) :: stack, env)
        | PushS(_) :: instrTail ->
            run_instructions (instrTail, Error :: stack, env)
        (* Execute name push *)
        | PushN(Name(nameVal)) :: instrTail ->
            run_instructions (instrTail, Name(nameVal) :: stack, env)
        | PushN(_) :: instrTail ->
            run_instructions (instrTail, Error :: stack, env)
        (* Execute boolean push *)
        | PushB(Bool(boolVal)) :: instrTail ->
            run_instructions (instrTail, Bool(boolVal) :: stack, env)
        | PushB(_) :: instrTail ->
            run_instructions (instrTail, Error :: stack, env)
        (* Execute unit and error push *)
        | Push(Error) :: instrTail ->
            run_instructions (instrTail, Error :: stack, env)
        | Push(Unit(unitVal)) :: instrTail ->
            run_instructions (instrTail, Unit(unitVal) :: stack, env)
        | Push(_) :: instrTail ->
            run_instructions (instrTail, Error :: stack, env)
        (* Execute pop *)
        | Pop :: instrTail -> (
            match stack with
                | stackHead :: stackTail ->
                    run_instructions (instrTail, stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute addition *)
        | Add :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Int(i1) :: Int(i2) :: stackTail ->
                    run_instructions (instrTail, Int(i2 + i1) :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute subtraction *)
        | Sub :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Int(i1) :: Int(i2) :: stackTail ->
                    run_instructions (instrTail, Int(i2 - i1) :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute multiplication *)
        | Mul :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Int(i1) :: Int(i2) :: stackTail ->
                    run_instructions (instrTail, Int(i2 * i1) :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute division *)
        | Div :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Int(i1) :: Int(i2) :: stackTail when i1 != 0 ->
                    run_instructions (instrTail, Int(i2 / i1) :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute division for remainder *)
        | Rem :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Int(i1) :: Int(i2) :: stackTail when i1 != 0 ->
                    run_instructions (instrTail, Int(i2 mod i1) :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute negation *)
        | Neg :: instrTail -> (
            match resolve_stack (1, stack, env) with
                | Int(intVal) :: stackTail ->
                    run_instructions (instrTail, Int(-intVal) :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute swaping *)
        | Swap :: instrTail -> (
            match stack with
                | el1 :: el2 :: stackTail ->
                    run_instructions (instrTail, el2 :: el1 :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (****** Part 2 commands ******)
        (* Execute concatenation *)
        | Cat :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | String(s1) :: String(s2) :: stackTail ->
                    run_instructions (instrTail, String(s2 ^ s1) :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute boolean AND *)
        | And :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Bool(b1) :: Bool(b2) :: stackTail ->
                    if (b1 = ":true:") && (b2 = ":true:") then
                        run_instructions (instrTail, Bool(":true:") :: stackTail, env)
                    else
                        run_instructions (instrTail, Bool(":false:") :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute boolean OR *)
        | Or :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Bool(b1) :: Bool(b2) :: stackTail ->
                    if (b1 = ":true:") || (b2 = ":true:") then
                        run_instructions (instrTail, Bool(":true:") :: stackTail, env)
                    else
                        run_instructions (instrTail, Bool(":false:") :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute boolean NOT *)
        | Not :: instrTail -> (
            match resolve_stack (1, stack, env) with
                | Bool(boolValue) :: stackTail ->
                    if boolValue = ":true:" then
                        run_instructions (instrTail, Bool(":false:") :: stackTail, env)
                    else
                        run_instructions (instrTail, Bool(":true:") :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute equality comparison *)
        | Equal :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Int(i1) :: Int(i2) :: stackTail ->
                    if (i1 = i2) then
                        run_instructions (instrTail, Bool(":true:") :: stackTail, env)
                    else
                        run_instructions (instrTail, Bool(":false:") :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute "less than" check *)
        | LessThan :: instrTail -> (
            match resolve_stack (2, stack, env) with
                | Int(i1) :: Int(i2) :: stackTail ->
                    if i2 < i1 then
                        run_instructions (instrTail, Bool(":true:") :: stackTail, env)
                    else
                        run_instructions (instrTail, Bool(":false:") :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute binding *)
        | Bind :: instrTail -> (
            match resolve_stack (1, stack, env) with
                | Error :: stackTail ->
                    run_instructions (instrTail, Error :: stack, env)
                | value :: Name(bName) :: stackTail  -> (
                    run_instructions (instrTail, Unit(bName) :: stackTail, 
                            add_binding_to_env (bName, value, env))
                )
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Execute "if" check *)
        | If :: instrTail -> (
            match resolve_stack_skip (2, 1, stack, env) with
                | elTrue :: elFalse :: Bool(elCondition) :: stackTail ->
                    if elCondition = ":true:" then
                        run_instructions (instrTail, elTrue :: stackTail, env)
                    else
                        run_instructions (instrTail, elFalse :: stackTail, env)
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Start new visibility scope *)
        | Let :: instrTail ->
            run_instructions (instrTail, [], ChildEnv(stack, [], env))
        (* Terminate visibility scope *)
        | End :: instrTail -> (
            match env with
                | ChildEnv(oldStack, oldBindings, parentEnv) -> (
                    match stack with 
                        | stackHead :: stackTail ->
                            run_instructions (instrTail, stackHead :: oldStack,
                                parentEnv)
                        | [] -> 
                           run_instructions (instrTail, Error :: oldStack,
                                parentEnv)
                )
                | _ ->
                    raise (ParseError "End in the base scope")
        )
        (****** Part 3 commands ******)
        (* Simple function declaration *)
        | Fun(funName, funArg) :: instrTail -> (
            let funInstr, otherInstr = consume_func (instrTail, [], 0) in
            let funClosure = Closure(funName, funArg, funInstr, env, false) in
                run_instructions (otherInstr, funClosure :: stack, 
                        add_binding_to_env (funName, funClosure, env))
        )
        (* In/out function declaration *)
        | InOutFun(funName, funArg) :: instrTail -> (
            let funInstr, otherInstr = consume_func (instrTail, [], 0) in
            let funClosure = Closure(funName, funArg, funInstr, env, true) in
                run_instructions (otherInstr, funClosure :: stack, 
                        add_binding_to_env (funName, funClosure, env))
        )
        (* Function call *)
        | Call :: instrTail -> (
            let tmpStack = resolve_stack (2, stack, env) in
            match tmpStack with
                | Error :: stackTail ->
                    run_instructions (instrTail, Error :: stack, env)
                | value :: Closure(funName, funArg, funInstr, funEnv, false) :: stackTail  -> (
                    let funStack, funEnv2 = run_instructions (funInstr, [], 
                            add_binding_to_env (funName, Closure(funName, funArg, funInstr, funEnv, false),
                                ChildEnv(stackTail, (funArg, value) :: [], funEnv)
                            )
                        )
                    in
                        match funStack with
                            | [] ->
                                run_instructions (instrTail, stackTail, env)
                            | funStackHead :: funStackTail -> 
                                run_instructions 
                                        (instrTail, funStackHead :: stackTail, env)
                )
                | value :: Closure(funName, funArg, funInstr, funEnv, true) :: stackTail  -> (
                    let funStack, funEnv2 = run_instructions (funInstr, [], 
                            add_binding_to_env (funName, Closure(funName, funArg, funInstr, funEnv, true),
                                ChildEnv(stackTail, (funArg, value) :: [], funEnv)
                            )
                        )
                    in
                    let newEnv = match stack with 
                            | Name(stackHeadName) :: stackTail2 ->
                                add_binding_to_env (stackHeadName,
                                        resolve_name (funArg, funEnv2), env)
                            | _ -> env
                    in
                        match funStack with
                            | [] ->
                                run_instructions (instrTail, stackTail, newEnv)
                            | funStackHead :: funStackTail -> 
                                run_instructions 
                                        (instrTail, funStackHead :: stackTail, newEnv)
                )
                | _ ->
                    run_instructions (instrTail, Error :: stack, env)
        )
        (* Return from function without value *)
        | FunEnd :: instrTail -> (
            ([], env)
        )
        (* Return from function with top value *)
        | Return :: instrTail -> (
            match resolve_stack (1, stack, env) with
                | stackHead :: stackTail -> (stackHead :: [], env)
                | _ -> (Error :: [], env)
        )
        (****** General commands ******)
        (* Execute quit instruction *)
        | Quit :: tokensTail ->
            (stack, env)
        (* Fallback *)
        | _ -> raise (ParseError "Unknown instruction execution reached")

(* Reads input program from "input" file, and puts output in "output" file *)
let interpreter ( (input : string), (output : string )) : unit =
    (* Read characters *)
    let chars = read_file_to_list input in
    (* Convert characters to tokens *)
    let tokens = parse_token chars in
    (* Parse instructions from tokens *)
    let instructions = parse_com tokens in
    (* Execute the parsed instructions *)
    let stack, env = run_instructions (instructions, [], BaseEnv([])) in
    (* Print stack out *)
    print_stack_to_file (output, stack)
    ;;

(*interpreter ("input.txt", "output.txt");*)
(*interpreter (Sys.argv.(1), Sys.argv.(2));*)
