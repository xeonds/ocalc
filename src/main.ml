type token =
  | Int of int | Plus | Minus | Times | Divide | LParen | RParen | EOF

let tokenize input =
  let len = String.length input in
  let rec aux pos =
    if pos >= len then [EOF]
    else match input.[pos] with
      | ' ' -> aux (pos + 1)
      | '+' -> Plus :: aux (pos + 1)
      | '-' -> Minus :: aux (pos + 1)
      | '*' -> Times :: aux (pos + 1)
      | '/' -> Divide :: aux (pos + 1)
      | '(' -> LParen :: aux (pos + 1)
      | ')' -> RParen :: aux (pos + 1)
      | '0'..'9' as c ->
        let rec extract_number acc pos =
          if pos < len && Char.code input.[pos] - Char.code '0' >= 0 && Char.code input.[pos] - Char.code '0' <= 9 then
            extract_number (acc * 10 + Char.code input.[pos] - Char.code '0') (pos + 1)
          else
            acc, pos
        in
        let num, new_pos = extract_number (Char.code c - Char.code '0') (pos + 1) in
        Int num :: aux new_pos
      | _ -> failwith "Unexpected character"
  in aux 0

type expr =
  | Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec parse_expr tokens =
  let rec parse_primary tokens =
    match tokens with
    | (Int n) :: rest -> Num n, rest
    | LParen :: rest ->
      let expr, rest' = parse_expr rest in
      (match rest' with
       | RParen :: rest'' -> expr, rest''
       | _ -> failwith "Expected closing parenthesis")
    | _ -> failwith "Expected number or parenthesis"
  and parse_term tokens =
    let lhs, tokens = parse_primary tokens in
    let rec aux lhs tokens =
      match tokens with
      | Times :: rest ->
        let rhs, rest' = parse_primary rest in
        aux (Mul (lhs, rhs)) rest'
      | Divide :: rest ->
        let rhs, rest' = parse_primary rest in
        aux (Div (lhs, rhs)) rest'
      | _ -> lhs, tokens
    in
    aux lhs tokens
  in
  let lhs, tokens = parse_term tokens in
  let rec aux lhs tokens =
    match tokens with
    | Plus :: rest ->
      let rhs, rest' = parse_term rest in
      aux (Add (lhs, rhs)) rest'
    | Minus :: rest ->
      let rhs, rest' = parse_term rest in
      aux (Sub (lhs, rhs)) rest'
    | _ -> lhs, tokens
  in
  aux lhs tokens

let rec eval = function
  | Num n -> n
  | Add (lhs, rhs) -> eval lhs + eval rhs
  | Sub (lhs, rhs) -> eval lhs - eval rhs
  | Mul (lhs, rhs) -> eval lhs * eval rhs
  | Div (lhs, rhs) -> eval lhs / eval rhs

let calculate input =
  let tokens = tokenize input in
  let expr, _ = parse_expr tokens in
  eval expr

(* Example usage *)
let rec repl () =
    try
        Printf.printf "> ";
        let line = read_line () in
        let result = calculate line in
        Printf.printf "%s = %d\n" line result;
        repl ()
    with
    | _ -> Printf.printf "Error encountered\n"; repl ()

let () = repl ()
