(* This code is adapted from John Harrison's automated reasoning book.  I need
   to put a better attribution here *)

let explode : string -> char list = fun s ->
  let len = String.length s in
  let rec loop i acc =
    if i >= len
    then List.rev acc
    else loop (i + 1) (s.[i] :: acc)
  in
  loop 0 []

module Expression = struct
  type t =
  | Var of string
  | Const of int
  | Add of t * t
  | Mul of t * t

  let simplify t =
    let simplify1 t =
      match t with
      | Add (Const n, Const m) -> Const (n + m)
      | Mul (Const n, Const m) -> Const (n * m)
      | Add (Const 0, x)
      | Add (x, Const 0) -> x
      | Mul (Const 0, _)
      | Mul (_, Const 0) -> Const 0
      | Mul (Const 1, x)
      | Mul (x, Const 1) -> x
      | _ -> t
    in
    let rec loop t =
      match t with
      | Var _
      | Const _ -> t
      | Add (x,y) -> simplify1 (Add (loop x, loop y))
      | Mul (x,y) -> simplify1 (Mul (loop x, loop y))
    in
    loop t
end

module Lexing = struct
  open Expression

  let matches s = let chars = explode s in fun c -> List.mem c chars

  let space = matches " \t\n\r"
  and punctuation = matches "()[]{},"
  and symbolic = matches "~‘!@#$%^&*-+=|\\:;<>.?/"
  and numeric = matches "0123456789"
  and alphanumeric = matches
    "abcdefghijklmnopqrstuvwxyz_’ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;


  let rec lexwhile prop inp =
    match inp with
      c::cs when prop c -> let tok,rest = lexwhile prop cs in String.make 1 c^tok,rest
    | _ -> "",inp;;

  let rec lex inp =
    match snd(lexwhile space inp) with
      [] -> []
    | c::cs ->
      let prop =
        if alphanumeric(c) then alphanumeric
        else if symbolic(c) then symbolic
        else fun c -> false
      in
      let toktl,rest = lexwhile prop cs in
      (String.make 1 c^toktl)::lex rest;;

  let forall f l =
    let rec loop = function
      | [] -> true
      | x :: l ->
        if not (f x)
        then false
        else loop l
    in
    loop l

  let rec parse_expression i =
  match parse_product i with
    e1,"+"::i1 -> let e2,i2 = parse_expression i1 in Add(e1,e2),i2
  | e1,i1 -> e1,i1
    and parse_product i =
  match parse_atom i with
    e1,"*"::i1 -> let e2,i2 = parse_product i1 in Mul(e1,e2),i2
  | e1,i1 -> e1,i1
    and parse_atom i =
  match i with
    [] -> failwith "Expected an expression at end of input"
  | "("::i1 -> (match parse_expression i1 with
                  e2,")"::i2 -> e2,i2
                | _ -> failwith "Expected closing bracket")
  | tok::i1 -> if forall numeric (explode tok)
               then Const(int_of_string tok),i1
    else Var(tok),i1;;

  let make_parser pfn s =
    let expr,rest = pfn (lex(explode s)) in
    if rest = [] then expr else failwith "Unparsed input";;

  let default_parser = make_parser parse_expression

  let rec string_of_exp pr e =
    match e with
      Var s -> s
    | Const n -> string_of_int n
    | Add(e1,e2) ->
      let s = (string_of_exp 3 e1)^" + "^(string_of_exp 2 e2) in
      if 2 < pr then "("^s^")" else s
    | Mul(e1,e2) ->
      let s = (string_of_exp 5 e1)^" * "^(string_of_exp 4 e2) in
      if 4 < pr then "("^s^")" else s;;
end

let simplify : string -> string = fun s ->
  Lexing.string_of_exp 0 (Expression.simplify (Lexing.default_parser s))


let () =
  Js.Unsafe.global##simplify_string_ <- Js.wrap_callback (fun js_string ->
    Js.string (simplify (Js.to_string js_string))
  )
