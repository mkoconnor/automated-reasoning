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
