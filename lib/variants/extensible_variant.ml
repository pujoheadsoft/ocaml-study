(*
  Extensible variant types
  拡張可能なバリアント型
*)

(* このモジュールの中で拡張を行う *)
module Expr = struct
  (* .. で拡張可能なバリアント型を定義する *)
  type attr = ..

  (* += でバリアント型を拡張(既存のバリアント型にコンストラクタを追加)できる *)
  type attr += Str of string

  (* 後からまた拡張できる *)
  type attr +=
    | Int of int
    | Float of float
end

(* 拡張可能バリアント型のパターンマッチングをする関数 *)
let to_string = function
  | Expr.Str s -> s
  | Expr.Int i -> Int.to_string i
  | Expr.Float f -> string_of_float f
  | _ -> "?"