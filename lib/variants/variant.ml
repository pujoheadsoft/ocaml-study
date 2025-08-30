(* バリアント型 *)

(*
  バリアントは複数のコンストラクタを持つ型で、代数的データ型の一種。
  Haskellの代数的データ型に似ているが、構文が異なる。
  F#の判別共用体とはほとんど同じ。命名規則は異なるが。

  バリアント型を type x = A | B | C としたとき
  AやBやCはコンストラクタと呼ばれ、バリアント型xの値を生成するために使われる。
*)

(* 引数を持たないコンストラクタのみの列挙型としてのバリアント *)
type sign = Positive | Negative;;

(* 値を持ち、再帰的な定義のバリアント *)
type node = Leaf of int | Branch of node * node;;

type 'a list = Nil | Cons of 'a * 'a list;;

(*
  GADTs（Generalized Algebraic Data Types）もサポートしている
  _ term のようにワイルドカードを使って定義する。
  'a term と書いてもいいが、OCamlのGADTのときはワイルドカードを使うことが多そうだ。
  （別に 'a がどこかで使われているわけではないのでなんでもいい）
  任意の型の term を表すことができる。
*)
type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term