(*
  多相バリアント

  多相バリアントには三つのタイプがある
  1. 完全型
  2. 開放型
  3. 閉鎖型

  多相バリアントの一つ一つの値はタグと呼ばれる
*)

(* 多相バリアント型の定義はこのように`をつけて行うだけ *)
type ab = [`A | `B]
type a = [`A]
type b = [`B]
type bcd = [`B | `C | `D]
type abc = [`A | `B | `C]

(*
  1. 完全型
  完全型は、厳密にそのタグのみしか受け入れることができない

  以下の引数の型 [`A] が完全型の指定
  厳密に`Aのみが存在するバリアント型の値しか適用できない
*)
let a_only (_: [`A]) = 1


(* これは厳密に `A のみなのでOK *)
let ok_example_a_only (v: a) = a_only v

(* 以下は、`Aのみを期待しているところに`Aか`Bの型の値を適用しようとしているのでエラーになる *)
(* let ng_example_a_only (v: ab) = a_only v  *)

(* ---------------- *)

(*
  2. 開放型
  開放型は指定されたタグに加えて、他のタグも受け入れることができる。

  以下の引数の型 [> `A] が開放型の指定
  `Aさえ定義されていれば、他のコンストラクタがあって適用できる
 *)
let a_open (_ : [> `A]) = 1

(* ab には `A が含まれるのでOK *)
let op_example_a_open (v: ab) = a_open v

(* 以下は`Aが含まれていないのでNG *)
(* let ng_example_a_only (v: bcd) = a_open v *)

(* ---------------------- *)
(*
  3. 閉鎖型
  閉鎖型は、列挙されたタグのサブセットのみが許可される

  以下の [< `A | `B] が閉鎖型の指定
*)
let ab_close (x : [< `A | `B]) = match x with
  | `A -> 1
  | `B -> 2
  (* | `C -> 3 これは警告がでる *)

(* 以下はサブセットなのでOK *)
let ok_example_ab_close1 (v: ab) = ab_close v
let ok_example_ab_close2 (v: a) = ab_close v
let ok_example_ab_close3 (v: b) = ab_close v

(* 次は、サブセットではないのでNG *)
(* let ng_example_ab_close (v: abc) = ab_close v *)

(* --------------------- *)
(* 
  その他の例
 *)
type 'a rectangle = [`Square of 'a | `Rectangle of 'a * 'a]  
type 'a shape = [`Circle of 'a | 'a rectangle]  

(* 多相バリアント型は #でパターンマッチできる *)
let try_rectangle = function  
  | #rectangle as r -> Some r  
  | `Circle _ -> None
