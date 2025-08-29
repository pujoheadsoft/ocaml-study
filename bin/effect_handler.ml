open Effect
open Effect.Deep
open Effect.Shallow

(*
  moduleは型、関数、値などをまとめる。
  sigはモジュール型シグネチャを定義するキーワード
  シグネチャはモジュールが提供すべきインタフェースを宣言する
*)
module type ASK = sig
  (* 抽象型t このモジュールが扱う具体的な型　モジュールの実装時に具体的な型に決定される *)
  type t

  (* エフェクトを実行する関数 *)
  val ask : unit -> t

  (*
    エフェクトを解釈する関数(ハンドラー)
    init:tはラベル付き引数 initという名前で引数を指定できる
    'a は型パラメータ(多相型)。関数が任意の型を扱えることを表す。
  *)
  val run : (unit -> 'a) -> init:t -> 'a
end

(*
  module が module name (S...) のようにパラメーターをとるときはファンクターの定義となる。
  つまり Ask という名前のファンクターを定義している。
  S : sig type t end
  は S が 「type t という型を持つモジュール」でなければならないことを示している。

  : ASK with type t = S.t
  はこのファンクターが返すモジュールの型制約。
  : ASK は、返されるモジュールがASKシグネチャに従うことを表す。
  with type t = S.t は、ASK シグネチャの抽象型 t をパラメーター S の型 t と同じにするという意味。

  = struct
  はファンクターの実装部分の始まり。

 *)
module Ask (S : sig type t end) : ASK with type t = S.t = struct
  (* 内部での型定義。宣言部分の定義（外部に向けての制約）とは別に必要 *)
  type t = S.t

  (*
    type _ Effect.t +=
    は拡張可能バリアント型への新しいコンストラクタの追加を表す
    
    Ask : t Effect.t
    は Ask という名前のエフェクトコンストラクタを定義
    t Effect.t はこのエフェクトを実行(perform)したときの戻り値の型が
    上で定義されている型 t の値を返すエフェクトという意味。
    (Haskellの m a とは逆に a m のように書く)
   *)
  type _ Effect.t += Ask : t Effect.t

  let ask () : t = perform Ask

  let run (f: unit -> 'a) ~(init: t) : 'a =
    (*
      エフェクトハンドラーにはDeepとShallowがある
      複数回エフェクトが実行されたとき、その分だけ解釈を行う必要があるが、
      Deepは自動で同じハンドラーを再インストールしてくれるが
      Shallowはしてくれないので自分で再インストールが必要。
      この関数はShallowのハンドラーで、continue_withを使っている。
      自前でやる必要性からループ処理を記述している。
     *)
    let rec loop : type a r. t -> (a, r) continuation -> a -> r =
      fun env k x ->
        continue_with k x
        {
          retc = (fun result -> result);
          exnc = (fun e -> raise e);
          effc = (fun (type b) (eff: b Effect.t) ->
            match eff with
            | Ask -> Some (fun (k: (b, r) continuation) ->
                loop env k env
              )
            | _ -> None
          )
        }
    in
    loop init (fiber f) ()
end

module IntReader = Ask (struct type t = int end)  
module StringReader = Ask (struct type t = string end) 

let example () =  
  let value = IntReader.ask () in  
  Printf.printf "Got value: %d\n" value;  
  let another = IntReader.ask () in  
  Printf.printf "Got same value: %d\n" another  
  
let program () = IntReader.run example ~init:43