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
      -------
      [シグネチャの意味]
      type a r. は全称量化された型。この場合のように関数定義内で局所的に導入される抽象型変数を`locally abstract types`という。
      t は環境の型(askが返す型で、type tで定義されているやつ)
      (a, r) continuation は型 a の値を受け取って型 r の結果を返す継続（continuation）の型
      a は継続の入力の型
      r は継続の出力の型
     *)
    let rec loop : type a r. t -> (a, r) continuation -> a -> r =
      (*
        funは無名関数
        こういったfunを使わないloopに直接的な関数定義をすると型システムがlocally abstract typesのスコープを正しく解決できないらしい
      *)
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
    (*
      loopの呼び出し
      fiberは a -> b を (a, b) continuation に変換する関数
    *)
    loop init (fiber f) ()

end

module IntAsk = Ask (struct type t = int end)  
module StringAsk = Ask (struct type t = string end) 

let exampleIntAsk () =
  let value = IntAsk.ask () in
  Printf.printf "Got value: %d\n" value;
  let another = IntAsk.ask () in
  Printf.printf "Got same value: %d\n" another

let exampleStringAsk () =
  let value = StringAsk.ask () in
  Printf.printf "Got value: %s\n" value;
  let another = StringAsk.ask () in
  Printf.printf "Got same value: %s\n" another

let runExampleIntAsk () = IntAsk.run exampleIntAsk ~init:43

let runExampleStringAsk () = StringAsk.run exampleStringAsk ~init:"Hello"