#!/bin/sh
# lib/ から、本文の文字列化専用ライブラリ(to_tex.cmxa)をビルドし、
# サンプルコード/ 以下の各節ディレクトリへ配置する。ビルド生成物は
# リポジトリにコミットせず、このスクリプトを実行するたびに手元のOCamlバージョンで
# その場生成する。
#
# to_tex.cmxaの設計について:
#   to_tex.cmxaには、本書の代数モジュール（Integer・Rational・Q_rootpなど）
#   の実装そのものは一切含まれていない。含まれているのは、それらを
#   TeXの文字列に変換するためのexpr_*.mlだけである。
#
#   expr_*.mlは（下で説明するダイジェスト問題を避けるため）本物の代数
#   モジュールを一切参照しない設計になっているので、build.sh自体もlib/の
#   integer.ml・rational.mlのような実装ファイルを一切コンパイルしない。
#   to_tex.cmxaのビルドは、常にexpr_*.mlだけで完結する。
#
#   さらにexpr.ml自体を代数モジュールと同じ粒度（expr_integer.ml,
#   expr_rational.ml, ...）に分割し、-no-alias-depsでコンパイルしてある。
#   これにより、たとえばExpr.Integerしか使わないプログラムをリンクするとき、
#   to_tex.cmxaの中にExpr.Quadratic用のコード（実体のQ_rootpモジュールを
#   要求する）が同居していても、それは実際には引っ張り出されない。
#
#   結果として、各節ディレクトリでは
#     ocamlopt <その節までに書いたmlファイル...> to_tex.cmxa <test.ml>
#   のように、依存するmlファイルを先に、to_tex.cmxaを後に並べてコンパイル
#   すれば、その節がまだ習っていないはずの代数モジュール（Q_rootpやElementaryなど）
#   を求められることはない（ネイティブリンカーは左から右に依存を解決するため、
#   この順序は必須）。
#
#   もう1つ、これとは別の問題として、OCamlのネイティブリンカーはリンクする
#   全モジュールの.cmiダイジェスト（インターフェース全体のハッシュ値）が
#   完全一致することを要求する。to_tex.cmxaはlib/の「完成形」から1回だけ
#   ビルドされるため、たとえばIntegerに本文のあとの章でsucc・root・
#   differentiateのような関数が追加されると、読者が本文どおり（それより前の
#   章の内容だけ）でコンパイルしたintegerc.mlはto_tex.cmxaの中のexpr_integer.cmx
#   が要求するダイジェストと一致しなくなり、"inconsistent assumptions over
#   interface" というリンクエラーになる。
#   これを避けるため、expr_*.mlは実体の代数モジュールと極力縁を切ってある:
#     - Integer・Rational・Quaternion・Octonion・Hexadecanion:
#       実体は参照せず、int・(int*int)のような構造的に等価な型を直接書く。
#       この先いくつ関数が増えても、この書き方には影響しない
#     - Complex・Q_poly・Root2_field:
#       対応する代数モジュールと同じ形の値を関数合成だけで作れるので、
#       もともと実体を参照していない
#     - Q_rootp・Quadratic・Quadratic_poly・Omega_field・Cubic・Elementary:
#       内部がMap.Make/Set.Makeの出力という抽象型なので、上のように
#       構造的に書き下すことができない。かわりに、必要な操作だけを要求する
#       ファンクタ（Expr.Q_rootp.Make・Expr.Quadratic.Make・Expr.Elementary.Make
#       など）にしてあり、読者が自分の代数モジュールをその場で渡して
#       インスタンス化する（例: module Quadratic_expr = Expr.Quadratic.Make (Q_rootp)）。
#       Elementaryだけは、指数の有理関数部分の判定（Quadratic.equal/zero）と
#       分母が1かどうかの判定（Quadratic_poly.equal/one）にQ_rootpの表示情報
#       だけでは足りないため、Q_rootpに加えてQuadratic・Quadratic_polyの実体も
#       あわせて渡す（例: module Elementary_expr =
#       Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)）
#
# 使い方:
#   ./build.sh        to_tex.cmxaをビルドして各節ディレクトリへ配置する
#   ./build.sh clean  生成物をすべて削除する
set -e

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
CODE_DIR="$ROOT_DIR/サンプルコード"
LIB_DIR="$ROOT_DIR/lib"
LIB="to_tex.cmxa"

# expr_core.mlはどの代数モジュールにも依存しない共通部品。Q_rootp・
# Quadratic・Quadratic_poly・Omega_field・Cubic・Elementaryはファンクタ（Make）
# なので、ここでコンパイルする時点では実体の代数モジュールを必要としない
# （読者がリンク時に自分のQ_rootp・Quadratic・Quadratic_poly・Elementaryを
# 渡してインスタンス化する）。
EXPR_UNITS="expr_core expr_integer expr_rational expr_complex \
expr_quaternion expr_octonion expr_hexadecanion expr_q_poly \
expr_root2_field expr_q_rootp expr_quadratic expr_quadratic_poly \
expr_omega_field expr_cubic expr_elementary expr"

clean() {
  echo "生成物を削除しています..."
  rm -f "$LIB_DIR"/*.cmi "$LIB_DIR"/*.cmx "$LIB_DIR"/*.o \
        "$LIB_DIR"/*.cmxa "$LIB_DIR"/*.a
  find "$CODE_DIR" -mindepth 2 \
    \( -name "*.cmi" -o -name "*.cmx" -o -name "*.o" -o -name "*.cmxa" -o -name "*.a" -o -name "a.out" \) \
    -delete
  echo "完了しました。"
}

if [ "$1" = "clean" ]; then
  clean
  exit 0
fi

echo "文字列化レイヤー(expr_*.ml)をビルドしています..."
(
  cd "$LIB_DIR"
  for u in $EXPR_UNITS; do
    if [ "$u" = "expr" ]; then
      ocamlopt -no-alias-deps -c "$u.ml"
    else
      ocamlopt -c "$u.ml"
    fi
  done
)

echo "to_tex.cmxaをビルドしています..."
(
  cd "$LIB_DIR"
  units=""
  for u in $EXPR_UNITS; do
    units="$units $u.cmx"
  done
  ocamlopt -a -o "$LIB" $units
)

echo "各節ディレクトリへ配置しています..."
find "$CODE_DIR" -mindepth 2 -maxdepth 2 -type d | while read -r dir; do
  cp "$LIB_DIR"/expr*.cmi "$LIB_DIR/$LIB" "$LIB_DIR/to_tex.a" "$dir"/
done

echo "完了しました。各節ディレクトリで、その節までに書いたmlファイルを"
echo "先に、to_tex.cmxaを後に並べてコンパイルできます。例:"
echo "  ocamlopt integer.ml to_tex.cmxa integer_test.ml && ./a.out"
echo "Q_rootp・Quadratic・Quadratic_poly・Omega_field・Cubicを使う場合は、"
echo "Expr.QuadraticのようにModule.Makeをそのまま渡すのではなく、"
echo "module Quadratic_expr = Expr.Quadratic.Make (Q_rootp) のように"
echo "自分のQ_rootpモジュールでその場でインスタンス化してください。"
echo "Elementaryを使う場合は、Q_rootpに加えてQuadratic・Quadratic_polyも渡します:"
echo "module Elementary_expr ="
echo "  Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)"
