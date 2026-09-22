#!/bin/sh
# lib/ から to_tex.cmxa をビルドし、各節ディレクトリへ配置する。
#
# 使い方:
#   ./build.sh        to_tex.cmxaをビルドして配置する
#   ./build.sh clean  生成物をすべて削除する
set -e

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
CODE_DIR="$ROOT_DIR/サンプルコード"
LIB_DIR="$ROOT_DIR/lib"
LIB="to_tex.cmxa"

EXPR_UNITS="expr_core expr_integer expr_rational expr_complex \
expr_quaternion expr_octonion expr_hexadecanion expr_q_poly \
expr_root2_field expr_q_rootp expr_quadratic expr_quadratic_poly \
expr_omega_field expr_cubic expr_elementary expr"

clean() {
  rm -f "$LIB_DIR"/*.cmi "$LIB_DIR"/*.cmx "$LIB_DIR"/*.o \
        "$LIB_DIR"/*.cmxa "$LIB_DIR"/*.a
  find "$CODE_DIR" -mindepth 2 \
    \( -name "*.cmi" -o -name "*.cmx" -o -name "*.o" -o -name "*.cmxa" -o -name "*.a" -o -name "a.out" \) \
    -delete
}

if [ "$1" = "clean" ]; then
  clean
  exit 0
fi

(
  cd "$LIB_DIR"
  for u in $EXPR_UNITS; do
    if [ "$u" = "expr" ]; then
      ocamlopt -no-alias-deps -c "$u.ml"
    else
      ocamlopt -c "$u.ml"
    fi
  done
  units=""
  for u in $EXPR_UNITS; do
    units="$units $u.cmx"
  done
  ocamlopt -a -o "$LIB" $units
)

find "$CODE_DIR" -mindepth 2 -maxdepth 2 -type d | while read -r dir; do
  cp "$LIB_DIR"/expr*.cmi "$LIB_DIR/$LIB" "$LIB_DIR/to_tex.a" "$dir"/
done

echo "完了しました。"
