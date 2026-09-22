# 関数型言語OCamlで作る代数計算機 サンプルコード

書籍『関数型言語OCamlで作る代数計算機』のサンプルコードリポジトリです。本文に登場する実装コードを、章・節ごとにそのまま動かせる形で収録しています。

## セットアップ

```sh
git clone https://github.com/icchon/ocaml-algebraic-calculator-book.git
cd ocaml-algebraic-calculator-book
```

OCamlは[opam](https://opam.ocaml.org/)経由でインストールします。

```sh
brew install opam       # macOS
```

```sh
sudo apt install opam   # Debian/Ubuntu(Windowsの場合はWSL上で実行)
```

```sh
opam init
opam switch create default 5.2.1
eval $(opam env)
ocaml -version   # => The OCaml toplevel, version 5.2.1
```

動作確認として、リポジトリ直下の`sample.ml`を実行してみてください。

```sh
ocamlopt sample.ml && ./a.out   # => hello OCaml
```

## サンプルコードを動かす

`サンプルコード/`以下には、本書の章・節にそのまま対応するディレクトリが並んでいます。

```
サンプルコード/
├── 01_基礎的な代数をつくる/
│   ├── 01_整数/ 02_有理数/ 03_複素数/ 04_ベクトル/ 05_多項式/ 06_体の拡大/
├── 02_二次方程式/01_二次方程式を解く/
├── 03_微分・積分/01_微分を導入する/ 02_積分を導入する/ 03_2階線形微分方程式を解く/
└── 04_三次方程式/01_三次方程式を解くために必要なもの/ 02_三次方程式を解く/ 03_なぜ三次方程式までなのか/
```

`04_三次方程式/`は現在加筆中の章です。

各ディレクトリに`.ml`ファイルは置いていません（本文と一緒に変わっていくものなので、ここに固定のコピーは置かない方針です）。代わりに、本文の通りに書いた自分の`integer.ml`・`rational.ml`のようなファイルと、その節までに登場した計算結果を文字列に変換するための共通ライブラリ(`to_tex.cmxa`)を、同じディレクトリでコンパイルして使います。

クローン後に一度だけ、リポジトリ直下で次を実行してください。

```sh
./build.sh
```

`lib/`のソースから`to_tex.cmxa`一式がビルドされ、すべての節ディレクトリへ配置されます。`to_tex.cmxa`には、代数モジュール（Integer・Rational・Q_rootpなど）の実装そのものは含まれていません。含まれているのは、それらをTeXの文字列に変換するためのコードだけです。しかもこの変換コードは、代数モジュールの構造的な形（intのペアである、リストであるなど）だけを直接利用し、代数モジュールの実体そのものをコンパイル時にすら必要としません。そのため、あなたが本文どおりに書いた（まだ完成していない・後の章の関数を含まない）`integer.ml`などを、そのまま`to_tex.cmxa`とリンクできます（詳しくは`build.sh`内のコメントを参照）。

あとは好きな節のディレクトリに移動し、その節までに本文で書いた`.ml`ファイルを置いて、本文と同じコマンドで動かせます。たとえば「01_整数」なら、本文どおりに書いた`add_group.ml`・`mul_monoid.ml`・`euclidean.ml`・`integer.ml`・`integer_test.ml`をそのディレクトリに置いて、

```sh
cd サンプルコード/01_基礎的な代数をつくる/01_整数
ocamlopt add_group.ml mul_monoid.ml euclidean.ml integer.ml to_tex.cmxa integer_test.ml && ./a.out
```

のようにコンパイルします。**依存する`.ml`ファイルを先に、`to_tex.cmxa`を後に**並べる必要があります（OCamlのネイティブリンカーは左から右に依存を解決するため）。この順序さえ守れば、その節でまだ登場していない代数モジュール（`Q_rootp`や`Elementary`など）を要求されることはありません。

### Q_rootp・Quadratic・Quadratic_poly・Omega_field・Cubic・Elementaryを表示する場合

`Expr.Integer`・`Expr.Rational`のような多くの型は、そのまま`(module Expr.Integer)`の形で使えます。しかし`Q_rootp`（平方根を並列に添加した体）やそれを土台にした`Quadratic`・`Quadratic_poly`・`Omega_field`・`Cubic`・`Elementary`は、内部がOCaml標準ライブラリの`Map`という抽象的な構造になっているため、`to_tex.cmxa`側に固定の実装を1つだけ用意しておくことができません。かわりに、これらは自分のモジュールを渡してその場でインスタンス化するファンクタ（`Expr.Quadratic.Make`など）になっています。

```ocaml
module Quadratic_expr = Expr.Quadratic.Make (Q_rootp)
(* あとは (module Quadratic_expr) を (module Expr.Integer) と同じように使える *)
```

`Elementary`だけは、指数の有理関数部分の判定や分母が1かどうかの判定に`Q_rootp`の表示情報だけでは足りないため、`Quadratic`・`Quadratic_poly`の実体もあわせて渡します。

```ocaml
module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)
```

生成物を消したいときは`./build.sh clean`を実行してください。

## ライセンス

MIT License。詳細は[LICENSE](./LICENSE)を参照してください。
