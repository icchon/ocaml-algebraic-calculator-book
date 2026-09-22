# 関数型言語OCamlで作る代数計算機 サンプルコード

書籍『関数型言語OCamlで作る代数計算機』のサンプルコードリポジトリです。

## セットアップ

```sh
git clone https://github.com/icchon/ocaml-algebraic-calculator-book.git
cd ocaml-algebraic-calculator-book
```

```sh
brew install opam       # macOS
sudo apt install opam   # Debian/Ubuntu(Windowsの場合はWSL上で実行)
```

```sh
opam init
opam switch create default 5.5.1
eval $(opam env)
ocaml -version   # => The OCaml toplevel, version 5.5.1
```

```sh
ocamlopt sample.ml && ./a.out   # => hello OCaml
```

```sh
./build.sh
```

## サンプルコードを動かす

`サンプルコード/`以下は、本書の章・節に対応するディレクトリです。

```
サンプルコード/
├── 01_基礎的な代数をつくる/
│   ├── 01_整数/ 02_有理数/ 03_複素数/ 04_ベクトル/ 05_多項式/ 06_体の拡大/
├── 02_二次方程式/01_二次方程式を解く/
├── 03_微分・積分/01_微分を導入する/ 02_積分を導入する/ 03_2階線形微分方程式を解く/
└── 04_三次方程式/01_三次方程式を解くために必要なもの/ 02_三次方程式を解く/ 03_なぜ三次方程式までなのか/
```

`04_三次方程式/`は現在加筆中の章です。

本文どおりに書いた`.ml`ファイルを、対応するディレクトリに置いてコンパイルします。

```sh
cd サンプルコード/01_基礎的な代数をつくる/01_整数
ocamlopt add_group.ml mul_monoid.ml euclidean.ml integer.ml to_tex.cmxa integer_test.ml && ./a.out
```

依存する`.ml`ファイルを先に、`to_tex.cmxa`を後に並べてください。

生成物を消すには`./build.sh clean`を実行してください。

## ライセンス

[LICENSE](./LICENSE)
