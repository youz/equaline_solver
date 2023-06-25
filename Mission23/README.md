# Mission23 solver

## F#版

### ビルド方法

```
> dotnet build --configuration Release
```

または

```
> make-standalone.bat
```


### 使い方

```
usage: solve_m23.exe [-sNN] [-nNN] [-m] [-c] [-v] [-h]
  [options]
     -sNN : NN番目のステージまで解く (未指定なら10)
     -nNN : 移動回数の少ない順にNN件の解答を出力する (未指定なら1)
     -m   : 盤面は表示せずカーソル移動手順のみ出力する
     -c   : 解の数のみを出力する
     -v   : 各ステージの解の数を表示
     -h   : このヘルプを表示
```


## Common Lisp版 (プロトタイプ)

### 使い方

```
> sbcl --script solve_m23.lisp 10
```
