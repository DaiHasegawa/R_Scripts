# 引数取得
args <- commandArgs(trailingOnly = T)

# csvファイルの読み込み
data = read.csv(args[1])

# データの確認
cat("\n### Data Format ###\n")
print( str(data) )

# データの要約
cat("\n### Data Summary ###\n")
print( summary(data))

# カラム名を取り出す
columns = colnames(data)

# 独立変数に名前をつける
x = factor(data[, 2])


# 分散の等質性の検定とt検定
for ( i in 3:ncol(data) )
{
  cat("\n########################\n")
  cat(columns[i])
  cat("\n########################\n")

  # データカラム（従属変数）に名前をつける
  y = data[, i]

  # 要約
  cat("\n--- summary ---\n")
  print(by(y, x, summary))
  cat("\n--- standard Deviation ---\n")
  print(by(y, x, sd))

  # 分散の等質性の検定
  cat("\n--- 分散の等質性（ p-value < .05 の場合はt検定はできない） ---\n")
  print( var.test(y ~ x) )

  # t検定
  cat("\n--- t検定 ---\n")
  print( t.test(y ~ x, var.equal=TRUE) )
}
