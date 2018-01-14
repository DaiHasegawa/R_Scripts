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

# factorのカラム（独立変数）に名前をつける
x = factor(data[, 2])


# t検定
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

  # 2群で同じ参加者なので分散の等質性の検定は不要

  # t検定
  cat("\n--- t検定 ---\n")
  print( t.test(y ~ x, paired=TRUE) )
}
