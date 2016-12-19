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

# 参加者のカラムに名前をつける
subject = data[, 1]

# factorのカラム（独立変数）に名前をつける
factor = data[, 2]


# 分散の等質性の検定とt検定
for ( i in 3:ncol(data) )
{
  cat("\n########################\n")
  cat(columns[i])
  cat("\n########################\n")

  # データカラム（従属変数）に名前をつける
  variable = data[, i]

  # 要約
  cat("\n--- Summary: factor ---\n")
  print(by(variable, factor, summary))

  # ANOVA (within: factor_1, between: factor_2)
  cat("\n-- ANOVA ---\n")
  print(summary(aov(variable ~ factor)))

  # 多重検定
  cat("\n-- 多重検定 ---\n")
  print(pairwise.t.test(variable, factor, p.adjust.method="bonferroni"))
}
