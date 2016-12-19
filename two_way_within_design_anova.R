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
factor_1 = data[, 2] # within
factor_2 = data[, 3] # within


# 分散の等質性の検定とt検定
for ( i in 4:ncol(data) )
{
  cat("\n########################\n")
  cat(columns[i])
  cat("\n########################\n")

  # データカラム（従属変数）に名前をつける
  variable = data[, i]

  # 要約
  cat("\n--- Summary: factor 1 ---\n")
  print(by(variable, factor_1, summary))
  cat("\n-- Summary: factor 2 ---\n")
  print(by(variable, factor_2, summary))

  # ANOVA (within: factor_1, between: factor_2)
  cat("\n-- ANOVA ---\n")
  print(summary(aov(variable ~ factor_1 * factor_2 + Error(subject/(factor_1 * factor_2)))))

  # 多重検定
  cat("\n-- 多重検定 ---\n")
  print(pairwise.t.test(variable, factor_1:factor_2, paired=TRUE, p.adjust.method="bonferroni"))
}
