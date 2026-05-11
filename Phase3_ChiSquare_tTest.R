###############################################################################
#  フェーズ3：基礎的な統計的関連の推計と解釈
#  目的：基礎的な統計的検定と臨床研究における応用 / 単純な関連性の評価
#  トピック：カイ二乗検定、t検定
#
#  使用データ：NHANES（米国国民健康栄養調査 2009-2012）
###############################################################################

library(tidyverse)

# フェーズ2で加工したデータを読み込み
df <- read.csv("nhanes_adult_processed.csv", stringsAsFactors = TRUE)
str(df)


# =============================================================================
# 1. カイ二乗検定（Chi-squared Test）
# =============================================================================
# 【目的】
# 2つのカテゴリ変数の間に関連があるかを検定する
#
# 【仮説】
#   H0（帰無仮説）: 2変数は独立である（関連がない）
#   H1（対立仮説）: 2変数は独立でない（関連がある）
#
# 【仮定（前提条件）】
#   1. データが独立した観測である
#   2. 期待度数がすべてのセルで5以上（目安）
#      → 5未満のセルが多い場合はフィッシャーの正確検定を使う

# --- 例1：性別（Gender）と糖尿病（Diabetes）の関連 ---

# まず、クロス集計表を作成
cross_table <- table(df$Gender, df$Diabetes)
cross_table

# 行の割合を確認（各性別における糖尿病の割合）
prop.table(cross_table, margin = 1)  # margin=1: 行の合計で割る
# 解釈：男女で糖尿病の割合に差があるか？

# クロス集計の可視化
ggplot(df %>% filter(!is.na(Diabetes)),
       aes(x = Gender, fill = Diabetes)) +
  geom_bar(position = "fill") +               # 割合で表示
  labs(title = "性別ごとの糖尿病の割合（NHANES）",
       x = "性別", y = "割合", fill = "糖尿病") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

# --- カイ二乗検定の実行 ---
chi_result <- chisq.test(cross_table)
chi_result

# 【出力の読み方】
# X-squared = カイ二乗統計量（大きいほど関連が強い可能性）
# df        = 自由度（(行数-1) × (列数-1)）
# p-value   = p値（< 0.05 なら統計的に有意）

# 期待度数の確認（仮定の確認）
chi_result$expected
# → すべてのセルで5以上か確認

cat("\n--- カイ二乗検定の結果：性別 × 糖尿病 ---\n")
cat("カイ二乗統計量:", chi_result$statistic, "\n")
cat("自由度:", chi_result$parameter, "\n")
cat("p値:", format(chi_result$p.value, digits = 4), "\n")
if (chi_result$p.value < 0.05) {
  cat("→ p < 0.05：性別と糖尿病の間に統計的に有意な関連がある\n")
} else {
  cat("→ p >= 0.05：性別と糖尿病の間に統計的に有意な関連は認められない\n")
}


# --- 例2：喫煙状態（Smoking_status）と糖尿病（Diabetes）の関連 ---
cross_table2 <- table(df$Smoking_status, df$Diabetes)
cross_table2
prop.table(cross_table2, margin = 1)
chisq.test(cross_table2)

# 可視化
ggplot(df %>% filter(!is.na(Diabetes) & Smoking_status != "不明"),
       aes(x = Smoking_status, fill = Diabetes)) +
  geom_bar(position = "fill") +
  labs(title = "喫煙状態ごとの糖尿病の割合（NHANES）",
       x = "喫煙状態", y = "割合", fill = "糖尿病") +
  theme_minimal()


# --- 例3：人種（Race1）と糖尿病（Diabetes）の関連 ---
cross_table3 <- table(df$Race1, df$Diabetes)
cross_table3
chi3 <- chisq.test(cross_table3)
chi3
chi3$expected  # 期待度数を確認


# --- フィッシャーの正確検定（期待度数が小さい場合） ---
# 【解説】
# 期待度数が5未満のセルがある場合、カイ二乗検定は不正確になります。
# フィッシャーの正確検定は正確な確率を計算するため、
# サンプルサイズが小さい場合に推奨されます。

# 2×2表の場合のみ直接使用可能
fisher.test(table(df$Gender, df$Diabetes))

# 大きな表ではシミュレーションベースで実行
fisher.test(cross_table3, simulate.p.value = TRUE, B = 10000)


# =============================================================================
# 2. t検定（t-test）
# =============================================================================
# 【目的】
# 2つのグループ間で連続変数の平均値に差があるかを検定する
#
# 【仮説】
#   H0（帰無仮説）: 2群の母平均は等しい（μ1 = μ2）
#   H1（対立仮説）: 2群の母平均は異なる（μ1 ≠ μ2）
#
# 【仮定（前提条件）】
#   1. 各群のデータが正規分布に従う（またはサンプルサイズが十分大きい）
#   2. 2群は独立している（対応のないt検定の場合）
#   3. 等分散性（Studentのt検定の場合）
#      → 等分散でない場合は Welch の t検定を使う（Rのデフォルト）

# --- 例1：性別ごとの収縮期血圧の比較 ---

# まず、各群の記述統計量を確認
df %>%
  filter(!is.na(BPSysAve)) %>%
  group_by(Gender) %>%
  summarise(
    n       = n(),
    mean    = mean(BPSysAve),
    sd      = sd(BPSysAve),
    median  = median(BPSysAve)
  )

# 箱ひげ図で視覚的に確認
ggplot(df, aes(x = Gender, y = BPSysAve, fill = Gender)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "性別ごとの収縮期血圧（NHANES）",
       x = "性別", y = "収縮期血圧 (mmHg)") +
  theme_minimal()

# --- 仮定の確認1：正規性の検定（Shapiro-Wilk検定） ---
# H0: データは正規分布に従う → p > 0.05 なら正規性を仮定できる
# 注意：サンプルサイズが5000以上だと使えないので、ランダムサンプルで実行
set.seed(42)
male_bp   <- df$BPSysAve[df$Gender == "male" & !is.na(df$BPSysAve)]
female_bp <- df$BPSysAve[df$Gender == "female" & !is.na(df$BPSysAve)]
shapiro.test(sample(male_bp, min(5000, length(male_bp))))
shapiro.test(sample(female_bp, min(5000, length(female_bp))))

# Q-Qプロットで視覚的に確認
par(mfrow = c(1, 2))
qqnorm(male_bp, main = "男性 Q-Qプロット"); qqline(male_bp, col = "red")
qqnorm(female_bp, main = "女性 Q-Qプロット"); qqline(female_bp, col = "red")
par(mfrow = c(1, 1))

# 【ポイント】
# サンプルサイズが大きい場合（n > 30〜50）は
# 中心極限定理によりt検定はロバスト（正規性からの逸脱に強い）

# --- 仮定の確認2：等分散性の検定（F検定） ---
var.test(BPSysAve ~ Gender, data = df)
# p > 0.05 → 等分散と仮定できる


# --- t検定の実行 ---

# Welch の t検定（等分散を仮定しない → Rのデフォルト、推奨）
t_result <- t.test(BPSysAve ~ Gender, data = df)
t_result

# Student の t検定（等分散を仮定する場合）
t_result_eq <- t.test(BPSysAve ~ Gender, data = df, var.equal = TRUE)
t_result_eq

# 【出力の読み方】
# t       = t統計量
# df      = 自由度
# p-value = p値
# 95 percent confidence interval = 平均値の差の95%信頼区間
# mean in group female / male = 各群の平均値

cat("\n--- t検定の結果：性別 × 収縮期血圧 ---\n")
cat("t統計量:", round(t_result$statistic, 3), "\n")
cat("自由度:", round(t_result$parameter, 1), "\n")
cat("p値:", format(t_result$p.value, digits = 4), "\n")
cat("平均値の差の95%CI:", round(t_result$conf.int, 2), "\n")
cat("女性平均:", round(t_result$estimate[1], 1),
    "男性平均:", round(t_result$estimate[2], 1), "\n")


# --- 例2：糖尿病有無ごとのBMIの比較 ---
df %>%
  filter(!is.na(Diabetes) & !is.na(BMI)) %>%
  group_by(Diabetes) %>%
  summarise(
    n      = n(),
    mean   = mean(BMI),
    sd     = sd(BMI)
  )

t.test(BMI ~ Diabetes, data = df)

# 可視化
ggplot(df %>% filter(!is.na(Diabetes)),
       aes(x = Diabetes, y = BMI, fill = Diabetes)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "糖尿病有無ごとのBMI（NHANES）",
       x = "糖尿病", y = "BMI (kg/m²)") +
  theme_minimal()


# --- 例3：運動習慣の有無ごとの血圧比較 ---
t.test(BPSysAve ~ PhysActive, data = df)


# =============================================================================
# 3. 正規性が仮定できない場合：ノンパラメトリック検定
# =============================================================================
# 【解説】
# データが正規分布に従わない場合（外れ値が多い、歪みが強い等）、
# ノンパラメトリック検定を使います。
#   - ウィルコクソンの順位和検定（Mann-Whitney U検定）→ t検定の代替
#   - ウィルコクソンの符号付順位検定 → 対応のあるt検定の代替

# Mann-Whitney U検定
wilcox.test(BPSysAve ~ Gender, data = df)

# 【ポイント】
# NHANESのようにサンプルサイズが大きい場合は
# t検定とほぼ同じ結果になることが多い


# =============================================================================
# 4. 結果のまとめ方（臨床論文のTable 1 スタイル）
# =============================================================================
# 【解説】
# 臨床研究の論文では、最初の表（Table 1）で患者背景を
# 群別にまとめるのが慣例です。

# tableone パッケージを使うと簡単に作れます
# install.packages("tableone")
library(tableone)

# Table 1 の作成：性別で層別化
vars     <- c("Age", "BMI", "BPSysAve", "BPDiaAve",
              "TotChol", "DirectChol", "Diabetes",
              "Smoking_status", "PhysActive", "Race1")
cat_vars <- c("Diabetes", "Smoking_status", "PhysActive", "Race1")

tab1 <- CreateTableOne(
  vars       = vars,
  strata     = "Gender",        # 群分け変数
  data       = df,
  factorVars = cat_vars,
  test       = TRUE             # 検定も実施
)

# 結果の表示（連続変数はmean±SD、カテゴリ変数は度数（%））
print(tab1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# 正規分布でない変数は中央値[IQR]で表示
print(tab1, showAllLevels = TRUE, nonnormal = c("BPSysAve", "BPDiaAve"))

# CSVとして保存（論文用）
tab1_df <- print(tab1, showAllLevels = TRUE, printToggle = FALSE)
write.csv(tab1_df, "Table1_by_Gender_NHANES.csv")

cat("\n===== フェーズ3 完了 =====\n")
cat("Table 1 を 'Table1_by_Gender_NHANES.csv' として保存しました。\n")


# =============================================================================
# ★ 復習問題
# =============================================================================
#
# --- 基本問題（必須） ---
#
# 【問1】カイ二乗検定
#   運動習慣（PhysActive）と糖尿病（Diabetes）の関連をカイ二乗検定で
#   検定してください。
#   (a) クロス集計表を作成し、各群の糖尿病の割合を求めてください。
#   (b) カイ二乗検定を実行し、p値から「有意/有意でない」を判断してください。
#
# 【問2】t検定
#   糖尿病の有無（Diabetes）ごとの BMI を比較してください。
#   (a) 各群の平均値・SDを求め、t検定を実行してください。
#   (b) 95%信頼区間は何を意味していますか？ 自分の言葉で説明してください。
#
#
# --- ボーナス問題（加点対象） ---
#
# 【ボーナス1】
#   糖尿病の有無（Diabetes）で層別化した Table 1 を tableone パッケージで
#   作成してください。変数は Age, BMI, BPSysAve, Gender, PhysActive, Race1。
#   Table 1 から読み取れる「糖尿病あり群の特徴」を3つ以上挙げてください。
#
# 【ボーナス2】
#   t検定で「p < 0.001」という結果が出た場合、それは「臨床的に意味のある差」
#   を必ず意味しますか？ NHANESのようにサンプルサイズが大きいデータでは
#   「統計的有意」と「臨床的意義」にどのような乖離が生じうるか、
#   具体例を挙げて説明してください。
