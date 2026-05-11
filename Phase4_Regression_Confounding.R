###############################################################################
#  フェーズ4：回帰分析と交絡の概念
#  目的：連続変数をアウトカムとする多変量解析
#        交絡の概念と研究デザイン / バイアスを避けた因果関係の検討
#
#  使用データ：NHANES（米国国民健康栄養調査 2009-2012）
###############################################################################

library(tidyverse)

df <- read.csv("nhanes_adult_processed.csv", stringsAsFactors = TRUE)
str(df)


# =============================================================================
# Part A: 交絡（Confounding）の概念 ← フェーズ5前半
# =============================================================================

# 【交絡（Confounding）とは？】
# ある曝露（X）とアウトカム（Y）の間の見かけの関連が、
# 第三の変数（交絡因子: C）によって歪められること。
#
# 交絡因子の条件（3つすべてを満たす）：
#   1. アウトカム（Y）のリスク因子である
#   2. 曝露（X）と関連がある
#   3. 曝露（X）の結果（中間変数）ではない
#
#   C (交絡因子)
#  / \
# v   v
# X → Y   ← Cを考慮しないとXとYの関連が歪む

# --- NHANESでの交絡の実例 ---
# 問い：「運動習慣（PhysActive）は血圧（BPSysAve）を下げるか？」
# 考えられる交絡因子：年齢（Age）
#   → 高齢者は運動量が減り（X と関連）、かつ血圧が上がる（Y のリスク因子）

# ステップ1：曝露とアウトカムの粗い関連を見る
df %>%
  filter(!is.na(PhysActive) & !is.na(BPSysAve)) %>%
  group_by(PhysActive) %>%
  summarise(
    n        = n(),
    mean_SBP = mean(BPSysAve),
    mean_Age = mean(Age)       # ← 交絡の手がかり
  )
# → 運動しない群は血圧が高い。しかし年齢も高い！

# ステップ2：交絡因子と曝露の関連を確認
df %>%
  filter(!is.na(PhysActive)) %>%
  group_by(PhysActive) %>%
  summarise(mean_Age = mean(Age))
# → 運動しない群の方が年齢が高い → 交絡の存在を示唆

# ステップ3：粗解析 vs 調整解析で比較
# 粗解析（交絡を無視）
model_crude <- lm(BPSysAve ~ PhysActive, data = df)
summary(model_crude)

# 調整解析（年齢で調整）
model_adj <- lm(BPSysAve ~ PhysActive + Age, data = df)
summary(model_adj)

cat("\n--- 交絡の検証：運動習慣 → 血圧 ---\n")
cat("粗解析のPhysActive係数:",
    round(coef(model_crude)["PhysActiveYes"], 2), "\n")
cat("調整解析のPhysActive係数:",
    round(coef(model_adj)["PhysActiveYes"], 2), "\n")
cat("→ 年齢で調整すると運動の効果（係数）がどう変わるか注目！\n")

# 可視化：年齢層別に運動と血圧の関連を見る
ggplot(df %>% filter(!is.na(PhysActive) & !is.na(BPSysAve) & !is.na(Age_group)),
       aes(x = PhysActive, y = BPSysAve, fill = PhysActive)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Age_group) +     # 年齢層で分割
  labs(title = "年齢層別：運動習慣と収縮期血圧（NHANES）",
       subtitle = "年齢という交絡因子を層別化で制御",
       x = "運動習慣", y = "収縮期血圧 (mmHg)") +
  theme_minimal()


# =============================================================================
# Part B: 単回帰分析（Simple Linear Regression） ← フェーズ4
# =============================================================================
# 【解説】
# 単回帰分析：1つの説明変数（X）で1つのアウトカム（Y）を予測する
#   Y = β0 + β1 * X + ε
#   β0 = 切片（Xが0のときのYの期待値）
#   β1 = 回帰係数（Xが1単位増加したときのYの変化量）
#   ε  = 誤差項

# --- 例：年齢（Age）→ 収縮期血圧（BPSysAve）---
model_simple <- lm(BPSysAve ~ Age, data = df)
summary(model_simple)

# 【出力の読み方】
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)
# (Intercept)  β0        SE(β0)      t値      p値     ← 切片
# Age          β1        SE(β1)      t値      p値     ← 回帰係数
#
# Multiple R-squared:  決定係数（モデルの説明力、0〜1）
# F-statistic:         モデル全体の有意性検定

cat("\n--- 単回帰分析：年齢 → 収縮期血圧 ---\n")
cat("切片:", round(coef(model_simple)[1], 2), "\n")
cat("年齢の係数:", round(coef(model_simple)[2], 3), "\n")
cat("解釈：年齢が1歳上がると血圧が約",
    round(coef(model_simple)[2], 2), "mmHg上昇\n")
cat("決定係数 R²:", round(summary(model_simple)$r.squared, 4), "\n")

# 95%信頼区間
confint(model_simple)

# 可視化
ggplot(df, aes(x = Age, y = BPSysAve)) +
  geom_point(alpha = 0.2, na.rm = TRUE) +
  geom_smooth(method = "lm", color = "red", na.rm = TRUE) +
  labs(title = "単回帰：年齢と収縮期血圧（NHANES）",
       x = "年齢 (歳)", y = "収縮期血圧 (mmHg)") +
  theme_minimal()


# --- 例2：BMI → 収縮期血圧 ---
model_bmi <- lm(BPSysAve ~ BMI, data = df)
summary(model_bmi)
cat("\nBMIの係数:", round(coef(model_bmi)[2], 3),
    "→ BMIが1増えると血圧が約", round(coef(model_bmi)[2], 2), "mmHg上昇\n")


# =============================================================================
# Part C: 重回帰分析（Multiple Linear Regression） ← フェーズ5後半
# =============================================================================
# 【解説】
# 重回帰分析：複数の説明変数で1つのアウトカムを予測する
#   Y = β0 + β1*X1 + β2*X2 + ... + βp*Xp + ε
#
# 各βi は「他の変数を一定に保った場合の」Xiの効果
# → 交絡を統計的に調整（コントロール）できる

# 【重回帰の仮定】
# 1. 線形性：XとYの関係が直線的
# 2. 独立性：残差が互いに独立
# 3. 等分散性（ホモスケダスティシティ）：残差の分散が一定
# 4. 正規性：残差が正規分布に従う
# 5. 多重共線性がない：説明変数間に強い相関がない

# --- 重回帰モデルの段階的構築 ---

# モデル1：年齢のみ（単回帰）
model1 <- lm(BPSysAve ~ Age, data = df)

# モデル2：年齢 + 性別
model2 <- lm(BPSysAve ~ Age + Gender, data = df)

# モデル3：年齢 + 性別 + BMI + 糖尿病 + 人種（フルモデル）
model3 <- lm(BPSysAve ~ Age + Gender + BMI + Diabetes + Race1,
             data = df)

# --- 結果の比較 ---
summary(model1)
summary(model2)
summary(model3)

# 【回帰係数の解釈（モデル3）】
cat("\n--- 重回帰（フルモデル）の係数 ---\n")
coefs <- summary(model3)$coefficients
print(round(coefs, 3))

cat("\n解釈例：\n")
cat("- Age の係数:", round(coefs["Age", "Estimate"], 2),
    "\n  → 他の変数を調整した上で、年齢が1歳上がると\n",
    "    血圧が約", round(coefs["Age", "Estimate"], 2), "mmHg上昇\n")
cat("- Gendermale の係数:", round(coefs["Gendermale", "Estimate"], 2),
    "\n  → 女性と比べて男性は血圧が約",
    round(abs(coefs["Gendermale", "Estimate"]), 2), "mmHg",
    ifelse(coefs["Gendermale", "Estimate"] > 0, "高い", "低い"), "\n")
cat("- BMI の係数:", round(coefs["BMI", "Estimate"], 2),
    "\n  → BMIが1 kg/m²増えると血圧が約",
    round(coefs["BMI", "Estimate"], 2), "mmHg上昇\n")

# 95%信頼区間
cat("\n95%信頼区間:\n")
print(round(confint(model3), 2))


# --- モデルの診断 ---

# 仮定の確認：残差プロット
par(mfrow = c(2, 2))
plot(model3)
par(mfrow = c(1, 1))

# 各プロットの解釈：
# 1. Residuals vs Fitted : 残差にパターンがなければOK（線形性・等分散性）
# 2. Normal Q-Q          : 点が対角線上に並べばOK（正規性）
# 3. Scale-Location      : 水平な線ならOK（等分散性）
# 4. Residuals vs Leverage: Cookの距離が大きい点は影響力大（外れ値）

# --- 多重共線性の確認（VIF） ---
# VIF > 10 → 多重共線性の問題あり（一般的な基準）
# install.packages("car")
library(car)
vif(model3)
cat("\n多重共線性（VIF）: すべて10未満ならOK\n")


# --- モデルの比較（AIC / 調整済みR²） ---
cat("\n--- モデル比較 ---\n")
cat("モデル1 (Age のみ):      AIC =", round(AIC(model1)),
    "  Adj.R² =", round(summary(model1)$adj.r.squared, 4), "\n")
cat("モデル2 (Age+Gender):    AIC =", round(AIC(model2)),
    "  Adj.R² =", round(summary(model2)$adj.r.squared, 4), "\n")
cat("モデル3 (フルモデル):    AIC =", round(AIC(model3)),
    "  Adj.R² =", round(summary(model3)$adj.r.squared, 4), "\n")
# AIC が小さいほど良いモデル
# 調整済みR² が大きいほど説明力が高い

# --- ANOVA によるネストされたモデルの比較 ---
anova(model1, model2, model3)
# p値が有意 → 変数を追加することでモデルが有意に改善


# =============================================================================
# 補足：カテゴリ変数の扱い
# =============================================================================
# 【解説】
# Rは factor 型のカテゴリ変数を自動的にダミー変数に変換します。
# 例：Race1 = {Black, Hispanic, Mexican, White, Other}
#   → 参照カテゴリ（アルファベット順で最初: Black）に対する
#     各カテゴリのダミー変数が作られる

# 参照カテゴリの変更（Whiteを参照に）
df$Race1_relevel <- relevel(df$Race1, ref = "White")
model_race <- lm(BPSysAve ~ Age + Race1_relevel, data = df)
summary(model_race)
# → 各人種の係数は「Whiteと比べた」血圧の差として解釈

cat("\n===== フェーズ4 完了 =====\n")


# =============================================================================
# ★ 復習問題
# =============================================================================
#
# --- 基本問題（必須） ---
#
# 【問1】単回帰と回帰係数の解釈
#   BMI を説明変数、拡張期血圧（BPDiaAve）をアウトカムとする
#   単回帰モデルを作成してください。
#   (a) BMI の回帰係数を解釈してください（「BMIが1増えると…」）。
#   (b) 決定係数 R² はいくつですか？モデルの説明力はどの程度ですか？
#
# 【問2】交絡の検出
#   「BMI は収縮期血圧（BPSysAve）を上げるか？」を検討します。
#   (a) BMI → BPSysAve の単回帰モデルを作り、BMIの係数を記録してください。
#   (b) Age（年齢）を加えた重回帰モデルを作り、BMIの係数を比較してください。
#   (c) 係数が変わった場合、その理由を交絡の観点から説明してください。
#
#
# --- ボーナス問題（加点対象） ---
#
# 【ボーナス1】
#   BPSysAve をアウトカムとして以下の3モデルを比較してください。
#     モデルA: ~ Age + Gender
#     モデルB: ~ Age + Gender + BMI
#     モデルC: ~ Age + Gender + BMI + Diabetes + PhysActive
#   調整済みR²とAICを比較し、最適モデルを選択してください。
#   さらにモデルCの残差プロット（plot()）を確認し、
#   重回帰の仮定が満たされているか評価してください。
#
# 【ボーナス2】
#   NHANESデータで「運動習慣（PhysActive）→ 総コレステロール（TotChol）」
#   の関連を調べる場合、交絡因子になりうる変数を2つ以上挙げ、
#   それぞれが交絡因子の3条件を満たす理由を説明してください。
#   その上で、交絡を調整した重回帰モデルを実際に構築してください。
