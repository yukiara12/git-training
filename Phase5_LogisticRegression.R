###############################################################################
#  フェーズ5：公衆衛生・臨床研究で多用される多変量解析
#  目的：二値アウトカムに対応した多変量解析
#        罹患や死亡など、臨床・公衆衛生で重要なアウトカムの推計
#  トピック：ロジスティック回帰（+ 重回帰の再確認）
#
#  使用データ：NHANES（米国国民健康栄養調査 2009-2012）
#  アウトカム：Diabetes（糖尿病の有無）
###############################################################################

library(tidyverse)

df <- read.csv("nhanes_adult_processed.csv", stringsAsFactors = TRUE)

# 糖尿病を二値変数として確認
table(df$Diabetes, useNA = "always")
# DM 変数（0/1）はフェーズ2で作成済み
table(df$DM, useNA = "always")


# =============================================================================
# 1. ロジスティック回帰の基礎
# =============================================================================
# 【なぜロジスティック回帰が必要か？】
# 重回帰分析（lm）は連続アウトカム向けです。
# アウトカムが二値（0/1、例：糖尿病あり/なし）の場合、
# 通常の重回帰では以下の問題が起きます：
#   - 予測値が0〜1の範囲を超えてしまう
#   - 残差が正規分布にならない
#   - 等分散性が成り立たない
#
# ロジスティック回帰はこれらの問題を解決します：
#   log(p / (1-p)) = β0 + β1*X1 + β2*X2 + ...
#   左辺は「ログオッズ（logit）」
#   p = アウトカムが1になる確率（ここでは糖尿病になる確率）
#
# 【例で理解するオッズとオッズ比】
# 糖尿病の割合が20%の場合：
#   確率 p = 0.20
#   オッズ = p/(1-p) = 0.20/0.80 = 0.25（= "4人に1人"）
#   ログオッズ = log(0.25) = -1.39

# 【ロジスティック回帰の仮定】
# 1. アウトカムが二値（0 or 1）
# 2. 観測が独立
# 3. 説明変数とログオッズの関係が線形
# 4. 多重共線性がない
# 5. 十分なサンプルサイズ（目安：イベント数/変数数 ≧ 10〜20）

# イベント数の確認
cat("糖尿病あり:", sum(df$DM == 1, na.rm = TRUE), "人\n")
cat("糖尿病なし:", sum(df$DM == 0, na.rm = TRUE), "人\n")
cat("→ EPV(Events Per Variable)を確認して変数数を決める\n")


# =============================================================================
# 2. ロジスティック回帰の実装
# =============================================================================

# --- 2-1. 単変量ロジスティック回帰 ---
# アウトカム = DM（糖尿病 0/1）、説明変数 = BMI

model_logit1 <- glm(DM ~ BMI, data = df, family = binomial)
summary(model_logit1)

# 【出力の読み方】
# Coefficients:
#              Estimate  Std. Error  z value  Pr(>|z|)
# (Intercept)  β0        SE          z統計量  p値
# BMI          β1        SE          z統計量  p値
#
# ※ t検定ではなく z検定（Wald検定）を使用
# ※ Estimate はログオッズスケール（直感的に解釈しにくい）


# --- 2-2. オッズ比（OR）への変換 ---
# 【重要】
# ロジスティック回帰の係数はログオッズ → exp() でオッズ比に変換！

OR1 <- exp(coef(model_logit1))
CI1 <- exp(confint(model_logit1))

cat("\n--- 単変量ロジスティック回帰：BMI → 糖尿病 ---\n")
cat("BMIのオッズ比 (OR):", round(OR1["BMI"], 3), "\n")
cat("95% CI:", round(CI1["BMI", 1], 3), "-", round(CI1["BMI", 2], 3), "\n")

# 【オッズ比の解釈】
# 例：OR = 1.08 → BMIが1 kg/m²上がると糖尿病のオッズが8%増加
# OR > 1 → リスク増加
# OR < 1 → リスク減少（予防的）
# OR = 1 → 関連なし
# 95% CI が 1 をまたぐ → 統計的に有意でない


# --- 2-3. 複数の単変量モデルを一括実行（変数スクリーニング） ---
# 臨床研究ではまず各変数の粗オッズ比を確認してから多変量に進むことが多い

univar_vars <- c("Age", "Gender", "BMI", "Race1", "Smoking_status",
                 "PhysActive", "BPSysAve", "TotChol", "Poverty")

univar_results <- data.frame()
for (var in univar_vars) {
  formula <- as.formula(paste("DM ~", var))
  fit <- glm(formula, data = df, family = binomial)
  or <- exp(coef(fit))[-1]       # 切片を除く
  ci <- exp(confint(fit))[-1, , drop = FALSE]
  pv <- summary(fit)$coefficients[-1, "Pr(>|z|)", drop = FALSE]

  tmp <- data.frame(
    Variable = names(or),
    OR       = round(or, 3),
    Lower    = round(ci[, 1], 3),
    Upper    = round(ci[, 2], 3),
    p_value  = round(as.numeric(pv), 4)
  )
  univar_results <- rbind(univar_results, tmp)
}
rownames(univar_results) <- NULL
cat("\n--- 単変量解析の結果一覧（粗オッズ比）---\n")
print(univar_results)


# --- 2-4. 多変量ロジスティック回帰 ---
model_logit2 <- glm(DM ~ Age + Gender + BMI + Race1 + Smoking_status +
                      PhysActive + BPSysAve + Poverty,
                    data = df, family = binomial)
summary(model_logit2)

# オッズ比と信頼区間の一覧表
OR_table <- data.frame(
  OR    = exp(coef(model_logit2)),
  Lower = exp(confint(model_logit2))[, 1],
  Upper = exp(confint(model_logit2))[, 2],
  p_value = summary(model_logit2)$coefficients[, "Pr(>|z|)"]
)
OR_table <- round(OR_table, 3)

cat("\n--- 多変量ロジスティック回帰：調整オッズ比一覧 ---\n")
print(OR_table)

# 【係数の解釈例】
cat("\n解釈例：\n")
cat("Age の OR:", OR_table["Age", "OR"],
    "\n  → 他の変数を調整した上で、年齢1歳増加あたり\n",
    "    糖尿病のオッズが", round((OR_table["Age", "OR"]-1)*100, 1), "%増加\n")
cat("BMI の OR:", OR_table["BMI", "OR"],
    "\n  → BMIが1 kg/m²増加するごとに\n",
    "    糖尿病のオッズが", round((OR_table["BMI", "OR"]-1)*100, 1), "%増加\n")


# =============================================================================
# 3. オッズ比のフォレストプロット（可視化）
# =============================================================================

# 切片を除いたオッズ比テーブル
OR_plot_data <- OR_table[-1, ]  # 切片を除く
OR_plot_data$Variable <- rownames(OR_plot_data)

ggplot(OR_plot_data, aes(x = OR, y = reorder(Variable, OR))) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "多変量ロジスティック回帰：調整オッズ比（NHANES）",
       subtitle = "アウトカム：糖尿病",
       x = "オッズ比 (95% CI)", y = "") +
  theme_minimal() +
  scale_x_log10()  # 対数スケール（ORは乗法的なので対数が適切）


# =============================================================================
# 4. モデルの評価
# =============================================================================

# --- 4-1. モデルの当てはまり（Hosmer-Lemeshow検定） ---
# install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(model_logit2$y, fitted(model_logit2), g = 10)
# p > 0.05 → モデルの当てはまりが良い（帰無仮説=良いフィット）

# --- 4-2. 予測確率の計算 ---
df$predicted_prob <- predict(model_logit2, newdata = df, type = "response")

# 予測確率の分布（糖尿病あり/なしで比較）
ggplot(df %>% filter(!is.na(predicted_prob) & !is.na(Diabetes)),
       aes(x = predicted_prob, fill = Diabetes)) +
  geom_histogram(binwidth = 0.02, alpha = 0.7, position = "identity") +
  labs(title = "予測確率の分布（糖尿病有無別, NHANES）",
       x = "予測確率", y = "度数", fill = "糖尿病") +
  theme_minimal()

# --- 4-3. ROC曲線とAUC ---
# install.packages("pROC")
library(pROC)

# 欠損を除いたデータで計算
df_roc <- df %>% filter(!is.na(predicted_prob) & !is.na(DM))
roc_result <- roc(df_roc$DM, df_roc$predicted_prob)
auc_value  <- auc(roc_result)

plot(roc_result,
     main = paste("ROC曲線 (AUC =", round(auc_value, 3), ")"),
     print.auc = TRUE)

cat("\nAUC:", round(auc_value, 3), "\n")
# AUC の目安：
# 0.5      : ランダム（予測力なし）
# 0.5-0.7  : 低い予測力
# 0.7-0.8  : まずまずの予測力
# 0.8-0.9  : 良い予測力
# 0.9-1.0  : 非常に良い予測力

# --- 4-4. モデル比較（尤度比検定） ---
model_logit_null <- glm(DM ~ 1, data = df, family = binomial)
anova(model_logit_null, model_logit2, test = "LRT")
# p < 0.05 → 説明変数のあるモデルの方が有意に良い

cat("\nAIC比較:\n")
cat("帰無モデル:", round(AIC(model_logit_null)), "\n")
cat("単変量(BMI):", round(AIC(model_logit1)), "\n")
cat("多変量:", round(AIC(model_logit2)), "\n")


# =============================================================================
# 5. 重回帰との比較（フェーズ4の補強）
# =============================================================================
# 【解説】
# ロジスティック回帰と重回帰の違いを理解するために、
# 同じ説明変数で異なるアウトカムを分析

# 連続アウトカム（血圧）の重回帰
model_lm <- lm(BPSysAve ~ Age + Gender + BMI + Race1 + Smoking_status,
               data = df)

# 二値アウトカム（糖尿病）のロジスティック回帰
model_glm <- glm(DM ~ Age + Gender + BMI + Race1 + Smoking_status,
                  data = df, family = binomial)

cat("\n===== 重回帰 vs ロジスティック回帰の比較 =====\n\n")

cat("[重回帰分析] アウトカム：収縮期血圧（連続変数）\n")
cat("→ 係数は「他の変数を一定にしたとき、Xが1単位増えるとYがβだけ変化」\n")
cat("→ 例：Age=0.5 なら「1歳増で血圧0.5mmHg上昇」\n\n")
print(round(summary(model_lm)$coefficients[, c(1, 2, 4)], 3))

cat("\n[ロジスティック回帰] アウトカム：糖尿病（二値 0/1）\n")
cat("→ 係数はログオッズ → exp() でオッズ比に変換して解釈\n")
cat("→ 例：BMI の OR=1.08 なら「BMI 1増で糖尿病のオッズ8%増」\n\n")
or_compare <- data.frame(
  log_OR = round(coef(model_glm), 3),
  OR     = round(exp(coef(model_glm)), 3)
)
print(or_compare)


# =============================================================================
# まとめ
# =============================================================================
cat("\n")
cat("=============================================================\n")
cat("  手法の選択ガイド（本セミナーのまとめ）\n")
cat("=============================================================\n")
cat("  アウトカムの型           →  分析手法\n")
cat("-------------------------------------------------------------\n")
cat("  連続変数（血圧、BMI等）  →  重回帰分析（lm）\n")
cat("  二値（糖尿病 Yes/No等）  →  ロジスティック回帰（glm）\n")
cat("  生存時間（発症までの日数）→  Cox比例ハザード回帰（発展編）\n")
cat("  カウントデータ（入院回数）→  ポアソン回帰（発展編）\n")
cat("=============================================================\n")
cat("  検定の選択ガイド\n")
cat("=============================================================\n")
cat("  比較の種類               →  検定手法\n")
cat("-------------------------------------------------------------\n")
cat("  カテゴリ × カテゴリ      →  カイ二乗検定 / Fisher正確検定\n")
cat("  カテゴリ × 連続（2群）   →  t検定 / Wilcoxon検定\n")
cat("  カテゴリ × 連続（3群+）  →  ANOVA / Kruskal-Wallis検定\n")
cat("  連続 × 連続              →  相関係数 / 回帰分析\n")
cat("=============================================================\n")

# オッズ比テーブルをCSVとして保存
write.csv(OR_table, "OR_table_logistic_NHANES.csv")

cat("\n===== フェーズ5 完了：全セミナー完了 =====\n")
cat("オッズ比テーブルを 'OR_table_logistic_NHANES.csv' として保存しました。\n")


# =============================================================================
# ★ 復習問題
# =============================================================================
#
# --- 基本問題（必須） ---
#
# 【問1】オッズ比の解釈
#   以下の結果が得られたとします。それぞれ日本語で解釈してください。
#   (a) 喫煙の OR = 2.5, 95%CI: 1.8-3.5
#   (b) 運動習慣の OR = 0.6, 95%CI: 0.4-0.9
#   (c) 性別(male)の OR = 1.1, 95%CI: 0.7-1.6
#   ※ それぞれ「統計的に有意かどうか」も判断してください。
#
# 【問2】ロジスティック回帰の実践
#   高血圧（Hypertension、フェーズ2で作成した変数）をアウトカムとする
#   ロジスティック回帰モデルを作成してください。
#   説明変数は Age, Gender, BMI, Diabetes とします。
#   (a) 各変数のオッズ比と95%信頼区間を算出してください。
#   (b) 統計的に有意な変数はどれですか？
#
#
# --- ボーナス問題（加点対象） ---
#
# 【ボーナス1】
#   問2のモデルについて ROC曲線を描き、AUCを算出してください。
#   AUCの値からモデルの予測力をどう評価しますか？
#   さらに、フォレストプロットを作成し、結果を視覚的に提示してください。
#
# 【ボーナス2】総合問題（セミナー全体のまとめ）
#   「喫煙は糖尿病のリスクを高めるか？」という研究課題について：
#   (a) 適切な分析手法を選択し、その理由を述べてください。
#   (b) 調整すべき交絡因子を3つ以上挙げ、それぞれの理由を説明してください。
#   (c) 実際にRコードを書いて分析を実行し、結果を解釈してください。
