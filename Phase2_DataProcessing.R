###############################################################################
#  フェーズ2：データ加工
#  目的：欠測値があるときの変数処理、新たな変数作成
#
#  使用データ：NHANES パッケージ（米国国民健康栄養調査 2009-2012）
###############################################################################

library(tidyverse)

# フェーズ1で作成した分析用データの読み込み
df <- read.csv("nhanes_adult.csv", stringsAsFactors = TRUE)
str(df)


# =============================================================================
# 1. 欠測値（NA）の確認と処理
# =============================================================================
# 【解説】
# 実際の調査データには欠測値（NA: Not Available）がほぼ必ず存在します。
# NHANESも例外ではなく、多くの変数にNAがあります。
# 欠測値の処理は分析結果に大きく影響するため、慎重に行う必要があります。

# --- 1-1. 欠測値の有無を確認する ---

# 各変数のNA数を確認
na_counts <- colSums(is.na(df))
na_counts

# NAの割合（%）
na_pct <- round(colSums(is.na(df)) / nrow(df) * 100, 1)
data.frame(NA数 = na_counts, NA割合 = na_pct)

# 【ポイント】
# NHANESでは以下のような理由でNAが発生：
# - BPSysAve : 8歳未満は測定しない → 成人限定なら少ない
# - SmokeNow : 20歳以上かつSmoke100=Yesの人のみ回答
# - Education: 20歳以上で記録
# → 変数ごとに欠測の理由が異なる！

# summary() でもNA数がわかる
summary(df$BPSysAve)
summary(df$SmokeNow)

# 完全データ（すべての変数が揃っている行）の数
sum(complete.cases(df))
cat("完全データの行数:", sum(complete.cases(df)), "/", nrow(df), "\n")
cat("完全データの割合:", round(sum(complete.cases(df))/nrow(df)*100, 1), "%\n")


# --- 1-2. 欠測値の処理方法 ---

# 【方法A】リストワイズ削除（Complete Case Analysis）
# → 1つでもNAがある行をすべて削除
# メリット：実装が簡単
# デメリット：サンプルサイズが大幅に減る、バイアスの可能性
df_complete <- na.omit(df)
cat("リストワイズ削除後:", nrow(df_complete), "行（元:", nrow(df), "行）\n")

# 【方法B】分析に使う変数の欠測のみ削除（推奨）
# → 必要な変数にNAがある行だけ削除
# 例：BMIと血圧の関連を見るなら、この2変数のNAだけ削除
df_bp_bmi <- df %>% filter(!is.na(BMI) & !is.na(BPSysAve))
cat("BMI・血圧欠損のみ削除:", nrow(df_bp_bmi), "行\n")

# 【方法C】平均値・中央値で補完（単純補完）
# → 欠測値を平均値や中央値で置き換える
# 注意：ばらつきを過小評価するリスクあり。探索的分析では使われる。
df$BMI_imputed <- ifelse(is.na(df$BMI),
                          median(df$BMI, na.rm = TRUE),
                          df$BMI)
cat("BMI補完前のNA:", sum(is.na(df$BMI)), "\n")
cat("BMI補完後のNA:", sum(is.na(df$BMI_imputed)), "\n")

# 【方法D】カテゴリ変数の欠測を「Unknown」カテゴリにする
df$SmokeNow_filled <- as.character(df$SmokeNow)
df$SmokeNow_filled[is.na(df$SmokeNow_filled)] <- "Unknown"
df$SmokeNow_filled <- factor(df$SmokeNow_filled,
                              levels = c("No", "Yes", "Unknown"))
table(df$SmokeNow_filled, useNA = "always")

# 【ポイント】
# 臨床研究では欠測のメカニズムを考慮することが重要：
#   MCAR（完全ランダム欠損）：欠損がどの変数とも無関係
#   MAR（ランダム欠損）：観測データから欠損パターンを説明できる
#   MNAR（非ランダム欠損）：欠損値自体が欠損の原因に関連
# 多重補完法（Multiple Imputation）が推奨される場面も多い


# =============================================================================
# 2. 新しい変数の作成
# =============================================================================

# --- 2-1. 連続変数のカテゴリ化 ---
# 【解説】
# 臨床・公衆衛生研究では連続変数をカテゴリに分けることがあります。

# BMIカテゴリ（WHO基準）
df$BMI_cat <- cut(df$BMI,
                  breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
                  labels = c("低体重(<18.5)",
                             "普通(18.5-24.9)",
                             "過体重(25-29.9)",
                             "肥満I(30-34.9)",
                             "肥満II(35-39.9)",
                             "肥満III(40+)"),
                  right = FALSE)
table(df$BMI_cat, useNA = "always")

# 年齢カテゴリ
df$Age_group <- cut(df$Age,
                    breaks = c(20, 40, 60, 80, Inf),
                    labels = c("20-39", "40-59", "60-79", "80+"),
                    right = FALSE)
table(df$Age_group)

# 血圧カテゴリ（2017 ACC/AHA ガイドライン）
df$BP_cat <- cut(df$BPSysAve,
                 breaks = c(-Inf, 120, 130, 140, Inf),
                 labels = c("正常(<120)",
                            "正常高値(120-129)",
                            "高血圧I(130-139)",
                            "高血圧II(140+)"),
                 right = FALSE)
table(df$BP_cat, useNA = "always")


# --- 2-2. 二値変数（ダミー変数）の作成 ---
# 0/1 の二値変数を作成する

# 高血圧フラグ（収縮期 ≥ 140）
df$Hypertension <- ifelse(df$BPSysAve >= 140, 1, 0)
table(df$Hypertension, useNA = "always")

# 糖尿病を0/1に変換
df$DM <- ifelse(df$Diabetes == "Yes", 1, 0)
table(df$DM, useNA = "always")

# 肥満フラグ（BMI ≥ 30）
df$Obese <- ifelse(df$BMI >= 30, 1, 0)
table(df$Obese, useNA = "always")

# 喫煙歴（Smoke100 = 生涯100本以上）
df$EverSmoked <- ifelse(df$Smoke100 == "Yes", 1, 0)
table(df$EverSmoked, useNA = "always")


# --- 2-3. dplyr の mutate() を使った変数作成（推奨） ---
# 【解説】
# tidyverse の dplyr パッケージを使うと、パイプ演算子（%>%）で
# 読みやすいコードが書けます。

df <- df %>%
  mutate(
    # BMIの標準化（Zスコア：平均=0、SD=1 に変換）
    BMI_z = as.numeric(scale(BMI_imputed)),

    # 身長をメートルに変換
    Height_m = Height / 100,

    # コレステロール比（総コレステロール / HDL）
    Chol_ratio = TotChol / DirectChol
  )


# --- 2-4. case_when() を使った複雑な条件分岐 ---
# 【解説】
# 3つ以上の条件がある場合、case_when() が ifelse() のネストより読みやすい

df <- df %>%
  mutate(
    # 喫煙状態の統合変数
    Smoking_status = case_when(
      Smoke100 == "No"                    ~ "非喫煙",
      Smoke100 == "Yes" & SmokeNow == "No"  ~ "過去喫煙",
      Smoke100 == "Yes" & SmokeNow == "Yes" ~ "現在喫煙",
      TRUE                                 ~ "不明"
    ),
    # 心血管リスクの簡易分類
    CV_risk = case_when(
      Age >= 65 & DM == 1 & Hypertension == 1 ~ "高リスク",
      Age >= 50 | DM == 1 | Hypertension == 1 ~ "中リスク",
      TRUE                                     ~ "低リスク"
    )
  )

table(df$Smoking_status)
table(df$CV_risk)


# =============================================================================
# 3. データの選択・フィルタリング
# =============================================================================

# --- 3-1. 列（変数）の選択 ---
# select() で必要な列だけ抽出
df_selected <- df %>%
  select(Gender, Age, Race1, BMI, BPSysAve, Diabetes, Smoking_status)
head(df_selected)

# 列名のパターンで選択
df %>% select(starts_with("BP"))      # "BP" で始まる列
df %>% select(contains("Chol"))       # "Chol" を含む列

# --- 3-2. 行（観測値）のフィルタリング ---
# filter() で条件に合う行を抽出
male_data    <- df %>% filter(Gender == "male")
elderly_data <- df %>% filter(Age >= 65)
diabetic     <- df %>% filter(Diabetes == "Yes")

cat("男性のみ:", nrow(male_data), "人\n")
cat("65歳以上:", nrow(elderly_data), "人\n")
cat("糖尿病あり:", nrow(diabetic), "人\n")


# =============================================================================
# 4. データの集計
# =============================================================================

# --- group_by() + summarise() で群別集計 ---
df %>%
  group_by(Gender) %>%
  summarise(
    n          = n(),
    mean_age   = mean(Age, na.rm = TRUE),
    sd_age     = sd(Age, na.rm = TRUE),
    mean_BMI   = mean(BMI, na.rm = TRUE),
    mean_SBP   = mean(BPSysAve, na.rm = TRUE),
    prop_DM    = mean(DM, na.rm = TRUE)   # 糖尿病の割合
  )

# --- 複数グループでのクロス集計 ---
df %>%
  group_by(Gender, Age_group) %>%
  summarise(
    n        = n(),
    mean_SBP = mean(BPSysAve, na.rm = TRUE),
    sd_SBP   = sd(BPSysAve, na.rm = TRUE),
    .groups  = "drop"
  )


# =============================================================================
# 5. 加工後のデータを保存
# =============================================================================
write.csv(df, "nhanes_adult_processed.csv", row.names = FALSE)

cat("\n===== フェーズ2 完了 =====\n")
cat("加工済みデータを 'nhanes_adult_processed.csv' として保存しました。\n")


# =============================================================================
# ★ 復習問題
# =============================================================================
#
# --- 基本問題（必須） ---
#
# 【問1】欠測値の確認とカテゴリ変数の作成
#   (a) 各変数の欠測数を colSums(is.na(...)) で確認し、
#       欠測が最も多い変数はどれか答えてください。
#   (b) 収縮期血圧（BPSysAve）を以下の基準で3カテゴリに分けてください。
#         120未満: "正常" / 120以上140未満: "境界域" / 140以上: "高血圧"
#       ヒント：cut() 関数を使う。table() で各カテゴリの人数を確認すること。
#
# 【問2】二値変数の作成と集計
#   「肥満」変数を作成してください（BMI >= 30 なら 1、それ以外は 0）。
#   男女別の肥満の割合を group_by() + summarise() で求めてください。
#
#
# --- ボーナス問題（加点対象） ---
#
# 【ボーナス1】
#   case_when() を使い、以下の条件で Sleep_quality 変数を作成してください。
#     SleepHrsNight < 6 → "短時間" / 6〜8 → "適正" / > 8 → "長時間"
#   さらに、Sleep_quality と Diabetes のクロス集計を行い、
#   睡眠時間のカテゴリによって糖尿病の割合に違いがあるか考察してください。
#
# 【ボーナス2】
#   人種（Race1）・性別（Gender）・年齢層（Age_group）の3変数で
#   group_by() して、各グループの平均BMIと平均血圧を一覧表にしてください。
#   サンプルサイズが極端に少ないグループはありますか？
#   その場合、統計的な信頼性にどのような影響がありますか？
