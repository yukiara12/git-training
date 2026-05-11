###############################################################################
#  フェーズ1：データの読み込み・ビジュアライゼーション
#  目的：データ分析の準備と概要把握
#  トピック：Rのコードで何をやっているかを説明できること
#
#  使用データ：NHANES パッケージ（米国国民健康栄養調査 2009-2012）
###############################################################################

# =============================================================================
# ★ NHANESデータセットについて
# =============================================================================
# NHANES（National Health and Nutrition Examination Survey）は
# 米国CDC（疾病予防管理センター）が実施する全国規模の横断調査です。
# 健康状態・栄養・生活習慣に関する情報を、面接・身体測定・血液検査で収集しています。
# Rの NHANES パッケージには 2009-2012年のデータが含まれており、
# 教育目的で10,000人に再サンプリングされています。
#
# 本セミナーで使用する主な変数：
# ─────────────────────────────────────────────────────────────────
# 【人口統計学的変数（Demographics）】
#   Gender       : 性別（male / female）
#   Age          : 年齢（歳、0〜80歳）
#   Race1        : 人種（Black, Hispanic, Mexican, White, Other）
#   Education    : 教育歴（20歳以上で記録）
#   MaritalStatus: 婚姻状況（20歳以上で記録）
#   HHIncome     : 世帯年収カテゴリ
#   Poverty      : 貧困指数（連邦貧困線に対する所得比率、1未満=貧困）
#
# 【身体測定（Examination）】
#   Weight       : 体重（kg）
#   Height       : 身長（cm）
#   BMI          : 体格指数（kg/m²） = 体重 / 身長(m)²
#   Pulse        : 脈拍数（bpm）
#   BPSysAve     : 収縮期血圧（mmHg、3回測定の平均）
#   BPDiaAve     : 拡張期血圧（mmHg、3回測定の平均）
#
# 【血液検査（Laboratory）】
#   DirectChol   : HDLコレステロール（mmol/L）
#   TotChol      : 総コレステロール（mmol/L）
#
# 【質問票（Questionnaire）】
#   Diabetes     : 糖尿病の有無（Yes / No）
#   HealthGen    : 主観的健康感（Excellent〜Poor の5段階）
#   SmokeNow     : 現在喫煙の有無（Yes / No）
#   Smoke100     : 生涯100本以上の喫煙歴（Yes / No）
#   PhysActive   : 中等度以上の運動習慣（Yes / No）
#   SleepHrsNight: 平均睡眠時間（時間/夜）
#   SleepTrouble : 睡眠障害の有無（Yes / No）
#   Depressed    : 抑うつ（None, Several, Most）
#   AlcoholYear  : 年間飲酒日数
# ─────────────────────────────────────────────────────────────────


# =============================================================================
# 1. ライブラリのインストールと読み込み
# =============================================================================
# 【解説】
# Rには標準で入っている関数（base R）と、追加でインストールが必要な
# パッケージ（ライブラリ）があります。
# install.packages() は最初の1回だけ実行すれば十分です。
# library() はRを起動するたびに実行する必要があります。

# --- パッケージのインストール（初回のみ） ---
# install.packages("tidyverse")   # データ操作・可視化の統合パッケージ

# --- パッケージの読み込み（毎回必要） ---
library(tidyverse)  # ggplot2, dplyr, tidyr などを一括読み込み

# --- 文字化け対策
# --- Googleフォントから日本語フォントを追加
install.packages("showtext")
library(showtext)
font_add_google("Noto Sans JP", "noto")
showtext_auto()  # 自動的にshowtextを有効化

# 【ポイント】
# tidyverse は以下のパッケージを含みます：
#   - ggplot2 : グラフ作成
#   - dplyr   : データ操作（filter, mutate, select, summarise など）
#   - tidyr   : データ整形（pivot_longer, pivot_wider など）
#   - readr   : CSV読み込み（read_csv）


# =============================================================================
# 2. CSVファイルの読み込み
# =============================================================================
# 【解説】
# 配布された nhanes_adult.csv を読み込みます。
# read.csv() は R に最初から入っている関数で、CSVファイルを読み込めます。

# --- データの読み込み ---
# ★ nhanes_adult.csv を作業ディレクトリに置いてから実行してください
# 作業ディレクトリの確認：getwd()
# 作業ディレクトリの変更：setwd("C:/Users/yourname/Documents")

df <- read.csv("nhanes_adult.csv", stringsAsFactors = TRUE)
# stringsAsFactors = TRUE : 文字列をfactor（カテゴリ変数）として読み込む

# 【参考：他の読み込み方法】
# 方法2: read.table() で区切り文字を指定
# df <- read.table("nhanes_adult.csv", header = TRUE, sep = ",")
#   header = TRUE : 1行目を列名として扱う
#   sep = ","     : カンマ区切り

# 方法3: tidyverse の read_csv()（推奨）
# df <- read_csv("nhanes_adult.csv")

# 【注意】
# Windowsのパスは \ ではなく / を使う（例: "C:/Users/data.csv"）

# --- 読み込んだデータの確認 ---
# データの最初の6行を表示
head(df)

# データの構造を確認（列名、型、最初の数値）
str(df)

# データの次元（行数 × 列数）
dim(df)

# 列名の一覧
names(df)

cat("サンプルサイズ:", nrow(df), "人\n")
cat("変数の数:", ncol(df), "\n")

# 【確認ポイント】
# str() の出力で、各変数が正しい型になっているか確認しましょう：
#   - Factor（カテゴリ変数）: Gender, Race1, Education, Diabetes 等
#   - num / int（数値）: Age, BMI, BPSysAve 等


# =============================================================================
# 3. summary() 関数の使い方
# =============================================================================
# 【解説】
# summary() はデータの要約統計量を一括表示する便利な関数です。
# 連続変数 → 最小値, 第1四分位, 中央値, 平均, 第3四分位, 最大値, NA数
# カテゴリ変数（factor） → 各カテゴリの度数

# --- データ全体の要約 ---
summary(df)
# ★ NA's の数に注目！→ 実データには欠損値が必ずある

# --- 個別の変数の要約 ---
summary(df$Age)         # 年齢の要約統計量
summary(df$BMI)         # BMIの要約統計量（NAあり）
summary(df$Gender)      # 性別の度数

# --- 特定の統計量を個別に求める ---
mean(df$Age)                                # 平均
sd(df$Age)                                  # 標準偏差
median(df$Age)                              # 中央値
quantile(df$Age, c(0.25, 0.75))             # 四分位数
mean(df$BMI, na.rm = TRUE)                  # NAを除外して平均
# na.rm = TRUE：NAを無視して計算（忘れるとNAが返る！）

table(df$Gender)                            # 度数分布表
prop.table(table(df$Gender))                # 割合


# =============================================================================
# 4. ヒストグラム、箱ひげ図、散布図
# =============================================================================

# --- 4-1. ヒストグラム（Histogram） ---
# 【目的】1つの連続変数の分布（形状）を確認する。
# 正規分布に近いか、歪みがあるか、外れ値があるかなどを視覚的に判断する。

# base R のヒストグラム
hist(df$BMI,
     main = "BMIの分布（NHANES 成人）",
     xlab = "BMI (kg/m²)",
     ylab = "度数",
     col  = "lightblue",
     breaks = 30)

# ggplot2 のヒストグラム（より柔軟なカスタマイズが可能）
ggplot(df, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white",
                 na.rm = TRUE) +
  labs(title = "BMIの分布（NHANES 成人）",
       x = "BMI (kg/m²)", y = "度数") +
  theme_minimal()

# 【解釈のポイント】
# - 分布の中心（ピーク）はどこか？ → 米国成人のBMIは25-30付近が多い
# - 右に裾が長い（右に歪んだ分布）→ 高BMIの外れ値がある
# - 正規分布と比べてどうか？

# 年齢の分布
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "coral", color = "white") +
  labs(title = "年齢の分布（NHANES 成人）",
       x = "年齢 (歳)", y = "度数") +
  theme_minimal()


# --- 4-2. 箱ひげ図（Box Plot） ---
# 【目的】中央値、四分位数、外れ値を1つの図で表現する。
# グループ間の比較に特に有用。
#
#   ----[====|====]----  o
#   ^   ^    ^    ^  ^   ^
#   |   Q1  Med  Q3  |  外れ値
#   最小値（ヒゲ）   最大値（ヒゲ）

# 性別ごとのBMI分布
ggplot(df, aes(x = Gender, y = BMI, fill = Gender)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "性別ごとのBMI分布（NHANES）",
       x = "性別", y = "BMI (kg/m²)") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "lightpink", "male" = "lightblue"))

# 糖尿病の有無ごとの収縮期血圧
ggplot(df %>% filter(!is.na(Diabetes)),
       aes(x = Diabetes, y = BPSysAve, fill = Diabetes)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "糖尿病有無ごとの収縮期血圧（NHANES）",
       x = "糖尿病", y = "収縮期血圧 (mmHg)") +
  theme_minimal()

# 【解釈のポイント】
# - 箱の位置（中央値）が上下に離れている → グループ間で差がありそう
# - 箱の大きさ（IQR = Q3 - Q1）→ ばらつきの大きさ
# - 外れ値（○で表示される点）→ 異常値の有無


# --- 4-3. 散布図（Scatter Plot） ---
# 【目的】2つの連続変数の関連性を視覚的に確認する。

# 年齢と収縮期血圧の関連
ggplot(df, aes(x = Age, y = BPSysAve)) +
  geom_point(alpha = 0.3, color = "darkblue", na.rm = TRUE) +
  geom_smooth(method = "lm", color = "red", se = TRUE, na.rm = TRUE) +
  labs(title = "年齢と収縮期血圧の関連（NHANES）",
       x = "年齢 (歳)", y = "収縮期血圧 (mmHg)") +
  theme_minimal()

# BMIと収縮期血圧の関連（性別で色分け）
ggplot(df, aes(x = BMI, y = BPSysAve, color = Gender)) +
  geom_point(alpha = 0.3, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(title = "BMIと収縮期血圧の関連（性別別）",
       x = "BMI (kg/m²)", y = "収縮期血圧 (mmHg)", color = "性別") +
  theme_minimal()

# 【解釈のポイント】
# - 点が右上がりに並ぶ → 正の相関（年齢が高い→血圧も高い）
# - 点が右下がりに並ぶ → 負の相関
# - 点がランダムに散らばる → 相関なし

# --- 相関係数の計算 ---
cor(df$Age, df$BPSysAve, use = "complete.obs")         # ピアソンの相関係数
cor.test(df$Age, df$BPSysAve, use = "complete.obs")     # 検定付き
cor(df$BMI, df$BPSysAve, use = "complete.obs")
# use = "complete.obs" : 両方の変数がNAでないケースのみ使用


cat("\n===== フェーズ1 完了 =====\n")


# =============================================================================
# ★ 復習問題
# =============================================================================
#
# --- 基本問題（必須） ---
#
# 【問1】データの基本確認と記述統計
#   (a) このデータセットには何人分のデータが含まれていますか？（ヒント：nrow()）
#   (b) 変数 BPSysAve（収縮期血圧）には欠損値が何件ありますか？（ヒント：sum(is.na(...))）
#   (c) 全体の BMI の平均値と標準偏差を求めてください（NAに注意）。
#
# 【問2】可視化
#   (a) 収縮期血圧（BPSysAve）のヒストグラムを ggplot2 で作成してください
#       （binwidth = 5）。分布の形状にはどのような特徴がありますか？
#   (b) 人種（Race1）ごとの BMI の箱ひげ図を ggplot2 で作成してください。
#       人種間で分布に違いはありそうですか？
#
#
# --- ボーナス問題（加点対象） ---
#
# 【ボーナス1】
#   BMI と拡張期血圧（BPDiaAve）の散布図を作成し、相関係数を算出してください。
#   さらに、性別（Gender）で色分けした散布図も作成し、
#   男女で回帰直線の傾きに違いがあるか視覚的に評価してください。
#
# 【ボーナス2】
#   ggplot2 の facet_wrap() を使い、人種（Race1）ごとに
#   年齢（Age）と収縮期血圧（BPSysAve）の散布図を5枚のパネルに分けて
#   表示してください。人種ごとに年齢と血圧の関連に違いはありますか？
