# Databricks notebook source
1+1

# COMMAND ----------

# Databricks notebook source
# ==============================================================================
# Prepared masterdata -> category/segment -> IPW-DID -> market metrics -> Excel
# ==============================================================================

# ★ このコード一式を置いたWorkspaceフォルダに合わせて変更してください。
PIPELINE_CODE_DIR <- "/Workspace/Users/ryo_takahashi@macromill.com/Netflix様_セールスリフト/時点考慮版_パイプライン"


# COMMAND ----------

source(file.path(PIPELINE_CODE_DIR, "00_data_settings.R"), encoding = "UTF-8")

ctx <- create_run_context(PIPELINE_CONFIG)

load_pipeline_packages()

source(file.path(PIPELINE_CODE_DIR, "01_data_processing.R"), encoding = "UTF-8")
source(file.path(PIPELINE_CODE_DIR, "02_calculation.R"), encoding = "UTF-8")
source(file.path(PIPELINE_CODE_DIR, "03_formatting.R"), encoding = "UTF-8")

# masterdataは前処理済みなので、01ではファイル存在確認だけを行います。
processing_results <- run_data_processing(
  config = PIPELINE_CONFIG,
  ctx = ctx
)

# category / segment作成後、元パイプラインと同じIPW-DIDを実施します。
calculation_results <- run_calculation(
  config = PIPELINE_CONFIG,
  ctx = ctx,
  processing_results = processing_results
)

excel_path <- run_formatting(
  config = PIPELINE_CONFIG,
  ctx = ctx,
  calculation_results = calculation_results
)

cat("\n==============================\n")
cat("IPW-DID Pipeline 完了\n")
cat("run_dir          : ", ctx$run_dir, "\n", sep = "")
cat("excel_path       : ", excel_path, "\n", sep = "")
cat("market_metrics   : skipped\n")

# COMMAND ----------
