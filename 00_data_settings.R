# Databricks notebook source
# ==============================================================================
# Cross-media IPW-DID Pipeline settings
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. パッケージ
# ------------------------------------------------------------------------------

install_missing_packages <- function(pkgs) {
  installed_pkgs <- rownames(installed.packages())
  missing_pkgs <- setdiff(pkgs, installed_pkgs)

  if (length(missing_pkgs) == 0) {
    cat("不足パッケージはありません。\n")
    return(invisible(TRUE))
  }

  cat("不足パッケージをインストールします:\n")
  print(missing_pkgs)

  install.packages(
    missing_pkgs,
    repos = "https://cloud.r-project.org",
    dependencies = c("Depends", "Imports", "LinkingTo"),
    Ncpus = max(1, parallel::detectCores() - 1)
  )

  invisible(TRUE)
}

load_pipeline_packages <- function() {
  pkgs <- c(
    "data.table",
    "dplyr",
    "tidyr",
    "readr",
    "readxl",
    "purrr",
    "stringr",
    "tibble",
    "WeightIt",
    "DRDID",
    "openxlsx"
  )

  install_missing_packages(pkgs)

  for (pkg in pkgs) {
    cat("library読み込み: ", pkg, "\n", sep = "")
    library(pkg, character.only = TRUE)
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# 1. 設定作成用関数
# ------------------------------------------------------------------------------

make_kpi <- function(
  kpi_id,
  kpi_label,
  pre_col,
  post_col,
  file_label,
  multiplier = NULL
) {
  list(
    kpi_id = kpi_id,
    kpi_label = kpi_label,
    pre_col = pre_col,
    post_col = post_col,
    file_label = file_label,
    multiplier = multiplier
  )
}

make_behavior_category_spec <- function(
  method,
  value_col,
  segment_col,
  axis_label,
  category_labels,
  tie_breaker_col = NULL
) {
  list(
    method = method,
    value_col = value_col,
    category_col = segment_col,
    axis_label = axis_label,
    category_labels = category_labels,
    tie_breaker_col = tie_breaker_col
  )
}

make_embedded_contact <- function(
  sample_id_col,
  study_col = "media_contact_flg",
  fq_col = NULL,
  exposure_value = 1,
  control_value = 2
) {
  list(
    type = "embedded",
    sample_id_col = sample_id_col,
    study_col = study_col,
    fq_col = fq_col,
    exposure_value = exposure_value,
    control_value = control_value
  )
}

make_crossmedia_project <- function(
  dataset_id,
  dataset_name,
  masterdata_filename,
  data_type,
  analysis_id_col,
  media,
  own_media_flag,
  all_media_flags,
  behavior_value_col,
  behavior_category_method,
  behavior_category_labels,
  kpis,
  rename_map = list()
) {
  project_id <- paste0(dataset_id, "_", tolower(media))
  project_name <- paste0(dataset_name, "_", media)
  segment_col <- paste0(behavior_value_col, "_segment")

  list(
    project_id = project_id,
    project_name = project_name,
    dataset_name = dataset_name,
    media = media,
    output_filename_prefix = project_name,
    data_type = data_type,
    input_mode = "MASTERDATA",
    masterdata_path = file.path(PIPELINE_CODE_DIR, "data", masterdata_filename),
    analysis_id_col = analysis_id_col,
    own_media_flag = own_media_flag,
    other_media_covariates = setdiff(all_media_flags, own_media_flag),

    masterdata = list(
      rename_map = rename_map,
      drop_cols = c("...1", "Unnamed: 0"),
      media_col = "media",
      media_value = media
    ),

    contact = make_embedded_contact(
      sample_id_col = analysis_id_col,
      study_col = "media_contact_flg",
      exposure_value = 1,
      control_value = 2
    ),

    category_specs = list(
      make_behavior_category_spec(
        method = behavior_category_method,
        value_col = behavior_value_col,
        segment_col = segment_col,
        axis_label = segment_col,
        category_labels = behavior_category_labels,
        tie_breaker_col = analysis_id_col
      )
    ),

    covariates = c(
      segment_col,
      DEFAULT_COVARIATES,
      setdiff(all_media_flags, own_media_flag)
    ),

    # 今回は媒体別の全体リフトのみを推計する。
    segments = character(0),
    kpis = kpis,
    weight_col = "component_weight",
    focal = 1,
    drop_missing_covariates = TRUE
  )
}

# ------------------------------------------------------------------------------
# 2. パス・共通設定
# ------------------------------------------------------------------------------

if (!exists("PIPELINE_CODE_DIR") || is.null(PIPELINE_CODE_DIR) || PIPELINE_CODE_DIR == "") {
  stop("PIPELINE_CODE_DIRを設定してから00_data_settings.Rを読み込んでください。")
}

# 必要に応じてこの1か所だけ変更する。
OUTPUT_ROOT <- "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/crossmedia"

DEFAULT_COVARIATES <- c(
  "SEX",
  "AGEID",
  "AREA",
  "MARRIED",
  "CHILD",
  "HINCOME",
  "PINCOME"
)

HAREKAZE_KPIS <- list(
  make_kpi(
    "purchase_flag",
    "購入率",
    "新プレ_購入フラグ",
    "新ポスト_購入フラグ",
    "購入率"
  ),
  make_kpi(
    "purchase_count",
    "平均購入回数",
    "新プレ_購入回数",
    "新ポスト_購入回数",
    "平均購入回数"
  ),
  make_kpi(
    "purchase_amount",
    "平均購入金額",
    "新プレ_購入金額",
    "新ポスト_購入金額",
    "平均購入金額"
  )
)

MERCARI_KPIS <- list(
  make_kpi(
    "usage_flag",
    "利用率",
    "新プレ_利用フラグ",
    "新ポスト_利用フラグ",
    "利用率"
  ),
  make_kpi(
    "usage_count",
    "平均利用回数",
    "新プレ_利用回数",
    "新ポスト_利用回数",
    "平均利用回数"
  ),
  make_kpi(
    "usage_time",
    "平均利用時間",
    "新プレ_利用時間",
    "新ポスト_利用時間",
    "平均利用時間"
  )
)

HAREKAZE_MEDIA_FLAGS <- c("flg_NF", "flg_Tver", "flg_Amazon")
MERCARI_MEDIA_FLAGS <- c("flg_NF", "flg_Tver", "flg_Amazon", "flg_TV")

MEDIA_FLAG_MAP <- c(
  "Netflix" = "flg_NF",
  "Tver" = "flg_Tver",
  "Amazon" = "flg_Amazon",
  "TV" = "flg_TV"
)

# ------------------------------------------------------------------------------
# 3. 晴れ風3媒体・メルカリ4媒体の案件設定
# ------------------------------------------------------------------------------

harekaze_projects <- lapply(c("Netflix", "Tver", "Amazon"), function(media_i) {
  make_crossmedia_project(
    dataset_id = "harekaze",
    dataset_name = "晴れ風",
    masterdata_filename = "晴れ風_crossmedia_masterdata.csv",
    data_type = "MHS",
    analysis_id_col = "モニタID",
    media = media_i,
    own_media_flag = unname(MEDIA_FLAG_MAP[[media_i]]),
    all_media_flags = HAREKAZE_MEDIA_FLAGS,
    behavior_value_col = "新プレ_購入回数",
    behavior_category_method = "zero_vs_positive",
    behavior_category_labels = c(
      "1" = "新プレ購入回数_Non",
      "2" = "新プレ購入回数_Any"
    ),
    kpis = HAREKAZE_KPIS,
    rename_map = list(
      "WB値" = "component_weight",
      "半年購買有無" = "半年購入有無"
    )
  )
})

mercari_projects <- lapply(c("Netflix", "Tver", "Amazon", "TV"), function(media_i) {
  make_crossmedia_project(
    dataset_id = "mercari",
    dataset_name = "メルカリ",
    masterdata_filename = "メルカリ_crossmedia_masterdata.csv",
    data_type = "A-CUBE",
    analysis_id_col = "mid",
    media = media_i,
    own_media_flag = unname(MEDIA_FLAG_MAP[[media_i]]),
    all_media_flags = MERCARI_MEDIA_FLAGS,
    behavior_value_col = "新プレ_利用時間",
    behavior_category_method = "zero_plus_tertile",
    behavior_category_labels = c(
      "1" = "新プレ利用時間_Non",
      "2" = "新プレ利用時間_Light",
      "3" = "新プレ利用時間_Middle",
      "4" = "新プレ利用時間_Heavy"
    ),
    kpis = MERCARI_KPIS
  )
})

PIPELINE_CONFIG <- list(
  output_root = OUTPUT_ROOT,
  timezone = "Asia/Tokyo",
  run_market_metrics = FALSE,
  projects = c(harekaze_projects, mercari_projects)
)

# ------------------------------------------------------------------------------
# 4. run_タイムスタンプ フォルダ作成
# ------------------------------------------------------------------------------

create_run_context <- function(config = PIPELINE_CONFIG) {
  timestamp <- format(
    as.POSIXct(Sys.time(), tz = config$timezone),
    "%Y%m%d_%H%M%S"
  )

  run_id <- paste0("run_", timestamp)
  run_dir <- file.path(config$output_root, run_id)

  dirs <- list(
    run_dir = run_dir,
    rds_dir = file.path(run_dir, "rds"),
    processed_dir = file.path(run_dir, "01_processed"),
    result_dir = file.path(run_dir, "02_results"),
    excel_dir = file.path(run_dir, "03_excel"),
    log_dir = file.path(run_dir, "logs")
  )

  for (d in unlist(dirs)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  ctx <- c(
    list(
      run_id = run_id,
      timestamp = timestamp,
      timezone = config$timezone
    ),
    dirs
  )

  saveRDS(config, file.path(ctx$rds_dir, "pipeline_config.rds"))
  saveRDS(ctx, file.path(ctx$rds_dir, "run_context.rds"))

  cat("runフォルダを作成しました:\n")
  cat(ctx$run_dir, "\n")

  return(ctx)
}
