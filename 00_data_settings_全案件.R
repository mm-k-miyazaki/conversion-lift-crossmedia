# Databricks notebook source
# ==============================================================================
# IPW-DID Pipeline for prepared masterdata
# 00_data_settings.R
# ------------------------------------------------------------------------------
# すでに購買指標・広告接触・属性が1行1IDで結合済みのmasterdataから開始します。
# 編集が必要なのは、原則として MASTERDATA_PATH / OUTPUT_ROOT / media_inputs です。
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

# 0をcategory=1、0以外を10分位でcategory=2～11とし、
# categoryを6段階のsegmentにまとめる設定です。
# IPW-DIDではsegment列を、元パイプラインの6段階カテゴリと同じ用途で使用します。
make_zero_plus_decile_category_segment_spec <- function(
  value_col,
  category_col,
  segment_col,
  axis_label,
  category_prefix
) {
  list(
    method = "zero_plus_decile_category_segment",
    value_col = value_col,
    raw_category_col = category_col,
    category_col = segment_col,
    axis_label = axis_label,
    category_labels = c(
      "1" = paste0(category_prefix, "_Non"),
      "2" = paste0(category_prefix, "_Light"),
      "3" = paste0(category_prefix, "_Semi-Light"),
      "4" = paste0(category_prefix, "_Middle"),
      "5" = paste0(category_prefix, "_Semi-Heavy"),
      "6" = paste0(category_prefix, "_Heavy")
    )
  )
}

make_media_inputs <- function(reach = NA_real_, impressions = NA_real_, cost = NA_real_) {
  list(
    reach = reach,
    impressions = impressions,
    cost = cost
  )
}

# masterdata内の接触関連列を指定します。外部ファイルは読みません。
make_embedded_contact <- function(
  sample_id_col = "SAMPLEID",
  study_col = "flg_StudyID",
  fq_col = "StudyID",
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

# ------------------------------------------------------------------------------
# 2. パス・共通設定
# ------------------------------------------------------------------------------

OUTPUT_ROOT <- "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮"

# ★ Databricks上でmasterdata.csvを置いた場所に合わせて変更してください。

DEFAULT_COVARIATES <- c(
  "SEX",
  "AGEID",
  "AREA",
  "MARRIED",
  "CHILD",
  "HINCOME",
  "PINCOME"
)

DEFAULT_SEGMENTS <- c(
  "SEX",
  "AGE_GROUP",
  "SEX_AGE_GROUP",
  "MF1_3",
  "FQ_CATEGORY"
)

# ------------------------------------------------------------------------------
# 3. 案件設定
# ------------------------------------------------------------------------------

PIPELINE_CONFIG <- list(
  output_root = OUTPUT_ROOT,
  timezone = "Asia/Tokyo",

  projects = list(

     # --------------------------------------------------------------------------
    # A-cube: メルカリ様
    # --------------------------------------------------------------------------
    list(
      project_id = "mercari",
      project_name = "メルカリ様",
      output_filename_prefix = "メルカリ様",
      data_type = "ACUBE",

      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/メルカリ/メルカリ_timeconsiderd_masterdata.csv",

       # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        # rename_map = list(
        #   "モニタID" = "mid",
        #   "WB値" = "component_weight",
        #   "半年購買有無" = "半年購入有無"
        # ),
        drop_cols = c("Unnamed: 0")
      ),


        # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_利用時間",
          category_col = "プレ_利用時間_category",
          segment_col = "プレ_利用時間_segment",
          axis_label = "プレ_利用時間_segment",
          category_prefix = "プレ利用時間"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_利用時間_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年利用有無",
        "プレ_利用時間_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "usage_flag",
          "利用率",
          "新プレ_利用フラグ",
          "新ポスト_利用フラグ",
          "利用フラグ"
        ),
        make_kpi(
          "usage_time",
          "利用時間",
          "新プレ_利用時間",
          "新ポスト_利用時間",
          "利用時間"
        ),
        make_kpi(
          "usage_count",
          "利用回数",
          "新プレ_利用回数",
          "新ポスト_利用回数",
          "利用回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 02_2_market_metrics.R 用です。数値がわかり次第、以下のように指定してください。
      # media_inputs = make_media_inputs(reach = 123456, impressions = 2345678, cost = 3456789)
      media_inputs = make_media_inputs(reach = 3330807, impressions = 12960047, cost = 19818507)
    ),

    # --------------------------------------------------------------------------
    # A-cube: Piccoma様
    # --------------------------------------------------------------------------
    list(
      project_id = "Piccoma",
      project_name = "Piccoma様",
      output_filename_prefix = "Piccoma様",
      data_type = "ACUBE",
      
      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/ピッコマ/ピッコマ_timeconsiderd_masterdata.csv",

       # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        # rename_map = list(
        #   "モニタID" = "mid",
        #   "WB値" = "component_weight",
        #   "半年購買有無" = "半年購入有無"
        # ),
        drop_cols = c("Unnamed: 0")
      ),


        # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_利用時間",
          category_col = "プレ_利用時間_category",
          segment_col = "プレ_利用時間_segment",
          axis_label = "プレ_利用時間_segment",
          category_prefix = "プレ利用時間"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_利用時間_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年利用有無",
        "プレ_利用時間_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "usage_flag",
          "利用率",
          "新プレ_利用フラグ",
          "新ポスト_利用フラグ",
          "利用フラグ"
        ),
        make_kpi(
          "usage_time",
          "利用時間",
          "新プレ_利用時間",
          "新ポスト_利用時間",
          "利用時間"
        ),
        make_kpi(
          "usage_count",
          "利用回数",
          "新プレ_利用回数",
          "新ポスト_利用回数",
          "利用回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 02_2_market_metrics.R 用です。数値がわかり次第、以下のように指定してください。
      # media_inputs = make_media_inputs(reach = 123456, impressions = 2345678, cost = 3456789)
      media_inputs = make_media_inputs(reach = 3382363, impressions = 15167961, cost = 22448340)
    ),


    list(
      project_id = "visa_251201_260114_touch",
      project_name = "VISA様（251201-260114）_タッチ",
      output_filename_prefix = "VISA様（251201-260114）_タッチ",

      # 市場指標の定義は元のVISA MHS案件と同じにするためMHSのままです。
      data_type = "MHS",

      # ここだけが通常のMHS前処理との違いです。
      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/VISA1/visa1touch_timeconsiderd_masterdata.csv",

      # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        rename_map = list(
          "モニタID" = "mid",
          "WB値" = "component_weight",
          "半年タッチ購買有無" = "半年タッチ購入有無"
        ),
        drop_cols = c("Unnamed: 0")
      ),

      # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_タッチ購入回数",
          category_col = "プレ_タッチ購入回数_category",
          segment_col = "プレ_タッチ購入回数_segment",
          axis_label = "プレ_タッチ購入回数_segment",
          category_prefix = "プレタッチ購入回数"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_タッチ購入回数_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年タッチ購入有無",
        "プレ_タッチ購入回数_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "purchase_flag",
          "タッチ購入率",
          "新プレ_タッチ購入フラグ",
          "新ポスト_タッチ購入フラグ",
          "タッチ購入フラグ"
        ),
        make_kpi(
          "purchase_amount",
          "タッチ購入金額",
          "新プレ_タッチ購入金額",
          "新ポスト_タッチ購入金額",
          "タッチ購入金額"
        ),
        make_kpi(
          "purchase_count",
          "タッチ購入回数",
          "新プレ_タッチ購入回数",
          "新ポスト_タッチ購入回数",
          "タッチ購入回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 元のVISA 251201-260114案件の値をそのまま引き継いでいます。
      # ★ reach / impressions / cost の正しい値が別にある場合はここだけ差し替えてください。
      media_inputs = make_media_inputs(
        reach = 2211244,
        impressions = 2211244,
        cost = 2211244
      )
    ),

  list(
      project_id = "visa_251201_260114",
      project_name = "VISA様（251201-260114）",
      output_filename_prefix = "VISA様（251201-260114）",

      # 市場指標の定義は元のVISA MHS案件と同じにするためMHSのままです。
      data_type = "MHS",

      # ここだけが通常のMHS前処理との違いです。
      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/VISA1/visa1all_timeconsiderd_masterdata.csv",

      # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        rename_map = list(
          "モニタID" = "mid",
          "WB値" = "component_weight",
          "半年購買有無" = "半年購入有無"
        ),
        drop_cols = c("Unnamed: 0")
      ),

      # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_購入回数",
          category_col = "プレ_購入回数_category",
          segment_col = "プレ_購入回数_segment",
          axis_label = "プレ_購入回数_segment",
          category_prefix = "プレ購入回数"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_購入回数_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年購入有無",
        "プレ_購入回数_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "purchase_flag",
          "購入率",
          "新プレ_購入フラグ",
          "新ポスト_購入フラグ",
          "購入フラグ"
        ),
        make_kpi(
          "purchase_amount",
          "購入金額",
          "新プレ_購入金額",
          "新ポスト_購入金額",
          "購入金額"
        ),
        make_kpi(
          "purchase_count",
          "購入回数",
          "新プレ_購入回数",
          "新ポスト_購入回数",
          "購入回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 元のVISA 251201-260114案件の値をそのまま引き継いでいます。
      # ★ reach / impressions / cost の正しい値が別にある場合はここだけ差し替えてください。
      media_inputs = make_media_inputs(
        reach = 2211244,
        impressions = 2211244,
        cost = 2211244
      )
    ),

    list(
      project_id = "visa_260322_260502_touch",
      project_name = "VISA様（260322-260502）_タッチ",
      output_filename_prefix = "VISA様（260322-260502）_タッチ",

      # 市場指標の定義は元のVISA MHS案件と同じにするためMHSのままです。
      data_type = "MHS",

      # ここだけが通常のMHS前処理との違いです。
      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/VISA2/visa2touch_timeconsiderd_masterdata.csv",

      # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        rename_map = list(
          "モニタID" = "mid",
          "WB値" = "component_weight",
          "半年タッチ購買有無" = "半年タッチ購入有無"
        ),
        drop_cols = c("Unnamed: 0")
      ),

      # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_タッチ購入回数",
          category_col = "プレ_タッチ購入回数_category",
          segment_col = "プレ_タッチ購入回数_segment",
          axis_label = "プレ_タッチ購入回数_segment",
          category_prefix = "プレタッチ購入回数"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_タッチ購入回数_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年タッチ購入有無",
        "プレ_タッチ購入回数_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "purchase_flag",
          "タッチ購入率",
          "新プレ_タッチ購入フラグ",
          "新ポスト_タッチ購入フラグ",
          "タッチ購入フラグ"
        ),
        make_kpi(
          "purchase_amount",
          "タッチ購入金額",
          "新プレ_タッチ購入金額",
          "新ポスト_タッチ購入金額",
          "タッチ購入金額"
        ),
        make_kpi(
          "purchase_count",
          "タッチ購入回数",
          "新プレ_タッチ購入回数",
          "新ポスト_タッチ購入回数",
          "タッチ購入回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 元のVISA 251201-260114案件の値をそのまま引き継いでいます。
      # ★ reach / impressions / cost の正しい値が別にある場合はここだけ差し替えてください。
      media_inputs = make_media_inputs(reach = 3257269, impressions = 7024800, cost = 13704268)
    ),

  list(
      project_id = "visa_260322_260502",
      project_name = "VISA様（260322-260502）",
      output_filename_prefix = "VISA様（260322-260502）",

      # 市場指標の定義は元のVISA MHS案件と同じにするためMHSのままです。
      data_type = "MHS",

      # ここだけが通常のMHS前処理との違いです。
      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/VISA2/visa2all_timeconsiderd_masterdata.csv",

      # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        rename_map = list(
          "モニタID" = "mid",
          "WB値" = "component_weight",
          "半年購買有無" = "半年購入有無"
        ),
        drop_cols = c("Unnamed: 0")
      ),

      # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_購入回数",
          category_col = "プレ_購入回数_category",
          segment_col = "プレ_購入回数_segment",
          axis_label = "プレ_購入回数_segment",
          category_prefix = "プレ購入回数"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_購入回数_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年購入有無",
        "プレ_購入回数_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "purchase_flag",
          "購入率",
          "新プレ_購入フラグ",
          "新ポスト_購入フラグ",
          "購入フラグ"
        ),
        make_kpi(
          "purchase_amount",
          "購入金額",
          "新プレ_購入金額",
          "新ポスト_購入金額",
          "購入金額"
        ),
        make_kpi(
          "purchase_count",
          "購入回数",
          "新プレ_購入回数",
          "新ポスト_購入回数",
          "購入回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 元のVISA 251201-260114案件の値をそのまま引き継いでいます。
      # ★ reach / impressions / cost の正しい値が別にある場合はここだけ差し替えてください。
      media_inputs = make_media_inputs(reach = 3257269, impressions = 7024800, cost = 13704268)
    ),

    list(
      project_id = "Rits",
      project_name = "Rits様",
      output_filename_prefix = "Rits様",
      data_type = "MHS",

      # ここだけが通常のMHS前処理との違いです。
      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/リッツ/リッツ_timeconsiderd_masterdata.csv",

      # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        rename_map = list(
          "モニタID" = "mid",
          "WB値" = "component_weight",
          "半年購買有無" = "半年購入有無"
        ),
        drop_cols = c("Unnamed: 0")
      ),

      # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_購入回数",
          category_col = "プレ_購入回数_category",
          segment_col = "プレ_購入回数_segment",
          axis_label = "プレ_購入回数_segment",
          category_prefix = "プレ購入回数"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_購入回数_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年購入有無",
        "プレ_購入回数_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "purchase_flag",
          "購入率",
          "新プレ_購入フラグ",
          "新ポスト_購入フラグ",
          "購入フラグ"
        ),
        make_kpi(
          "purchase_amount",
          "購入金額",
          "新プレ_購入金額",
          "新ポスト_購入金額",
          "購入金額"
        ),
        make_kpi(
          "purchase_count",
          "購入回数",
          "新プレ_購入回数",
          "新ポスト_購入回数",
          "購入回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 元のVISA 251201-260114案件の値をそのまま引き継いでいます。
      # ★ reach / impressions / cost の正しい値が別にある場合はここだけ差し替えてください。
      media_inputs = make_media_inputs(reach = 1482127, impressions = 3085123, cost = 12192660)
    ),

    list(
      project_id = "Clorets",
      project_name = "Clorets様",
      output_filename_prefix = "Clorets様",
      data_type = "MHS",

      # ここだけが通常のMHS前処理との違いです。
      input_mode = "MASTERDATA",
      masterdata_path = "/Volumes/sales-lift/ryo_takahashi/netflix様_セールスリフト/時点考慮/クロレッツ/クロレッツ_timeconsiderd_masterdata.csv",

      # masterdata内での列名を、後続処理が使う標準列名へ変換します。
      masterdata = list(
        rename_map = list(
          "モニタID" = "mid",
          "WB値" = "component_weight",
          "半年購買有無" = "半年購入有無"
        ),
        drop_cols = c("Unnamed: 0")
      ),

      # 接触データはmasterdataに結合済みなので、列名だけ指定します。
      contact = make_embedded_contact(
        sample_id_col = "SAMPLEID",
        study_col = "flg_StudyID",
        fq_col = "StudyID",
        exposure_value = 1,
        control_value = 2
      ),

      # 共通事前期間の購入回数からcategoryとsegmentを作成します。
      category_specs = list(
        make_zero_plus_decile_category_segment_spec(
          value_col = "プレ_購入回数",
          category_col = "プレ_購入回数_category",
          segment_col = "プレ_購入回数_segment",
          axis_label = "プレ_購入回数_segment",
          category_prefix = "プレ購入回数"
        )
      ),

      # 元パイプラインの6段階カテゴリと同じ値を持つsegmentを共変量に使用します。
      covariates = c(
        "プレ_購入回数_segment",
        DEFAULT_COVARIATES
      ),

      segments = c(
        DEFAULT_SEGMENTS,
        "半年購入有無",
        "プレ_購入回数_segment"
      ),

      # 個人別に設定済みの新プレ・新ポストをDIDのKPIとして使用します。
      kpis = list(
        make_kpi(
          "purchase_flag",
          "購入率",
          "新プレ_購入フラグ",
          "新ポスト_購入フラグ",
          "購入フラグ"
        ),
        make_kpi(
          "purchase_amount",
          "購入金額",
          "新プレ_購入金額",
          "新ポスト_購入金額",
          "購入金額"
        ),
        make_kpi(
          "purchase_count",
          "購入回数",
          "新プレ_購入回数",
          "新ポスト_購入回数",
          "購入回数"
        )
      ),

      weight_col = "component_weight",
      focal = 1,
      drop_missing_covariates = TRUE,

      # 元のVISA 251201-260114案件の値をそのまま引き継いでいます。
      # ★ reach / impressions / cost の正しい値が別にある場合はここだけ差し替えてください。
      media_inputs = make_media_inputs(reach = 1761773, impressions = 4426190, cost = 17492667)
    )

  )
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
