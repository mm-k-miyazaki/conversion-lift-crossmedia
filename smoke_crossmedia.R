# End-to-end calculation smoke test (Excel formatting is intentionally excluded).

args <- commandArgs(trailingOnly = TRUE)
code_dir <- if (length(args) > 0 && args[[1]] != "") args[[1]] else "."
output_dir <- if (length(args) > 1 && args[[2]] != "") {
  args[[2]]
} else {
  file.path(code_dir, ".validation_output")
}

PIPELINE_CODE_DIR <- normalizePath(code_dir, winslash = "/", mustWork = TRUE)

source(file.path(PIPELINE_CODE_DIR, "00_data_settings.R"), encoding = "UTF-8")

required_packages <- c(
  "data.table",
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "purrr",
  "stringr",
  "tibble",
  "WeightIt",
  "DRDID"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("スモークテストに必要なパッケージがありません: ", paste(missing_packages, collapse = ", "))
}

invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

source(file.path(PIPELINE_CODE_DIR, "01_data_processing.R"), encoding = "UTF-8")
source(file.path(PIPELINE_CODE_DIR, "02_calculation.R"), encoding = "UTF-8")

PIPELINE_CONFIG$output_root <- normalizePath(
  output_dir,
  winslash = "/",
  mustWork = FALSE
)

ctx <- create_run_context(PIPELINE_CONFIG)
processing_results <- run_data_processing(PIPELINE_CONFIG, ctx)
calculation_results <- run_calculation(PIPELINE_CONFIG, ctx, processing_results)

expected_category_counts <- list(
  "harekaze_netflix" = c("1" = 9446L, "2" = 32L),
  "harekaze_tver" = c("1" = 4725L, "2" = 9L),
  "harekaze_amazon" = c("1" = 21900L, "2" = 32L),
  "mercari_netflix" = c("1" = 3298L, "2" = 373L, "3" = 373L, "4" = 373L),
  "mercari_tver" = c("1" = 1893L, "2" = 215L, "3" = 215L, "4" = 214L),
  "mercari_amazon" = c("1" = 9306L, "2" = 1015L, "3" = 1015L, "4" = 1014L),
  "mercari_tv" = c("1" = 2223L, "2" = 254L, "3" = 253L, "4" = 253L)
)

if (length(calculation_results) != 7L) {
  stop("計算結果の媒体数が7件ではありません。")
}

result_checks <- lapply(calculation_results, function(project_result) {
  project <- PIPELINE_CONFIG$projects[[
    match(project_result$project_id, vapply(
      PIPELINE_CONFIG$projects,
      function(x) x$project_id,
      FUN.VALUE = character(1)
    ))
  ]]

  rawdata <- readRDS(project_result$rawdata_rds_path)
  category_col <- project$category_specs[[1]]$category_col
  expected_counts <- expected_category_counts[[project_result$project_id]]
  actual_counts <- table(factor(
    rawdata[[category_col]],
    levels = as.integer(names(expected_counts))
  ))

  if (!identical(as.integer(actual_counts), as.integer(expected_counts))) {
    stop(
      project_result$project_name,
      " の新プレ行動カテゴリ件数が期待値と一致しません: actual=",
      paste(as.integer(actual_counts), collapse = ","),
      " / expected=",
      paste(as.integer(expected_counts), collapse = ",")
    )
  }

  if (!category_col %in% project$covariates ||
      any(c("プレ_購入回数_segment", "プレ_利用回数_segment") %in% project$covariates)) {
    stop(project_result$project_name, " の行動カテゴリ共変量設定が不正です。")
  }

  if (length(project_result$kpi_results) != 3L) {
    stop(project_result$project_name, " のKPI結果が3件ではありません。")
  }

  lapply(project_result$kpi_results, function(kpi_result) {
    result <- readRDS(kpi_result$rds_path)

    if ("message" %in% names(result)) {
      messages <- unique(result$message[!is.na(result$message)])
      if (length(messages) > 0) {
        stop(
          project_result$project_name,
          " / ",
          kpi_result$kpi_label,
          " の推計に失敗しました: ",
          paste(messages, collapse = " | ")
        )
      }
    }

    if (!identical(unique(as.character(result$軸)), "全体")) {
      stop(project_result$project_name, " に全体以外の分析軸が含まれています。")
    }

    if (!all(c("案件名", "媒体", "KPI", "media_contact_flg", "接触区分") %in% names(result))) {
      stop(project_result$project_name, " の結果メタデータ列が不足しています。")
    }

    TRUE
  })
})

cat("\nSMOKE_TEST_OK: 7媒体×3KPIのIPW-DID計算が完了しました。\n")
cat("run_dir: ", ctx$run_dir, "\n", sep = "")
