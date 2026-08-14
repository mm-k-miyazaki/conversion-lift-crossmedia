# ==============================================================================
# IPW-DID Pipeline
# 02_2_market_metrics.R
# ------------------------------------------------------------------------------
# 追加計算：IPW-DIDの全体結果から、市場当たり平均リフト / IPA / CPA / ROASを作成します。
# 属性別の計算は使わず、軸=全体・カテゴリ=全体の推計効果だけを利用します。
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. 小さいユーティリティ
# ------------------------------------------------------------------------------

null_to_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(x[[1]]))
}

safe_divide <- function(numerator, denominator) {
  if (is.na(numerator) || is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }

  numerator / denominator
}

safe_multiply <- function(x, y) {
  if (is.na(x) || is.na(y)) {
    return(NA_real_)
  }

  x * y
}

get_project_media_inputs <- function(project) {
  media_inputs <- project$media_inputs

  if (is.null(media_inputs)) {
    media_inputs <- list(reach = NA_real_, impressions = NA_real_, cost = NA_real_)
  }

  list(
    reach = null_to_na(media_inputs$reach),
    impressions = null_to_na(media_inputs$impressions),
    cost = null_to_na(media_inputs$cost)
  )
}

classify_kpi_role <- function(kpi) {
  text <- paste(
    kpi$kpi_id,
    kpi$kpi_label,
    kpi$pre_col,
    kpi$post_col,
    kpi$file_label,
    sep = " "
  )

  if (stringr::str_detect(text, "率|フラグ|flag")) {
    return("rate")
  }

  if (stringr::str_detect(text, "金額|amount")) {
    return("amount")
  }

  if (stringr::str_detect(text, "回数|count")) {
    return("count")
  }

  "other"
}

extract_overall_lift <- function(result_rds_path, focal = 1) {
  if (is.null(result_rds_path) || is.na(result_rds_path) || !file.exists(result_rds_path)) {
    return(NA_real_)
  }

  result <- readRDS(result_rds_path)

  if (!all(c("軸", "カテゴリ", "推計効果") %in% names(result))) {
    return(NA_real_)
  }

  out <- result %>%
    dplyr::filter(軸 == "全体", カテゴリ == "全体")

  if ("treat_flg" %in% names(out)) {
    out <- out %>%
      dplyr::filter(as.character(treat_flg) == as.character(focal))
  }

  if (nrow(out) == 0) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(out$推計効果[[1]]))
}

collect_project_lifts <- function(project, project_calculation_result) {
  kpi_rows <- lapply(project$kpis, function(kpi) {
    kpi_result <- project_calculation_result$kpi_results[[kpi$kpi_id]]
    result_rds_path <- if (!is.null(kpi_result)) kpi_result$rds_path else NA_character_

    tibble::tibble(
      kpi_id = kpi$kpi_id,
      kpi_label = kpi$kpi_label,
      file_label = kpi$file_label,
      kpi_role = classify_kpi_role(kpi),
      lift = extract_overall_lift(
        result_rds_path = result_rds_path,
        focal = project$focal
      )
    )
  })

  dplyr::bind_rows(kpi_rows)
}

get_first_lift_by_role <- function(lifts, role) {
  out <- lifts %>%
    dplyr::filter(kpi_role == role, !is.na(lift))

  if (nrow(out) == 0) {
    return(NA_real_)
  }

  out$lift[[1]]
}

# ------------------------------------------------------------------------------
# 1. 市場指標作成
# ------------------------------------------------------------------------------

make_market_metrics_for_project <- function(project, project_calculation_result) {
  media <- get_project_media_inputs(project)
  lifts <- collect_project_lifts(project, project_calculation_result)

  data_type <- toupper(project$data_type)

  rate_lift <- get_first_lift_by_role(lifts, "rate")
  amount_lift <- get_first_lift_by_role(lifts, "amount")
  count_lift <- get_first_lift_by_role(lifts, "count")

  incremental_rate_target <- safe_multiply(media$reach, rate_lift / 100)
  incremental_amount <- safe_multiply(media$reach, amount_lift)
  incremental_count <- safe_multiply(media$reach, count_lift)

  if (data_type == "MHS") {
    metrics <- tibble::tibble(
      指標 = c(
        "市場当たり平均購入人数",
        "市場当たり平均購入金額",
        "市場当たり平均購入回数",
        "IPA",
        "CPA",
        "ROAS"
      ),
      値 = c(
        incremental_rate_target,
        incremental_amount,
        incremental_count,
        safe_divide(media$impressions, incremental_rate_target),
        safe_divide(media$cost, incremental_rate_target),
        safe_divide(incremental_amount, media$cost)
      ),
      計算式 = c(
        "リーチ人数 × 購入率のリフト / 100", # %には100をかけているので100で割り戻す
        "リーチ人数 × 購入金額のリフト",
        "リーチ人数 × 購入回数のリフト",
        "impression / 購入率のリフト由来の増分人数",
        "出稿費用 / 購入率のリフト由来の増分人数",
        "購入金額のリフト由来の増分金額 / 出稿費用"
      )
    )
  } else if (data_type == "ACUBE" || data_type == "A-CUBE") {
    metrics <- tibble::tibble(
      指標 = c(
        "市場当たり平均利用人数",
        "市場当たり平均利用回数",
        "IPA",
        "CPA"
      ),
      値 = c(
        incremental_rate_target,
        incremental_count,
        safe_divide(media$impressions, incremental_rate_target),
        safe_divide(media$cost, incremental_rate_target)
      ),
      計算式 = c(
        "リーチ人数 × 利用率のリフト / 100",
        "リーチ人数 × 利用回数のリフト",
        "impression / 利用率のリフト由来の増分人数",
        "出稿費用 / 利用率のリフト由来の増分人数"
      )
    )
  } else {
    metrics <- tibble::tibble(
      指標 = character(),
      値 = numeric(),
      計算式 = character()
    )
  }

  metrics %>%
    dplyr::mutate(
      project_id = project$project_id,
      project_name = project$project_name,
      data_type = project$data_type,
      reach = media$reach,
      impressions = media$impressions,
      cost = media$cost,
      利用した率リフト = rate_lift,
      利用した金額リフト = amount_lift,
      利用した回数リフト = count_lift,
      注記 = dplyr::if_else(
        is.na(reach) | is.na(impressions) | is.na(cost),
        "reach / impressions / cost が未設定のため、一部指標はNAです。00_data_settings.Rのmedia_inputsに後で指定してください。",
        ""
      ),
      .before = 1
    )
}

run_market_metrics <- function(config, ctx, calculation_results) {
  cat("\n===== 市場指標計算開始 =====\n")

  if (is.null(calculation_results)) {
    stop("calculation_results がNULLです。run_calculation() の後に実行してください。")
  }

  metrics <- purrr::map_dfr(config$projects, function(project) {
    project_calculation_result <- calculation_results[[project$project_id]]

    if (is.null(project_calculation_result)) {
      warning("計算結果が見つからないためスキップします: ", project$project_id)
      return(tibble::tibble())
    }

    make_market_metrics_for_project(
      project = project,
      project_calculation_result = project_calculation_result
    )
  })

  out_dir <- file.path(ctx$result_dir, "market_metrics")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out_csv_path <- file.path(out_dir, paste0("市場指標_summary_", ctx$timestamp, ".csv"))
  out_rds_path <- file.path(ctx$rds_dir, "market_metrics_results.rds")

  readr::write_csv(metrics, out_csv_path, na = "")
  saveRDS(metrics, out_rds_path)

  cat("市場指標CSV出力完了: ", out_csv_path, "\n", sep = "")

  list(
    csv_path = out_csv_path,
    rds_path = out_rds_path,
    metrics = metrics
  )
}
