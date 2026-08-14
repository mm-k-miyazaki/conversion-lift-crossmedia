
safe_name <- function(x) {
  x <- gsub("[\\/\\\\:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x
}

check_file_exists <- function(path, label = "file") {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    stop(label, " が存在しません: ", path)
  }
  invisible(TRUE)
}

check_required_cols <- function(path, required_cols, file_label) {
  header <- names(data.table::fread(path, nrows = 0, encoding = "UTF-8"))
  missing_cols <- setdiff(required_cols, header)

  if (length(missing_cols) > 0) {
    stop(
      file_label,
      " に必要列がありません: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  invisible(TRUE)
}

fill_na_zero <- function(dt, target_cols) {
  target_cols <- intersect(target_cols, names(dt))

  if (length(target_cols) == 0) {
    return(dt[])
  }

  dt[
    ,
    (target_cols) := lapply(.SD, function(x) {
      x[is.na(x)] <- 0
      x
    }),
    .SDcols = target_cols
  ]

  return(dt[])
}

# ------------------------------------------------------------------------------
# 1. MHS読み込み関数
# ------------------------------------------------------------------------------

required_wb_cols <- c(
  "モニタID",
  "WB値"
)

# 支払い方コードは必須にしない
# 通常購買案件では支払い方コードがなくても回すため
required_tmp_cols <- c(
  "モニタID",
  "購入日時",
  "購入金額（税抜）"
)

optional_tmp_cols <- c(
  "支払い方コード"
)

read_wb <- function(path, file_label) {
  check_file_exists(path, file_label)
  check_required_cols(
    path = path,
    required_cols = required_wb_cols,
    file_label = file_label
  )

  dt <- data.table::fread(
    path,
    select = required_wb_cols,
    encoding = "UTF-8"
  )

  data.table::setnames(
    dt,
    old = c("モニタID", "WB値"),
    new = c("mid", "component_weight")
  )

  if (anyDuplicated(dt$mid) > 0) {
    stop(file_label, " のモニタIDが重複しています。")
  }

  return(dt[])
}

read_tmp <- function(path, file_label) {
  check_file_exists(path, file_label)

  # ヘッダーだけ先に読む
  header <- names(data.table::fread(path, nrows = 0, encoding = "UTF-8"))

  # 必須列チェック
  missing_required <- setdiff(required_tmp_cols, header)

  if (length(missing_required) > 0) {
    stop(
      file_label,
      " に必要列がありません: ",
      paste(missing_required, collapse = ", ")
    )
  }

  # 支払い方コードなど、存在すれば読む列
  select_cols <- c(
    required_tmp_cols,
    intersect(optional_tmp_cols, header)
  )

  dt <- data.table::fread(
    path,
    select = select_cols,
    encoding = "UTF-8",
    colClasses = list(character = "購入日時")
  )

  # 必須列のリネーム
  data.table::setnames(
    dt,
    old = c("モニタID", "購入日時", "購入金額（税抜）"),
    new = c("mid", "purchase_datetime", "amount_ex_tax")
  )

  # 支払い方コードがある場合だけリネーム
  if ("支払い方コード" %in% names(dt)) {
    data.table::setnames(
      dt,
      old = "支払い方コード",
      new = "payment_code"
    )
  }

  dt[, amount_ex_tax := as.numeric(gsub(",", "", as.character(amount_ex_tax)))]

  # 支払い方コードがある場合だけ数値化
  if ("payment_code" %in% names(dt)) {
    dt[, payment_code := as.integer(as.character(payment_code))]
  }

  date_chr <- substr(dt$purchase_datetime, 1, 10)
  dt[, purchase_date := data.table::as.IDate(date_chr, format = "%Y-%m-%d")]

  # 念のため、スラッシュ区切り日付も許容します。
  bad_idx <- is.na(dt$purchase_date) & !is.na(date_chr)

  if (any(bad_idx)) {
    dt[bad_idx, purchase_date := data.table::as.IDate(date_chr[bad_idx], format = "%Y/%m/%d")]
  }

  bad_date_n <- dt[is.na(purchase_date) & !is.na(purchase_datetime), .N]

  if (bad_date_n > 0) {
    stop(file_label, " で購入日時から日付変換できない行があります。件数: ", bad_date_n)
  }

  return(dt[])
}

# ------------------------------------------------------------------------------
# 2. MHS集計関数
# ------------------------------------------------------------------------------

make_purchase_summary <- function(dt, label = "") {
  new_names <- c(
    paste0("プレ_", label, "購入フラグ"),
    paste0("プレ_", label, "購入金額"),
    paste0("プレ_", label, "購入回数"),
    paste0("ポスト_", label, "購入フラグ"),
    paste0("ポスト_", label, "購入金額"),
    paste0("ポスト_", label, "購入回数")
  )

  if (nrow(dt) == 0) {
    out <- dt[0, .(mid)]
    out[, (new_names) := .(
      integer(), numeric(), integer(),
      integer(), numeric(), integer()
    )]
    return(out[])
  }

  agg <- dt[
    ,
    .(
      購入フラグ = 1L,
      購入金額 = sum(amount_ex_tax, na.rm = TRUE),
      購入回数 = .N
    ),
    by = .(mid, period)
  ]

  wide <- data.table::dcast(
    agg,
    mid ~ period,
    value.var = c("購入フラグ", "購入金額", "購入回数"),
    fill = 0
  )

  old_names <- c(
    "購入フラグ_プレ",
    "購入金額_プレ",
    "購入回数_プレ",
    "購入フラグ_ポスト",
    "購入金額_ポスト",
    "購入回数_ポスト"
  )

  for (nm in old_names) {
    if (!nm %in% names(wide)) {
      wide[, (nm) := 0]
    }
  }

  data.table::setnames(wide, old = old_names, new = new_names)
  data.table::setcolorder(wide, c("mid", new_names))

  return(wide[])
}

make_halfyear_flag <- function(dt, label = "") {
  new_name <- paste0("半年", label, "購入有無")

  if (nrow(dt) == 0) {
    out <- dt[0, .(mid)]
    out[, (new_name) := integer()]
    return(out[])
  }

  out <- unique(dt[, .(mid)])
  out[, (new_name) := 1L]

  data.table::setcolorder(out, c("mid", new_name))

  return(out[])
}

filter_by_mhs_label <- function(dt, label_spec) {
  if (is.null(label_spec$filter_col) || is.null(label_spec$filter_values)) {
    return(dt[])
  }

  if (!label_spec$filter_col %in% names(dt)) {
    stop("label filter column が存在しません: ", label_spec$filter_col)
  }

  out <- dt[get(label_spec$filter_col) %in% label_spec$filter_values]
  return(out[])
}

merge_list_by_mid <- function(dt_list) {
  if (length(dt_list) == 0) {
    stop("merge対象のdata.tableが0件です。")
  }

  out <- dt_list[[1]]

  if (length(dt_list) >= 2) {
    for (i in 2:length(dt_list)) {
      out <- merge(
        out,
        dt_list[[i]],
        by = "mid",
        all = TRUE
      )
    }
  }

  return(out[])
}

# ------------------------------------------------------------------------------
# 3. MHS案件1件を処理
# ------------------------------------------------------------------------------

process_mhs_project <- function(project, ctx) {
  cat("\n===== MHSデータ処理開始: ", project$project_name, " =====\n", sep = "")

  mhs <- project$mhs
  labels <- mhs$labels

  if (is.null(labels) || length(labels) == 0) {
    labels <- list(make_mhs_label(label = ""))
  }

  wb_prepost <- read_wb(
    path = mhs$prepost_wb_path,
    file_label = basename(mhs$prepost_wb_path)
  )

  tmp_prepost <- read_tmp(
    path = mhs$prepost_tmp_path,
    file_label = basename(mhs$prepost_tmp_path)
  )

  wb_half <- read_wb(
    path = mhs$half_wb_path,
    file_label = basename(mhs$half_wb_path)
  )

  tmp_half <- read_tmp(
    path = mhs$half_tmp_path,
    file_label = basename(mhs$half_tmp_path)
  )

  pre_start <- data.table::as.IDate(mhs$pre_start)
  pre_end <- data.table::as.IDate(mhs$pre_end)
  post_start <- data.table::as.IDate(mhs$post_start)
  post_end <- data.table::as.IDate(mhs$post_end)

  if (pre_start > pre_end) {
    stop(project$project_name, " のPRE期間が不正です。")
  }
  if (post_start > post_end) {
    stop(project$project_name, " のPOST期間が不正です。")
  }
  if (pre_end >= post_start) {
    stop(project$project_name, " のPRE_ENDとPOST_STARTが重複または逆転しています。")
  }

  tmp_prepost[, period := NA_character_]
  tmp_prepost[purchase_date >= pre_start & purchase_date <= pre_end, period := "プレ"]
  tmp_prepost[purchase_date >= post_start & purchase_date <= post_end, period := "ポスト"]

  tmp_prepost_period <- tmp_prepost[!is.na(period)]

  # プレ・ポスト側：全体 / label別を作成してouter join
  prepost_summary_list <- lapply(labels, function(label_spec) {
    make_purchase_summary(
      dt = filter_by_mhs_label(tmp_prepost_period, label_spec),
      label = label_spec$label
    )
  })

  purchase_prepost_summary <- merge_list_by_mid(prepost_summary_list)
  prepost_summary_cols <- setdiff(names(purchase_prepost_summary), "mid")
  purchase_prepost_summary <- fill_na_zero(
    dt = purchase_prepost_summary,
    target_cols = prepost_summary_cols
  )

  # 半年側：半年WB母集団を残し、TemporaryDataに購買がない人は0
  half_summary_list <- lapply(labels, function(label_spec) {
    make_halfyear_flag(
      dt = filter_by_mhs_label(tmp_half, label_spec),
      label = label_spec$label
    )
  })

  purchase_half_summary <- merge_list_by_mid(half_summary_list)
  purchase_half_summary <- merge(
    wb_half[, .(mid)],
    purchase_half_summary,
    by = "mid",
    all.x = TRUE
  )
  purchase_half_summary[, 半年期間有効モニタフラグ := 1L]

  half_flag_cols <- vapply(
    labels,
    function(label_spec) paste0("半年", label_spec$label, "購入有無"),
    FUN.VALUE = character(1)
  )

  purchase_half_summary <- fill_na_zero(
    dt = purchase_half_summary,
    target_cols = half_flag_cols
  )

  # プレポストWB母集団にleft join。半年系NAは0埋めしない。
  out <- merge(
    wb_prepost,
    purchase_prepost_summary,
    by = "mid",
    all.x = TRUE
  )

  prepost_cols <- unlist(lapply(labels, function(label_spec) {
    label <- label_spec$label
    c(
      paste0("プレ_", label, "購入フラグ"),
      paste0("プレ_", label, "購入金額"),
      paste0("プレ_", label, "購入回数"),
      paste0("ポスト_", label, "購入フラグ"),
      paste0("ポスト_", label, "購入金額"),
      paste0("ポスト_", label, "購入回数")
    )
  }))

  out <- fill_na_zero(
    dt = out,
    target_cols = prepost_cols
  )

  out <- merge(
    out,
    purchase_half_summary,
    by = "mid",
    all.x = TRUE
  )

  int_cols <- grep("フラグ|回数|有無", names(out), value = TRUE)
  out[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]

  output_cols <- c(
    "mid",
    unlist(lapply(labels, function(label_spec) {
      label <- label_spec$label
      c(
        paste0("プレ_", label, "購入フラグ"),
        paste0("プレ_", label, "購入金額"),
        paste0("プレ_", label, "購入回数"),
        paste0("ポスト_", label, "購入フラグ"),
        paste0("ポスト_", label, "購入金額"),
        paste0("ポスト_", label, "購入回数")
      )
    })),
    "半年期間有効モニタフラグ",
    half_flag_cols,
    "component_weight"
  )

  output_cols <- unique(output_cols)
  missing_output_cols <- setdiff(output_cols, names(out))

  if (length(missing_output_cols) > 0) {
    stop("出力予定列が存在しません: ", paste(missing_output_cols, collapse = ", "))
  }

  data.table::setcolorder(out, output_cols)

  tmp_half_wb_check <- tmp_half[mid %in% wb_half$mid]

  qc <- list(
    project_id = project$project_id,
    project_name = project$project_name,
    period = data.frame(
      pre_start = as.character(pre_start),
      pre_end = as.character(pre_end),
      post_start = as.character(post_start),
      post_end = as.character(post_end)
    ),
    basic_counts = data.frame(
      dataset = c(
        "wb_prepost",
        "tmp_prepost",
        "wb_half",
        "tmp_half",
        "out"
      ),
      n_rows = c(
        nrow(wb_prepost),
        nrow(tmp_prepost),
        nrow(wb_half),
        nrow(tmp_half),
        nrow(out)
      ),
      n_mid = c(
        data.table::uniqueN(wb_prepost$mid),
        data.table::uniqueN(tmp_prepost$mid),
        data.table::uniqueN(wb_half$mid),
        data.table::uniqueN(tmp_half$mid),
        data.table::uniqueN(out$mid)
      )
    ),
    prepost_period_counts = tmp_prepost[
      ,
      .(
        row_n = .N,
        mid_n = data.table::uniqueN(mid),
        wb_prepost_mid_n = data.table::uniqueN(mid[mid %in% wb_prepost$mid])
      ),
      by = .(
        period_check = data.table::fifelse(
          purchase_date >= pre_start & purchase_date <= pre_end,
          "PRE",
          data.table::fifelse(
            purchase_date >= post_start & purchase_date <= post_end,
            "POST",
            "OTHER"
          )
        )
      )
    ][order(period_check)],
    half_counts = data.frame(
      check = c(
        "TemporaryData_half_all",
        "TemporaryData_half_in_WB"
      ),
      row_n = c(
        nrow(tmp_half),
        nrow(tmp_half_wb_check)
      ),
      mid_n = c(
        data.table::uniqueN(tmp_half$mid),
        data.table::uniqueN(tmp_half_wb_check$mid)
      )
    ),
    na_counts = out[
      ,
      lapply(.SD, function(x) sum(is.na(x))),
      .SDcols = names(out)
    ]
  )

  out_dir <- file.path(ctx$processed_dir, safe_name(project$project_name))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out_path <- file.path(
    out_dir,
    paste0(safe_name(project$output_filename_prefix), "_購買データ.csv")
  )

  data.table::fwrite(
    out,
    file = out_path,
    bom = TRUE
  )

  rds_path <- file.path(ctx$rds_dir, paste0(project$project_id, "_mhs_processed.rds"))
  qc_path <- file.path(ctx$rds_dir, paste0(project$project_id, "_mhs_qc.rds"))

  saveRDS(out, rds_path)
  saveRDS(qc, qc_path)

  cat("MHSデータ処理完了: ", out_path, "\n", sep = "")
  cat("出力行数: ", nrow(out), " / 出力列数: ", ncol(out), "\n", sep = "")

  return(list(
    project_id = project$project_id,
    project_name = project$project_name,
    data_type = project$data_type,
    prepost_path = out_path,
    processed_rds_path = rds_path,
    qc_rds_path = qc_path
  ))
}

# ------------------------------------------------------------------------------
# 4. 案件ごとのデータ処理ループ
# ------------------------------------------------------------------------------

process_project_data <- function(project, ctx) {
  input_mode <- toupper(if (is.null(project$input_mode)) "" else project$input_mode)
  data_type <- toupper(project$data_type)

  # 購買KPI・広告接触・属性が結合済みのmasterdataは、そのまま後続へ渡します。
  if (input_mode == "MASTERDATA") {
    check_file_exists(
      project$masterdata_path,
      paste0(project$project_name, " masterdata_path")
    )

    cat(
      "\n===== masterdata前処理スキップ: ",
      project$project_name,
      " =====\n",
      sep = ""
    )
    cat("結合済みmasterdataを使用します: ", project$masterdata_path, "\n", sep = "")

    return(list(
      project_id = project$project_id,
      project_name = project$project_name,
      data_type = project$data_type,
      input_mode = project$input_mode,
      prepost_path = project$masterdata_path,
      processed_rds_path = NA_character_,
      qc_rds_path = NA_character_
    ))
  }

  if (data_type == "MHS") {
    return(process_mhs_project(project, ctx))
  }

  if (data_type == "ACUBE" || data_type == "A-CUBE") {
    check_file_exists(project$prepost_path, paste0(project$project_name, " prepost_path"))
    cat("\n===== A-cubeデータ処理スキップ: ", project$project_name, " =====\n", sep = "")
    cat("既存プレポストCSVを使用します: ", project$prepost_path, "\n", sep = "")

    return(list(
      project_id = project$project_id,
      project_name = project$project_name,
      data_type = project$data_type,
      prepost_path = project$prepost_path,
      processed_rds_path = NA_character_,
      qc_rds_path = NA_character_
    ))
  }

  stop("未対応のdata_typeです: ", project$data_type)
}

run_data_processing <- function(config, ctx) {
  results <- lapply(config$projects, function(project) {
    process_project_data(project, ctx)
  })

  names(results) <- vapply(
    config$projects,
    function(project) project$project_id,
    FUN.VALUE = character(1)
  )

  saveRDS(results, file.path(ctx$rds_dir, "data_processing_results.rds"))

  return(results)
}
