

repair_csv_names <- function(x) {
  empty_index <- which(is.na(x) | x == "")
  x[empty_index] <- paste0("...", empty_index)
  make.unique(x, sep = "_")
}

read_csv_safe <- function(path) {
  check_file_exists(path, "CSV")

  tryCatch(
    {
      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE,
        guess_max = Inf,
        na = c("", "NA"),
        name_repair = repair_csv_names
      )
    },
    error = function(e_utf8) {
      message("UTF-8で読み込み失敗。CP932で再読み込みします: ", path)

      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "CP932"),
        show_col_types = FALSE,
        guess_max = Inf,
        na = c("", "NA"),
        name_repair = repair_csv_names
      )
    }
  )
}

read_contact_data <- function(contact) {
  check_file_exists(contact$path, "接触データ")

  if (contact$type == "tsv") {
    return(
      data.table::fread(
        contact$path,
        sep = contact$sep,
        encoding = contact$encoding
      ) %>%
        as.data.frame()
    )
  }

  if (contact$type == "excel") {
    return(
      readxl::read_xlsx(
        contact$path,
        sheet = contact$sheet
      ) %>%
        as.data.frame()
    )
  }

  stop("未対応の接触データtypeです: ", contact$type)
}

validate_cols_in_data <- function(data, cols, data_label) {
  cols <- cols[!is.na(cols) & cols != "" & cols != "0"]
  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      data_label,
      " に必要列がありません: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  invisible(TRUE)
}

get_analysis_id_col <- function(project) {
  if (!is.null(project$analysis_id_col) &&
      !is.na(project$analysis_id_col) &&
      project$analysis_id_col != "") {
    return(project$analysis_id_col)
  }

  "mid"
}

get_project_media <- function(project) {
  if (!is.null(project$media) && !is.na(project$media) && project$media != "") {
    return(project$media)
  }

  NA_character_
}

get_contact_fq_col <- function(contact) {
  if (!is.null(contact$fq_col) && !is.na(contact$fq_col) && contact$fq_col != "") {
    return(contact$fq_col)
  }

  # 00_data_settings.R 側で fq_col をまだ追加していない場合のデフォルトです。
  # flg_StudyID は接触/非接触フラグ、StudyID は接触回数として使います。
  return("StudyID")
}

uses_segment <- function(project, segment_name) {
  !is.null(project$segments) && segment_name %in% project$segments
}

add_age_group <- function(contact_df) {
  if (!"AGE" %in% names(contact_df)) {
    if ("AGE_GROUP" %in% names(contact_df)) {
      return(contact_df)
    }
    stop("接触データに AGE 列がありません。AGE_GROUPを作成できません。")
  }

  contact_df %>%
    dplyr::mutate(
      AGE = as.numeric(AGE),
      AGE_GROUP = dplyr::case_when(
        AGE >= 18 & AGE <= 29 ~ "18~29歳",
        AGE >= 30 & AGE <= 39 ~ "30~39歳",
        AGE >= 40 & AGE <= 49 ~ "40~49歳",
        AGE >= 50 & AGE <= 59 ~ "50~59歳",
        AGE >= 60 & AGE <= 69 ~ "60~69歳",
        TRUE ~ NA_character_
      ),
      AGE_GROUP = factor(
        AGE_GROUP,
        levels = c(
          "18~29歳",
          "30~39歳",
          "40~49歳",
          "50~59歳",
          "60~69歳"
        )
      )
    )
}

add_sex_age_group <- function(contact_df) {
  validate_cols_in_data(contact_df, c("SEX", "AGE_GROUP"), "接触データ")

  contact_df %>%
    dplyr::mutate(
      SEX_AGE_GROUP = dplyr::case_when(
        as.character(SEX) == "1" & !is.na(AGE_GROUP) ~ paste0("男性(1)_", as.character(AGE_GROUP)),
        as.character(SEX) == "2" & !is.na(AGE_GROUP) ~ paste0("女性(2)_", as.character(AGE_GROUP)),
        TRUE ~ NA_character_
      ),
      SEX_AGE_GROUP = factor(
        SEX_AGE_GROUP,
        levels = unlist(lapply(
          c("男性(1)", "女性(2)"),
          function(sex_i) paste(
            sex_i,
            c("18~29歳", "30~39歳", "40~49歳", "50~59歳", "60~69歳"),
            sep = "_"
          )
        ))
      )
    )
}

add_mf1_3 <- function(contact_df) {
  validate_cols_in_data(contact_df, c("SEX", "AGE"), "接触データ")

  contact_df %>%
    dplyr::mutate(
      AGE = as.numeric(AGE),
      MF1_3 = dplyr::case_when(
        as.character(SEX) == "1" & AGE >= 18 & AGE <= 34 ~ "M1",
        as.character(SEX) == "1" & AGE >= 35 & AGE <= 49 ~ "M2",
        as.character(SEX) == "1" & AGE >= 50 & AGE <= 69 ~ "M3",
        as.character(SEX) == "2" & AGE >= 18 & AGE <= 34 ~ "F1",
        as.character(SEX) == "2" & AGE >= 35 & AGE <= 49 ~ "F2",
        as.character(SEX) == "2" & AGE >= 50 & AGE <= 69 ~ "F3",
        TRUE ~ NA_character_
      ),
      MF1_3 = factor(MF1_3, levels = c("M1", "M2", "M3", "F1", "F2", "F3"))
    )
}

add_fq_category <- function(contact_df, fq_col = "StudyID") {
  if (is.null(fq_col) || is.na(fq_col) || fq_col == "") {
    return(contact_df)
  }

  if (!fq_col %in% names(contact_df)) {
    stop("FQ別分析に必要な列が接触データにありません: ", fq_col)
  }

  contact_df %>%
    dplyr::mutate(
      FQ_COUNT = suppressWarnings(as.numeric(.data[[fq_col]])),
      FQ_CATEGORY = dplyr::case_when(
        is.na(FQ_COUNT) ~ NA_character_,
        FQ_COUNT == 0 ~ "0回（非接触者）",
        FQ_COUNT >= 1 & FQ_COUNT <= 2 ~ "1～2回",
        FQ_COUNT >= 3 & FQ_COUNT <= 4 ~ "3～4回",
        FQ_COUNT >= 5 & FQ_COUNT <= 6 ~ "5～6回",
        FQ_COUNT >= 7 ~ "7回以上",
        TRUE ~ NA_character_
      ),
      FQ_CATEGORY = factor(
        FQ_CATEGORY,
        levels = c(
          "0回（非接触者）",
          "1～2回",
          "3～4回",
          "5～6回",
          "7回以上"
        )
      )
    )
}

add_default_contact_segments <- function(contact_df, contact, segments) {
  out <- add_age_group(contact_df)

  if ("SEX_AGE_GROUP" %in% segments) {
    out <- add_sex_age_group(out)
  }

  if ("MF1_3" %in% segments) {
    out <- add_mf1_3(out)
  }

  if ("FQ_CATEGORY" %in% segments) {
    out <- add_fq_category(
      contact_df = out,
      fq_col = get_contact_fq_col(contact)
    )
  }

  return(out)
}

# ------------------------------------------------------------------------------
# 1. 分析カテゴリ作成
# ------------------------------------------------------------------------------

validate_nonnegative_category_value <- function(data, value_col) {
  validate_cols_in_data(data, value_col, "カテゴリ作成元データ")

  invalid_negative <- !is.na(data[[value_col]]) & data[[value_col]] < 0
  if (any(invalid_negative)) {
    stop(
      value_col,
      " に0未満の値があります。Non/Any・Non/HMLカテゴリを作成できません。N=",
      sum(invalid_negative)
    )
  }

  invisible(TRUE)
}

# 0をNon、0超をAnyとする2区分。
add_zero_vs_positive_category <- function(data, value_col, category_col) {
  validate_nonnegative_category_value(data, value_col)

  data %>%
    dplyr::mutate(
      !!category_col := dplyr::case_when(
        is.na(.data[[value_col]]) ~ NA_integer_,
        .data[[value_col]] == 0 ~ 1L,
        .data[[value_col]] > 0 ~ 2L,
        TRUE ~ NA_integer_
      )
    )
}

# 0をNon、0超を値順＋ID順で人数がほぼ均等なLight/Middle/Heavyにする4区分。
add_zero_plus_tertile_category <- function(
  data,
  value_col,
  category_col,
  tie_breaker_col
) {
  validate_nonnegative_category_value(data, value_col)
  validate_cols_in_data(data, tie_breaker_col, "カテゴリの同値順序決定データ")

  data_with_id <- data %>%
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .tie_breaker = as.character(.data[[tie_breaker_col]])
    )

  df_na <- data_with_id %>%
    dplyr::filter(is.na(.data[[value_col]])) %>%
    dplyr::mutate(!!category_col := NA_integer_)

  df_zero <- data_with_id %>%
    dplyr::filter(!is.na(.data[[value_col]]) & .data[[value_col]] == 0) %>%
    dplyr::mutate(!!category_col := 1L)

  df_positive <- data_with_id %>%
    dplyr::filter(!is.na(.data[[value_col]]) & .data[[value_col]] > 0) %>%
    dplyr::arrange(.data[[value_col]], .tie_breaker, .row_id) %>%
    dplyr::mutate(!!category_col := dplyr::ntile(dplyr::row_number(), 3) + 1L)

  dplyr::bind_rows(df_zero, df_positive, df_na) %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(-.row_id, -.tie_breaker)
}

# 旧案件設定との後方互換用：0をカテゴリ1、0以外を10分位でカテゴリ2〜11にする。
add_zero_plus_decile_category <- function(data, value_col, category_col) {
  validate_cols_in_data(data, value_col, "カテゴリ作成元データ")

  data_with_id <- data %>%
    dplyr::mutate(.row_id = dplyr::row_number())

  df_na <- data_with_id %>%
    dplyr::filter(is.na(.data[[value_col]])) %>%
    dplyr::mutate(!!category_col := NA_integer_)

  df_zero <- data_with_id %>%
    dplyr::filter(!is.na(.data[[value_col]]) & .data[[value_col]] == 0) %>%
    dplyr::mutate(!!category_col := 1L)

  df_non_zero <- data_with_id %>%
    dplyr::filter(!is.na(.data[[value_col]]) & .data[[value_col]] != 0) %>%
    dplyr::mutate(!!category_col := dplyr::ntile(.data[[value_col]], 10) + 1L)

  dplyr::bind_rows(df_zero, df_non_zero, df_na) %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(-.row_id)
}

apply_category_spec <- function(rawdata, spec) {
  if (spec$method == "zero_vs_positive") {
    return(
      rawdata %>%
        add_zero_vs_positive_category(
          value_col = spec$value_col,
          category_col = spec$category_col
        )
    )
  }

  if (spec$method == "zero_plus_tertile") {
    if (is.null(spec$tie_breaker_col) ||
        is.na(spec$tie_breaker_col) ||
        spec$tie_breaker_col == "") {
      stop("zero_plus_tertileにはtie_breaker_colが必要です。")
    }

    return(
      rawdata %>%
        add_zero_plus_tertile_category(
          value_col = spec$value_col,
          category_col = spec$category_col,
          tie_breaker_col = spec$tie_breaker_col
        )
    )
  }

  if (spec$method == "zero_plus_decile_6") {
    rawdata <- rawdata %>%
      add_zero_plus_decile_category(
        value_col = spec$value_col,
        category_col = spec$category_col
      )

    rawdata <- rawdata %>%
      dplyr::mutate(
        !!spec$category_col := dplyr::case_when(
          .data[[spec$category_col]] == 1 ~ 1L,
          .data[[spec$category_col]] %in% c(2, 3) ~ 2L,
          .data[[spec$category_col]] %in% c(4, 5) ~ 3L,
          .data[[spec$category_col]] %in% c(6, 7) ~ 4L,
          .data[[spec$category_col]] %in% c(8, 9) ~ 5L,
          .data[[spec$category_col]] %in% c(10, 11) ~ 6L,
          TRUE ~ NA_integer_
        )
      )

    return(rawdata)
  }

  if (spec$method == "zero_plus_decile_category_segment") {
    raw_category_col <- spec$raw_category_col
    segment_col <- spec$category_col

    rawdata <- rawdata %>%
      add_zero_plus_decile_category(
        value_col = spec$value_col,
        category_col = raw_category_col
      ) %>%
      dplyr::mutate(
        !!segment_col := dplyr::case_when(
          .data[[raw_category_col]] == 1 ~ 1L,
          .data[[raw_category_col]] %in% c(2, 3) ~ 2L,
          .data[[raw_category_col]] %in% c(4, 5) ~ 3L,
          .data[[raw_category_col]] %in% c(6, 7) ~ 4L,
          .data[[raw_category_col]] %in% c(8, 9) ~ 5L,
          .data[[raw_category_col]] %in% c(10, 11) ~ 6L,
          TRUE ~ NA_integer_
        )
      )

    return(rawdata)
  }

  stop("未対応のcategory methodです: ", spec$method)
}

apply_category_specs <- function(rawdata, category_specs) {
  if (is.null(category_specs) || length(category_specs) == 0) {
    return(rawdata)
  }

  for (spec in category_specs) {
    rawdata <- apply_category_spec(rawdata, spec)
  }

  return(rawdata)
}

# ------------------------------------------------------------------------------
# 2. ラベル・並び順設定
# ------------------------------------------------------------------------------

make_label_config <- function(project) {
  age_levels <- c("18~29歳", "30~39歳", "40~49歳", "50~59歳", "60~69歳")
  sex_levels <- c("男性(1)", "女性(2)")
  sex_age_levels <- unlist(lapply(
    sex_levels,
    function(sex_i) paste(sex_i, age_levels, sep = "_")
  ))

  axis_labels <- c(
    "SEX" = "性別",
    "AGE_GROUP" = "年代別",
    "SEX_AGE_GROUP" = "性×年代別",
    "MF1_3" = "MF1-3別",
    "FQ_CATEGORY" = "FQ別",
    "半年利用有無" = "半年以内利用有無",
    "半年購入有無" = "半年以内購入有無",
    "半年タッチ購入有無" = "半年以内タッチ購入有無"
  )

  category_labels <- list(
    "SEX" = c("1" = "男性(1)", "2" = "女性(2)"),
    "SEX_AGE_GROUP" = stats::setNames(sex_age_levels, sex_age_levels),
    "MF1_3" = c(
      "M1" = "M1",
      "M2" = "M2",
      "M3" = "M3",
      "F1" = "F1",
      "F2" = "F2",
      "F3" = "F3"
    ),
    "FQ_CATEGORY" = c(
      "0" = "0回（非接触者）",
      "1_2" = "0回（非接触者）vs.1～2回",
      "3_4" = "0回（非接触者）vs.3～4回",
      "5_6" = "0回（非接触者）vs.5～6回",
      "7_plus" = "0回（非接触者）vs.7回以上"
    ),
    "半年利用有無" = c("1" = "あり(1)", "0" = "なし(0)"),
    "半年購入有無" = c("1" = "あり(1)", "0" = "なし(0)"),
    "半年タッチ購入有無" = c("1" = "あり(1)", "0" = "なし(0)")
  )

  if (!is.null(project$category_specs) && length(project$category_specs) > 0) {
    for (spec in project$category_specs) {
      axis_labels[spec$category_col] <- spec$axis_label
      category_labels[[spec$category_col]] <- spec$category_labels
    }
  }

  axis_order <- c("全体")
  for (seg_col in project$segments) {
    axis_label_i <- if (seg_col %in% names(axis_labels)) {
      unname(axis_labels[[seg_col]])
    } else {
      seg_col
    }

    axis_order <- c(axis_order, axis_label_i)
  }
  axis_order <- unique(axis_order)

  category_order <- c(
    "全体",
    "男性(1)",
    "女性(2)",
    age_levels,
    sex_age_levels,
    "M1",
    "M2",
    "M3",
    "F1",
    "F2",
    "F3",
    "0回（非接触者）",
    "0回（非接触者）vs.1～2回",
    "0回（非接触者）vs.3～4回",
    "0回（非接触者）vs.5～6回",
    "0回（非接触者）vs.7回以上",
    "あり(1)",
    "なし(0)"
  )

  # 既存のカテゴリ軸も必ず並び順に追加します。
  # 例：プレ購入回数_Non / Light / Semi-Light / Middle / Semi-Heavy / Heavy
  if (length(category_labels) > 0) {
    for (nm in names(category_labels)) {
      category_order <- c(category_order, unname(category_labels[[nm]]))
    }
  }
  category_order <- unique(category_order)

  list(
    axis_labels = axis_labels,
    category_labels = category_labels,
    axis_order = axis_order,
    category_order = category_order
  )
}

label_axis <- function(seg_col, label_config) {
  if (seg_col %in% names(label_config$axis_labels)) {
    return(unname(label_config$axis_labels[[seg_col]]))
  }

  return(seg_col)
}

label_category <- function(seg_col, value, label_config) {
  value_chr <- as.character(value)

  if (seg_col == "AGE_GROUP") {
    return(value_chr)
  }

  if (seg_col %in% names(label_config$category_labels)) {
    map <- label_config$category_labels[[seg_col]]
    if (value_chr %in% names(map)) {
      return(unname(map[[value_chr]]))
    }
  }

  return(value_chr)
}

order_by_map <- function(x, order_vec) {
  out <- match(x, order_vec)
  out[is.na(out)] <- 99L
  out
}

# ------------------------------------------------------------------------------
# 3. 分析用rawdata作成
# ------------------------------------------------------------------------------

prepare_rawdata <- function(project, project_runtime, ctx) {
  cat("\n===== 分析用rawdata作成: ", project$project_name, " =====\n", sep = "")

  input_mode <- toupper(if (is.null(project$input_mode)) "" else project$input_mode)
  analysis_id_col <- get_analysis_id_col(project)

  if (input_mode == "MASTERDATA") {
    rawdata <- read_csv_safe(project_runtime$prepost_path)

    masterdata_config <- project$masterdata
    rename_map <- masterdata_config$rename_map

    # CSV保存時に付いた不要な行番号列などを除外します。
    drop_cols <- intersect(masterdata_config$drop_cols, names(rawdata))
    unnamed_cols <- grep("^Unnamed:", names(rawdata), value = TRUE)
    drop_cols <- unique(c(drop_cols, unnamed_cols))

    if (length(drop_cols) > 0) {
      rawdata <- rawdata %>%
        dplyr::select(-dplyr::all_of(drop_cols))
    }

    # masterdataの列名を、元パイプラインの標準列名に揃えます。
    if (!is.null(rename_map) && length(rename_map) > 0) {
      for (old_name in names(rename_map)) {
        new_name <- as.character(rename_map[[old_name]])

        if (!old_name %in% names(rawdata)) {
          stop("masterdataの列名変換元が存在しません: ", old_name)
        }

        if (new_name %in% names(rawdata) && new_name != old_name) {
          stop(
            "masterdataの列名変換先がすでに存在します: ",
            old_name,
            " -> ",
            new_name
          )
        }

        names(rawdata)[names(rawdata) == old_name] <- new_name
      }
    }

    # クロスメディアmasterdataはID×媒体で1行のため、対象媒体を先に絞る。
    media_col <- masterdata_config$media_col
    media_value <- masterdata_config$media_value

    if (!is.null(media_col) && !is.null(media_value) &&
        !is.na(media_col) && !is.na(media_value) &&
        media_col != "" && media_value != "") {
      validate_cols_in_data(
        rawdata,
        media_col,
        paste0(project$project_name, " masterdata")
      )

      rawdata <- rawdata %>%
        dplyr::filter(as.character(.data[[media_col]]) == as.character(media_value))

      if (nrow(rawdata) == 0) {
        stop(project$project_name, " の対象媒体行が存在しません: ", media_value)
      }

      cat("対象媒体: ", media_value, " / N: ", nrow(rawdata), "\n", sep = "")
    }

    validate_cols_in_data(
      rawdata,
      c(
        analysis_id_col,
        "component_weight",
        project$contact$sample_id_col,
        project$contact$study_col
      ),
      paste0(project$project_name, " masterdata")
    )

    id_values <- rawdata[[analysis_id_col]]
    id_missing <- is.na(id_values) | trimws(as.character(id_values)) == ""

    if (any(id_missing)) {
      stop(
        project$project_name,
        " のmasterdataで分析IDが欠損しています: ",
        analysis_id_col,
        " / N=",
        sum(id_missing)
      )
    }

    if (anyDuplicated(id_values) > 0) {
      stop(
        project$project_name,
        " のmasterdataで同一ID×媒体が重複しています: ",
        analysis_id_col
      )
    }

    exposure_value <- as.character(project$contact$exposure_value)
    control_value <- as.character(project$contact$control_value)
    study_col <- project$contact$study_col
    study_values <- as.character(rawdata[[study_col]])
    allowed_study_values <- c(exposure_value, control_value)
    invalid_study <- is.na(study_values) | !(study_values %in% allowed_study_values)

    if (any(invalid_study)) {
      invalid_labels <- unique(study_values[invalid_study])
      invalid_labels[is.na(invalid_labels)] <- "NA"
      stop(
        project$project_name,
        " のmedia_contact_flgに1/2以外または欠損があります: ",
        paste(invalid_labels, collapse = ", ")
      )
    }

    study_counts <- table(factor(study_values, levels = allowed_study_values))
    if (any(study_counts == 0)) {
      stop(
        project$project_name,
        " は接触群または非接触群が存在しないため推計できません。"
      )
    }

    cat(
      "接触群 N: ", study_counts[[1]],
      " / 非接触群 N: ", study_counts[[2]],
      "\n",
      sep = ""
    )

    # 対象媒体自身のフラグとmedia_contact_flgの不整合を早期検出する。
    if (!is.null(project$own_media_flag) &&
        !is.na(project$own_media_flag) &&
        project$own_media_flag != "") {
      validate_cols_in_data(
        rawdata,
        project$own_media_flag,
        paste0(project$project_name, " masterdata")
      )

      own_flag_values <- as.character(rawdata[[project$own_media_flag]])
      inconsistent_own_flag <- is.na(own_flag_values) | own_flag_values != study_values

      if (any(inconsistent_own_flag)) {
        stop(
          project$project_name,
          " でmedia_contact_flgと対象媒体フラグ(",
          project$own_media_flag,
          ")が一致しません。N=",
          sum(inconsistent_own_flag)
        )
      }
    }

    if (uses_segment(project, "FQ_CATEGORY")) {
      validate_cols_in_data(
        rawdata,
        get_contact_fq_col(project$contact),
        paste0(project$project_name, " masterdata")
      )
    }

    # 全体分析のみの場合は、分析に使わない属性セグメントを作成しない。
    if (!is.null(project$segments) && length(project$segments) > 0) {
      rawdata <- add_default_contact_segments(
        contact_df = rawdata,
        contact = project$contact,
        segments = project$segments
      )
    }

    rawdata <- rawdata %>%
      dplyr::mutate(
        treat_flg = dplyr::case_when(
          as.character(.data[[study_col]]) == exposure_value ~ 1,
          as.character(.data[[study_col]]) == control_value ~ 0,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::filter(!is.na(treat_flg))

  } else {
    prepost_df <- read_csv_safe(project_runtime$prepost_path)
    contact_df <- read_contact_data(project$contact)

    validate_cols_in_data(
      prepost_df,
      "mid",
      paste0(project$project_name, " プレポストデータ")
    )

    contact_required_cols <- c(
      project$contact$sample_id_col,
      project$contact$study_col
    )

    if (uses_segment(project, "FQ_CATEGORY")) {
      contact_required_cols <- c(
        contact_required_cols,
        get_contact_fq_col(project$contact)
      )
    }

    validate_cols_in_data(
      contact_df,
      contact_required_cols,
      paste0(project$project_name, " 接触データ")
    )

    if (anyDuplicated(contact_df[[project$contact$sample_id_col]]) > 0) {
      stop(project$project_name, " の接触データで ", project$contact$sample_id_col, " が重複しています。")
    }

    # flg_StudyID は treat_flg 作成、StudyID は FQ_CATEGORY 作成に使います。
    contact_df <- add_default_contact_segments(
      contact_df = contact_df,
      contact = project$contact,
      segments = project$segments
    )

    join_by <- project$contact$sample_id_col
    names(join_by) <- "mid"

    exposure_value <- as.character(project$contact$exposure_value)
    control_value <- as.character(project$contact$control_value)
    study_col <- project$contact$study_col

    rawdata <- prepost_df %>%
      dplyr::inner_join(
        contact_df,
        by = join_by
      ) %>%
      dplyr::mutate(
        treat_flg = dplyr::case_when(
          as.character(.data[[study_col]]) == exposure_value ~ 1,
          as.character(.data[[study_col]]) == control_value ~ 0,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::filter(!is.na(treat_flg))
  }

  # 媒体別に設定された新プレ行動列から調整カテゴリを作成します。
  rawdata <- apply_category_specs(rawdata, project$category_specs)

  # KPI/共変量/分析軸/weightの列存在チェック
  kpi_cols <- unlist(lapply(project$kpis, function(kpi) c(kpi$pre_col, kpi$post_col)))
  required_cols <- unique(c(
    analysis_id_col,
    "treat_flg",
    kpi_cols,
    project$covariates,
    project$segments,
    project$weight_col
  ))

  validate_cols_in_data(
    rawdata,
    required_cols,
    paste0(project$project_name, " rawdata")
  )

  if (isTRUE(project$drop_missing_covariates)) {
    cov_list_for_filter <- project$covariates
    cov_list_for_filter <- cov_list_for_filter[
      !is.na(cov_list_for_filter) & cov_list_for_filter != "" & cov_list_for_filter != "0"
    ]

    cat("共変量欠損除外前 N: ", nrow(rawdata), "\n", sep = "")

    missing_summary <- rawdata %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(cov_list_for_filter),
          ~ sum(is.na(.x)),
          .names = "NA_{.col}"
        )
      )

    print(missing_summary)

    rawdata <- rawdata %>%
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(cov_list_for_filter),
          ~ !is.na(.x)
        )
      )

    cat("共変量欠損除外後 N: ", nrow(rawdata), "\n", sep = "")
  }

  # category / segmentを付与した分析用データを、確認用CSVとして保存します。
  processed_project_dir <- file.path(ctx$processed_dir, safe_name(project$project_name))
  dir.create(processed_project_dir, recursive = TRUE, showWarnings = FALSE)

  prepared_csv_path <- file.path(
    processed_project_dir,
    paste0(safe_name(project$output_filename_prefix), "_分析用masterdata.csv")
  )

  readr::write_csv(
    rawdata,
    file = prepared_csv_path,
    na = ""
  )

  rawdata_rds_path <- file.path(ctx$rds_dir, paste0(project$project_id, "_rawdata.rds"))
  saveRDS(rawdata, rawdata_rds_path)

  cat("rawdata作成完了: N=", nrow(rawdata), ", 列数=", ncol(rawdata), "\n", sep = "")
  cat("分析用masterdata出力: ", prepared_csv_path, "\n", sep = "")

  return(list(
    rawdata = rawdata,
    rawdata_rds_path = rawdata_rds_path,
    prepared_csv_path = prepared_csv_path
  ))
}


# ------------------------------------------------------------------------------
# 4. set_df作成
# ------------------------------------------------------------------------------

make_set_df <- function(project, kpi) {
  covariates <- project$covariates
  segments <- project$segments
  analysis_id_col <- get_analysis_id_col(project)

  covariates <- covariates[!is.na(covariates) & covariates != "" & covariates != "0"]
  segments <- segments[!is.na(segments) & segments != "" & segments != "0"]

  data.frame(
    item = c(
      "id",
      "treatflg",
      "kpi_pre",
      "kpi_post",
      rep("covariate", length(covariates)),
      rep("segment", length(segments)),
      "weight",
      "focal"
    ),
    value = c(
      analysis_id_col,
      "treat_flg",
      kpi$pre_col,
      kpi$post_col,
      covariates,
      segments,
      project$weight_col,
      as.character(project$focal)
    ),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 5. サブ関数：データセットを与えるとリフト値を計算する
# ------------------------------------------------------------------------------
# 添付コードの構造を維持しています。
# ただし以下を変更しています。
# - 率/フラグ系KPIだけ100倍、金額/回数系KPIは100倍しない
# - component_weightは読んでおくが、WeightIt/DRDIDの計算には掛けない
# - 相対リフトを追加
# ------------------------------------------------------------------------------

get_kpi_multiplier <- function(kpi) {
  text <- paste(
    kpi$kpi_id,
    kpi$kpi_label,
    kpi$pre_col,
    kpi$post_col,
    collapse = " "
  )

  if (grepl("率|フラグ|flag|rate", text, ignore.case = TRUE)) { # 若干怪しいかも
    return(100)
  }

  return(1)
}

add_relative_lift <- function(lift, treatflg, focal_use) {
  if (!all(c(treatflg, "前期間", "後期間") %in% names(lift))) {
    lift$`相対リフト` <- NA_real_
    return(lift)
  }

  treat_chr <- as.character(lift[[treatflg]])
  focal_chr <- as.character(focal_use)

  focal_row <- which(treat_chr == focal_chr)
  control_row <- which(treat_chr != focal_chr)

  if (length(focal_row) != 1 || length(control_row) != 1) {
    lift$`相対リフト` <- NA_real_
    return(lift)
  }

  exposed_pre <- lift$`前期間`[focal_row]
  exposed_post <- lift$`後期間`[focal_row]
  control_pre <- lift$`前期間`[control_row]
  control_post <- lift$`後期間`[control_row]

  counterfactual_post <- exposed_pre + (control_post - control_pre)

  rel_lift <- NA_real_
  if (!is.na(counterfactual_post) && counterfactual_post != 0) {
    rel_lift <- (exposed_post / counterfactual_post - 1) * 100
  }

  lift$`相対リフト` <- NA_real_
  lift$`相対リフト`[focal_row] <- rel_lift

  return(lift)
}

hand_ipwdid <- function(
  df, 
  weight_formula, 
  s_weight, 
  focal = 1, 
  id, 
  treatflg, 
  outcome,
  value_multiplier = 100 # 追加
  ) {
  data_for_weight <- df %>% dplyr::filter(period == 0)

  focal_use <- suppressWarnings(as.numeric(focal))
  if (is.na(focal_use)) {
    focal_use <- focal
  }

  if (length(unique(data_for_weight[[treatflg]])) < 2) {
    return(data.frame(message = "処置群または対照群が片方しか存在しないため推計できません."))
  }

  ## weightit で weight 推定
  ## component_weightはデータとして保持しますが、計算には掛けません。
  weightit_res <- data_for_weight %>%
    WeightIt::weightit(
      formula = weight_formula,
      data = .,
      #s.weights = s_weight,  # ★旧仕様：component_weightをWeightItに渡す
      method = "ps",
      estimand = "ATT",
      stabilize = FALSE,
      focal = focal_use
    )

  if (length(weightit_res$weights) != nrow(data_for_weight) ||
      any(!is.finite(weightit_res$weights)) ||
      any(weightit_res$weights <= 0)) {
    stop("IPWウェイトに欠損・無限大・0以下の値があります。傾向スコアの重なりを確認してください。")
  }

  data_for_weight <- data_for_weight %>%
    dplyr::mutate(
      ps_weight = weightit_res$weights,
      total_weight = ps_weight # component_weightを外したバージョン
      # total_weight = ps_weight * (!!as.name(s_weight))  # ★旧仕様：component_weightを掛ける
    )

  ## 分析用のdataframeにweightを結合
  df_ana <- df %>%
    dplyr::left_join(
      data_for_weight %>%
        dplyr::select(dplyr::all_of(c(id, "total_weight"))),
      by = id
    )

  if (is.na(df_ana$total_weight) %>% sum() > 0) {
    lift <- data.frame(message = "ウェイトが紐づかないサンプルが存在します.")
  } else {
    lift <- df_ana %>%
      dplyr::group_by(!!as.name(treatflg), period) %>%
      dplyr::summarise(
        N = dplyr::n(),
        WB_N = sum(total_weight),
        value = sum(!!as.name(outcome) * total_weight) * value_multiplier / WB_N,
        .groups = "drop"
      ) %>%
      dplyr::mutate(period = dplyr::case_when(
        period == 0 ~ "前期間",
        period == 1 ~ "後期間",
        TRUE ~ NA_character_
      )) %>%
      tidyr::pivot_wider(names_from = "period", values_from = "value") %>%
      dplyr::mutate(`前後差` = `後期間` - `前期間`) %>%
      dplyr::mutate(`推計効果` = dplyr::if_else(
        !!as.name(treatflg) == focal_use,
        `前後差`[!!as.name(treatflg) == focal_use] - `前後差`[!!as.name(treatflg) != focal_use],
        NA_real_
      ))%>%
      add_relative_lift(
        treatflg = treatflg,
        focal_use = focal_use
      )

    model_args <- list(
      yname = outcome,
      tname = "period",
      idname = id,
      dname = treatflg,
      xformla = weight_formula,
      #weightsname = s_weight,  # ★旧仕様：component_weightをDRDIDに渡す
      panel = TRUE,
      data = df
    )

    model <- do.call(DRDID::ipwdid, model_args)

    manual_att <- lift$推計効果[!is.na(lift$推計効果)]
    model_att <- model$ATT * value_multiplier

    ## DRDID パッケージによる計算結果と概ね一致していた場合のみ結果を表示
    if (length(manual_att) == 1 && isTRUE(all.equal(as.numeric(manual_att), as.numeric(model_att), tolerance = 1e-6))) {
      low_80ci <- (model_att - qnorm(0.9) * (model$se * value_multiplier)) %>% round(2)
      upp_80ci <- (model_att + qnorm(0.9) * (model$se * value_multiplier)) %>% round(2)

      lift <- lift %>%
        dplyr::mutate(
          `信頼区間_80%下限` = dplyr::if_else(!!as.name(treatflg) == focal_use, low_80ci, NA_real_),
          `信頼区間_80%上限` = dplyr::if_else(!!as.name(treatflg) == focal_use, upp_80ci, NA_real_)
        ) %>%
        dplyr::select(!!as.name(treatflg), N, WB_N, dplyr::everything())
    } else {
      lift <- data.frame(message = "推計効果に手法によりズレがあります.")
    }
  }

  return(lift)
}

safe_hand_ipwdid <- function(
  df, 
  weight_formula, 
  s_weight, 
  focal = 1, 
  id, 
  treatflg, 
  outcome,
  value_multiplier = 100
  ) {
  tryCatch(
    {
      hand_ipwdid(
        df = df,
        weight_formula = weight_formula,
        s_weight = s_weight,
        focal = focal,
        id = id,
        treatflg = treatflg,
        outcome = outcome,
        value_multiplier = value_multiplier
      )
    },
    error = function(e) {
      data.frame(message = paste0("ERROR: ", conditionMessage(e)))
    }
  )
}

# ------------------------------------------------------------------------------
# 6. メイン関数
# ------------------------------------------------------------------------------
# 添付コードの did_with_ipw() の流れを維持し、KPI名・軸名・カテゴリ名だけ
# 設定から渡せるようにしています。
# ------------------------------------------------------------------------------

make_fq_contrast_nest <- function(workdata, seg_col, label_config) {
  control_label <- "0回（非接触者）"

  fq_map <- tibble::tibble(
    target_value = c("1～2回", "3～4回", "5～6回", "7回以上"),
    contrast_label = c(
      "0回（非接触者）vs.1～2回",
      "0回（非接触者）vs.3～4回",
      "0回（非接触者）vs.5～6回",
      "0回（非接触者）vs.7回以上"
    )
  )

  fq_map %>%
    dplyr::mutate(
      data = purrr::map(
        target_value,
        ~ workdata %>%
          dplyr::filter(
            !is.na(.data[[seg_col]]),
            as.character(.data[[seg_col]]) %in% c(control_label, .x)
          )
      ),
      軸 = label_axis(seg_col, label_config),
      カテゴリ = contrast_label
    ) %>%
    dplyr::select(軸, カテゴリ, data)
}

did_with_ipw <- function(
  set_df = set_df, 
  rawdata = rawdata, 
  kpi_label = NULL, 
  label_config = NULL,
  value_multiplier = 100
) {
  ## それぞれの区分に該当する列名をベクトル化

  id <- set_df %>%
    dplyr::filter(item == "id") %>%
    .$value
  treatflg <- set_df %>%
    dplyr::filter(item == "treatflg") %>%
    .$value
  kpi_pre <- set_df %>%
    dplyr::filter(item == "kpi_pre") %>%
    .$value
  kpi_post <- set_df %>%
    dplyr::filter(item == "kpi_post") %>%
    .$value
  cov_list <- set_df %>%
    dplyr::filter(item == "covariate") %>%
    .$value
  seg_list <- set_df %>%
    dplyr::filter(item == "segment") %>%
    .$value
  weight <- set_df %>%
    dplyr::filter(item == "weight") %>%
    .$value
  focal <- set_df %>%
    dplyr::filter(item == "focal") %>%
    .$value

  cov_list <- cov_list[!is.na(cov_list) & cov_list != "" & cov_list != "0"]
  seg_list <- seg_list[!is.na(seg_list) & seg_list != "" & seg_list != "0"]

  ## ウェイトが指定されない場合、1で代替
  ## ウェイトがないとき、weight行のitem は0の想定
  if (length(weight) == 0 || is.na(weight) || weight == 0 || weight == "0") {
    weight <- "component_weight"

    rawdata <- rawdata %>%
      dplyr::mutate(component_weight = 1)
  }

  if (is.null(kpi_label)) {
    kpi_label <- kpi_post
  }

  if (is.null(label_config)) {
    label_config <- list(
      axis_labels = c("SEX" = "性別", "AGE_GROUP" = "年代別"),
      category_labels = list("SEX" = c("1" = "男性(1)", "2" = "女性(2)")),
      axis_order = c("全体", "性別", "年代別"),
      category_order = c("全体", "男性(1)", "女性(2)")
    )
  }

  all_column <- c(id, treatflg, kpi_pre, kpi_post, cov_list, seg_list, weight) %>%
    unique()
  all_column <- all_column[!is.na(all_column) & all_column != "" & all_column != "0"]

  ## 利用する列のみ取得
  validate_cols_in_data(rawdata, all_column, "did_with_ipw rawdata")

  workdata <- rawdata %>%
    dplyr::select(dplyr::all_of(all_column))

  ## 共変量/セグメント変数については factor 型に変換
  ## KPI列と重複している列はpivot_longerの型崩れを避けるためfactor化から外す
  factor_cols <- setdiff(
    unique(c(cov_list, seg_list)),
    c(kpi_pre, kpi_post)
  )

  if (length(factor_cols) > 0) {
    workdata <- workdata %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::all_of(factor_cols)),
        ~ as.factor(.)
      )
  }

  ## KPIを縦持ちに
  ## プレ・ポストのKPIを横並びから縦並びにするイメージ
  workdata <- workdata %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(c(kpi_pre, kpi_post)),
      names_to = "period",
      values_to = "kpi"
    ) %>%
    dplyr::mutate(period = dplyr::if_else(period == kpi_post, 1, 0))

  data_list <- list()

  ## 全体分析用
  data_list[["ALL"]] <-
    workdata %>%
    dplyr::mutate(
      軸 = "全体",
      カテゴリ = "全体"
    ) %>%
    dplyr::group_by(軸, カテゴリ) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  ## 各セグメント変数で group 化し、各セグメントごとのデータセット作成
  for (seg_col in seg_list) {
    if (seg_col == "FQ_CATEGORY") { # FQ別の時だけFQカテゴリ呼んで分析
      data_list[[seg_col]] <- make_fq_contrast_nest(
        workdata = workdata,
        seg_col = seg_col,
        label_config = label_config
      )
      next
    }

    seg_col_n <- as.name(seg_col)

    data_nest <- workdata %>%
      ## AGE_GROUPなどでNAカテゴリを作らない
      dplyr::filter(!is.na(!!seg_col_n)) %>%
      dplyr::group_by(!!seg_col_n) %>%
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        ## nest後に、セグメント列をdata内へ戻す
        data = purrr::map2(
          data,
          !!seg_col_n,
          ~ dplyr::mutate(.x, !!rlang::sym(seg_col) := .y)
        ),

        ## 軸名を作成
        軸 = label_axis(seg_col, label_config),

        ## カテゴリ名を作成
        カテゴリ = purrr::map_chr(
          as.character(!!seg_col_n),
          ~ label_category(seg_col, .x, label_config)
        )
      ) %>%
      dplyr::select(軸, カテゴリ, data)

    data_list[[seg_col]] <- data_nest
  }

  ## 上記の各分析を縦積みに
  data_full <- do.call("rbind", data_list)

  ## formula を作成
  weight_formula <- stringr::str_c(cov_list, collapse = " + ") %>%
    stringr::str_c(paste0(treatflg, " ~ "), .) %>%
    stats::as.formula()

  data_full <- data_full %>%
    dplyr::mutate(
      result = purrr::map(.x = data, ~ safe_hand_ipwdid(
        df = .x,
        weight_formula = weight_formula,
        s_weight = weight,
        focal = focal,
        id = id,
        treatflg = treatflg,
        outcome = "kpi",
        value_multiplier = value_multiplier
      ))
    )

  ## 分析結果を縦積みにしてデータフレーム化＆出力綺麗にするためととのえる
  result_all <- data_full %>%
    dplyr::select(軸, カテゴリ, result) %>%
    tidyr::unnest(cols = result) %>%
    dplyr::mutate(
      kpi = kpi_label,
      軸順 = order_by_map(軸, label_config$axis_order),
      カテゴリ順 = order_by_map(カテゴリ, label_config$category_order)
    )

  if (treatflg %in% names(result_all)) {
    result_all <- result_all %>%
      dplyr::arrange(軸順, カテゴリ順, .data[[treatflg]])
  } else {
    result_all <- result_all %>%
      dplyr::arrange(軸順, カテゴリ順)
  }

  result_all <- result_all %>%
    dplyr::select(
      kpi,
      軸,
      カテゴリ,
      dplyr::everything(),
      -軸順,
      -カテゴリ順
    )

  return(result_all)
}

# ------------------------------------------------------------------------------
# 7. 出力整形
# ------------------------------------------------------------------------------

format_result_for_csv <- function(result, treat_col = "treat_flg") {
  # 数値は丸めたり文字列化したりせず、そのままCSVへ保存する。
  # 小数点以下の表示桁数は03_formatting.RのExcelセル書式で制御する。
  result
}

# ------------------------------------------------------------------------------
# 8. 案件 × KPI ループ
# ------------------------------------------------------------------------------

run_one_kpi <- function(project, rawdata, kpi, label_config, ctx) {
  cat("\n--- KPI計算開始: ", project$project_name, " / ", kpi$kpi_label, " ---\n", sep = "")

  set_df <- make_set_df(project, kpi)

  # KPIごとに倍率を決める
  # 率・フラグ系: 100
  # 金額・回数系: 1
  value_multiplier <- get_kpi_multiplier(kpi)

  result <- did_with_ipw(
    set_df = set_df,
    rawdata = rawdata,
    kpi_label = kpi$kpi_label,
    label_config = label_config,
    value_multiplier = value_multiplier
  )

  dataset_name <- if (!is.null(project$dataset_name) &&
                      !is.na(project$dataset_name) &&
                      project$dataset_name != "") {
    project$dataset_name
  } else {
    project$project_name
  }

  media_name <- get_project_media(project)
  kpi_label_value <- kpi$kpi_label

  if ("treat_flg" %in% names(result)) {
    result <- result %>%
      dplyr::mutate(
        media_contact_flg = dplyr::case_when(
          treat_flg == 1 ~ 1,
          treat_flg == 0 ~ 2,
          TRUE ~ NA_real_
        ),
        接触区分 = dplyr::case_when(
          treat_flg == 1 ~ "接触",
          treat_flg == 0 ~ "非接触",
          TRUE ~ NA_character_
        )
      )
  } else {
    result$media_contact_flg <- NA_real_
    result$接触区分 <- NA_character_
  }

  result <- result %>%
    dplyr::mutate(
      案件名 = .env$dataset_name,
      媒体 = .env$media_name,
      KPI = .env$kpi_label_value,
      .before = 1
    ) %>%
    dplyr::select(
      案件名,
      媒体,
      KPI,
      media_contact_flg,
      接触区分,
      dplyr::everything()
    )

  result_out <- format_result_for_csv(
    result = result,
    treat_col = "treat_flg"
  )

  project_result_dir <- file.path(ctx$result_dir, safe_name(project$project_name))
  dir.create(project_result_dir, recursive = TRUE, showWarnings = FALSE)

  out_csv_path <- file.path(
    project_result_dir,
    paste0(
      safe_name(project$output_filename_prefix),
      "_ipw_did_",
      safe_name(kpi$file_label),
      "_result.csv"
    )
  )

  readr::write_csv(
    result_out,
    file = out_csv_path,
    na = ""
  )

  out_rds_path <- file.path(
    ctx$rds_dir,
    paste0(project$project_id, "_", kpi$kpi_id, "_result.rds")
  )
  saveRDS(result, out_rds_path)

  cat("KPI計算完了: ", out_csv_path, "\n", sep = "")

  list(
    project_id = project$project_id,
    project_name = project$project_name,
    kpi_id = kpi$kpi_id,
    kpi_label = kpi$kpi_label,
    csv_path = out_csv_path,
    rds_path = out_rds_path
  )
}

run_project_calculation <- function(project, project_runtime, ctx) {
  prepared <- prepare_rawdata(project, project_runtime, ctx)
  rawdata <- prepared$rawdata
  label_config <- make_label_config(project)

  kpi_results <- lapply(project$kpis, function(kpi) {
    run_one_kpi(
      project = project,
      rawdata = rawdata,
      kpi = kpi,
      label_config = label_config,
      ctx = ctx
    )
  })

  names(kpi_results) <- vapply(project$kpis, function(kpi) kpi$kpi_id, FUN.VALUE = character(1))

  list(
    project_id = project$project_id,
    project_name = project$project_name,
    rawdata_rds_path = prepared$rawdata_rds_path,
    kpi_results = kpi_results
  )
}

run_calculation <- function(config, ctx, processing_results) {
  results <- lapply(config$projects, function(project) {
    project_runtime <- processing_results[[project$project_id]]

    if (is.null(project_runtime)) {
      stop("データ処理結果が見つかりません: ", project$project_id)
    }

    run_project_calculation(
      project = project,
      project_runtime = project_runtime,
      ctx = ctx
    )
  })

  names(results) <- vapply(config$projects, function(project) project$project_id, FUN.VALUE = character(1))

  saveRDS(results, file.path(ctx$rds_dir, "calculation_results.rds"))

  return(results)
}
