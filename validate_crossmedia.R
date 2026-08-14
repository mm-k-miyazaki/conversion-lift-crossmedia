# Cross-media input/config validation (read-only)

args <- commandArgs(trailingOnly = TRUE)
code_dir <- if (length(args) > 0 && args[[1]] != "") args[[1]] else "."
PIPELINE_CODE_DIR <- normalizePath(code_dir, winslash = "/", mustWork = TRUE)

source(file.path(PIPELINE_CODE_DIR, "00_data_settings.R"), encoding = "UTF-8")

repair_input_names <- function(x) {
  empty_index <- which(is.na(x) | x == "")
  x[empty_index] <- paste0("...", empty_index)
  make.unique(x, sep = "_")
}

expected <- data.frame(
  project_id = c(
    "harekaze_netflix",
    "harekaze_tver",
    "harekaze_amazon",
    "mercari_netflix",
    "mercari_tver",
    "mercari_amazon",
    "mercari_tv"
  ),
  n = c(9478L, 4734L, 21932L, 4417L, 2537L, 12350L, 2983L),
  exposed = c(6144L, 175L, 653L, 2990L, 82L, 797L, 2136L),
  control = c(3334L, 4559L, 21279L, 1427L, 2455L, 11553L, 847L),
  stringsAsFactors = FALSE
)

expected_behavior <- data.frame(
  dataset_prefix = c("harekaze", "mercari"),
  method = c("zero_vs_positive", "zero_plus_tertile"),
  value_col = c("新プレ_購入回数", "新プレ_利用時間"),
  category_col = c("新プレ_購入回数_segment", "新プレ_利用時間_segment"),
  stringsAsFactors = FALSE
)

if (length(PIPELINE_CONFIG$projects) != nrow(expected)) {
  stop("案件数が7件ではありません: ", length(PIPELINE_CONFIG$projects))
}

results <- lapply(PIPELINE_CONFIG$projects, function(project) {
  data <- readr::read_csv(
    project$masterdata_path,
    show_col_types = FALSE,
    name_repair = repair_input_names,
    na = c("", "NA")
  )

  media_col <- project$masterdata$media_col
  media_value <- project$masterdata$media_value
  data <- data[as.character(data[[media_col]]) == media_value, , drop = FALSE]

  id_values <- data[[project$analysis_id_col]]
  study_values <- as.character(data[[project$contact$study_col]])
  own_values <- as.character(data[[project$own_media_flag]])
  generated_covariates <- vapply(
    project$category_specs,
    function(spec) spec$category_col,
    FUN.VALUE = character(1)
  )
  required_input_covariates <- setdiff(project$covariates, generated_covariates)
  behavior_expected <- expected_behavior[
    startsWith(project$project_id, expected_behavior$dataset_prefix),
    ,
    drop = FALSE
  ]

  if (nrow(behavior_expected) != 1 ||
      project$category_specs[[1]]$method != behavior_expected$method[[1]] ||
      project$category_specs[[1]]$value_col != behavior_expected$value_col[[1]] ||
      project$category_specs[[1]]$category_col != behavior_expected$category_col[[1]]) {
    stop(project$project_name, " の新プレ行動カテゴリ設定が期待値と一致しません。")
  }

  old_covariates <- c("プレ_購入回数_segment", "プレ_利用回数_segment")
  if (any(old_covariates %in% project$covariates)) {
    stop(project$project_name, " の共変量に旧6分割列が含まれています。")
  }

  missing_cols <- setdiff(
    c(
      project$analysis_id_col,
      project$contact$study_col,
      project$own_media_flag,
      required_input_covariates,
      vapply(project$category_specs, function(spec) spec$value_col, FUN.VALUE = character(1)),
      unlist(lapply(project$kpis, function(kpi) c(kpi$pre_col, kpi$post_col)))
    ),
    names(data)
  )

  if (length(missing_cols) > 0) {
    stop(project$project_name, " の必要列がありません: ", paste(missing_cols, collapse = ", "))
  }

  if (any(is.na(id_values)) || any(trimws(as.character(id_values)) == "")) {
    stop(project$project_name, " の分析IDに欠損があります。")
  }

  if (anyDuplicated(id_values) > 0) {
    stop(project$project_name, " で同一ID×媒体が重複しています。")
  }

  if (!setequal(unique(study_values), c("1", "2"))) {
    stop(project$project_name, " のmedia_contact_flgが1/2ではありません。")
  }

  if (any(is.na(own_values)) || any(own_values != study_values)) {
    stop(project$project_name, " で対象媒体フラグとmedia_contact_flgが一致しません。")
  }

  if (project$own_media_flag %in% project$covariates) {
    stop(project$project_name, " の共変量に対象媒体自身のフラグが含まれています。")
  }

  other_flag_values <- unlist(lapply(project$other_media_covariates, function(col) {
    as.character(data[[col]])
  }))

  if (any(is.na(other_flag_values)) || !all(other_flag_values %in% c("1", "2", "3"))) {
    stop(project$project_name, " の他媒体フラグに1/2/3以外または欠損があります。")
  }

  data.frame(
    project_id = project$project_id,
    n = nrow(data),
    exposed = sum(study_values == "1"),
    control = sum(study_values == "2"),
    duplicate_id_media = anyDuplicated(id_values),
    other_media_covariates = paste(project$other_media_covariates, collapse = "+"),
    stringsAsFactors = FALSE
  )
})

results <- do.call(rbind, results)
check <- merge(expected, results, by = "project_id", suffixes = c("_expected", "_actual"))

for (metric in c("n", "exposed", "control")) {
  expected_col <- paste0(metric, "_expected")
  actual_col <- paste0(metric, "_actual")

  if (any(check[[expected_col]] != check[[actual_col]])) {
    stop(metric, " が期待件数と一致しません。")
  }
}

print(results, row.names = FALSE)
cat("\nVALIDATION_OK: 7媒体、ID×媒体一意性、接触件数、共変量設定を確認しました。\n")
