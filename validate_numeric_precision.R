# Validate that result CSV values retain their numeric precision.

args <- commandArgs(trailingOnly = TRUE)
code_dir <- if (length(args) > 0 && args[[1]] != "") args[[1]] else "."
rds_dir <- if (length(args) > 1 && args[[2]] != "") args[[2]] else NULL

PIPELINE_CODE_DIR <- normalizePath(code_dir, winslash = "/", mustWork = TRUE)
source(file.path(PIPELINE_CODE_DIR, "02_calculation.R"), encoding = "UTF-8")

validate_result_roundtrip <- function(result, label) {
  csv_result <- format_result_for_csv(result)
  numeric_cols <- names(result)[vapply(result, is.numeric, FUN.VALUE = logical(1))]

  if (!all(vapply(csv_result[numeric_cols], is.numeric, FUN.VALUE = logical(1)))) {
    stop(label, " の数値列がCSV出力前に文字列化されています。")
  }

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)
  readr::write_csv(csv_result, temp_csv, na = "")
  read_back <- readr::read_csv(temp_csv, show_col_types = FALSE, na = c("", "NA"))

  for (numeric_col in numeric_cols) {
    if (!numeric_col %in% names(read_back) || !is.numeric(read_back[[numeric_col]])) {
      stop(label, " の数値列がCSVから数値として読み戻せません: ", numeric_col)
    }

    if (!isTRUE(all.equal(
      as.numeric(read_back[[numeric_col]]),
      as.numeric(result[[numeric_col]]),
      tolerance = 1e-12,
      check.attributes = FALSE
    ))) {
      stop(label, " の数値精度がCSV往復で失われています: ", numeric_col)
    }
  }

  invisible(TRUE)
}

synthetic_result <- data.frame(
  N = c(10L, 20L),
  WB_N = c(9.876543210123, 20.123456789012),
  推計効果 = c(0.123456789012, NA_real_),
  相対リフト = c(-12.345678901234, NA_real_),
  stringsAsFactors = FALSE
)

validate_result_roundtrip(synthetic_result, "synthetic")
validated_rds <- 0L

if (!is.null(rds_dir)) {
  result_paths <- list.files(
    rds_dir,
    pattern = "_result\\.rds$",
    full.names = TRUE
  )

  if (length(result_paths) == 0) {
    stop("検証対象のresult RDSがありません: ", rds_dir)
  }

  for (result_path in result_paths) {
    validate_result_roundtrip(readRDS(result_path), basename(result_path))
  }

  validated_rds <- length(result_paths)
}

cat(
  "NUMERIC_PRECISION_OK: synthetic + ",
  validated_rds,
  " result RDS validated.\n",
  sep = ""
)
