# Databricks notebook source
# ==============================================================================
# IPW-DID Pipeline
# 03_formatting.R
# ------------------------------------------------------------------------------
# 成型：02_results 配下に出力された全CSVを、1つのExcelにまとめます。
# INDEX / ALL_RESULTS / CSV別シートを作成します。
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Excel用ユーティリティ
# ------------------------------------------------------------------------------

make_safe_sheet_name <- function(x) {
  x <- stringr::str_replace_all(x, "[\\[\\]\\*\\?/\\\\:]", "_")
  x <- stringr::str_replace_all(x, "\\s+", "_")
  x <- stringr::str_sub(x, 1, 31)
  x
}

make_unique_sheet_names <- function(x) {
  base_names <- make_safe_sheet_name(x)

  out <- character(length(base_names))
  used <- character(0)

  for (i in seq_along(base_names)) {
    name_i <- base_names[i]

    if (!(name_i %in% used)) {
      out[i] <- name_i
      used <- c(used, name_i)
    } else {
      j <- 2
      repeat {
        suffix <- paste0("_", j)
        candidate <- paste0(
          stringr::str_sub(name_i, 1, 31 - nchar(suffix)),
          suffix
        )

        if (!(candidate %in% used)) {
          out[i] <- candidate
          used <- c(used, candidate)
          break
        }

        j <- j + 1
      }
    }
  }

  out
}

read_csv_for_excel_safe <- function(path) {
  tryCatch(
    {
      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE,
        guess_max = Inf,
        na = c("", "NA")
      )
    },
    error = function(e_utf8) {
      message("UTF-8で読み込み失敗。CP932で再読み込みします: ", path)

      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "CP932"),
        show_col_types = FALSE,
        guess_max = Inf,
        na = c("", "NA")
      )
    }
  )
}

get_relative_path <- function(paths, root_dir) {
  root_slash <- paste0(root_dir, "/")

  vapply(
    paths,
    function(path) {
      if (startsWith(path, root_slash)) {
        substring(path, nchar(root_slash) + 1)
      } else {
        basename(path)
      }
    },
    FUN.VALUE = character(1)
  )
}

# ------------------------------------------------------------------------------
# 1. Excel作成メイン
# ------------------------------------------------------------------------------

make_excel_from_result_csv <- function(ctx) {
  root_dir <- ctx$result_dir

  csv_paths <- list.files(
    path = root_dir,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  cat("\n===== Excel成型開始 =====\n")
  cat("検出CSV数: ", length(csv_paths), "\n", sep = "")

  if (length(csv_paths) == 0) {
    stop("CSVファイルが見つかりませんでした。result_dirを確認してください: ", root_dir)
  }

  relative_path <- get_relative_path(csv_paths, root_dir)
  path_parts <- strsplit(relative_path, "/", fixed = TRUE)

  csv_info <- tibble::tibble(
    csv_path = csv_paths,
    relative_path = relative_path,
    案件名 = vapply(path_parts, function(x) x[[1]], FUN.VALUE = character(1)),
    csvファイル名 = basename(csv_paths),
    csv名_拡張子なし = tools::file_path_sans_ext(csvファイル名)
  ) %>%
    dplyr::mutate(
      sheet_base = paste0(案件名, "_", csv名_拡張子なし),
      sheet_name = make_unique_sheet_names(sheet_base)
    )

  cat("\n===== 取り込み対象CSV =====\n")
  print(csv_info %>% dplyr::select(案件名, csvファイル名, sheet_name), n = Inf)

  wb <- openxlsx::createWorkbook()

  openxlsx::modifyBaseFont(
    wb,
    fontName = "Meiryo UI",
    fontSize = 10
  )

  header_style <- openxlsx::createStyle(
    fontName = "Meiryo UI",
    fontSize = 10,
    textDecoration = "bold",
    fgFill = "#70AD47",
    fontColour = "#FFFFFF",
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#D9EAD3"
  )

  index_sheet <- csv_info %>%
    dplyr::select(
      案件名,
      csvファイル名,
      sheet_name,
      csv_path
    )

  openxlsx::addWorksheet(wb, "INDEX", gridLines = TRUE)
  openxlsx::writeData(
    wb,
    sheet = "INDEX",
    x = index_sheet,
    headerStyle = header_style,
    withFilter = TRUE
  )
  openxlsx::freezePane(wb, sheet = "INDEX", firstActiveRow = 2)
  openxlsx::setColWidths(wb, sheet = "INDEX", cols = 1:ncol(index_sheet), widths = "auto")

  # ALL_RESULTSシート：すべてのCSVを縦結合します。
  all_results <- purrr::pmap_dfr(
    csv_info %>% dplyr::select(案件名, csvファイル名, sheet_name, csv_path),
    function(案件名, csvファイル名, sheet_name, csv_path) {
      read_csv_for_excel_safe(csv_path) %>%
        dplyr::mutate(
          案件名 = 案件名,
          csvファイル名 = csvファイル名,
          sheet_name = sheet_name,
          .before = 1
        )
    }
  )

  openxlsx::addWorksheet(wb, "ALL_RESULTS", gridLines = TRUE)
  openxlsx::writeData(
    wb,
    sheet = "ALL_RESULTS",
    x = all_results,
    headerStyle = header_style,
    withFilter = TRUE
  )
  openxlsx::freezePane(wb, sheet = "ALL_RESULTS", firstActiveRow = 2)
  openxlsx::setColWidths(wb, sheet = "ALL_RESULTS", cols = 1:ncol(all_results), widths = "auto")

  # CSVごとに個別シートも作成します。
  for (i in seq_len(nrow(csv_info))) {
    path_i <- csv_info$csv_path[i]
    sheet_i <- csv_info$sheet_name[i]

    cat("読み込み中: ", sheet_i, "\n", sep = "")

    df_i <- read_csv_for_excel_safe(path_i)

    openxlsx::addWorksheet(
      wb,
      sheetName = sheet_i,
      gridLines = TRUE
    )

    openxlsx::writeData(
      wb,
      sheet = sheet_i,
      x = df_i,
      headerStyle = header_style,
      withFilter = TRUE
    )

    openxlsx::freezePane(
      wb,
      sheet = sheet_i,
      firstActiveRow = 2
    )

    if (ncol(df_i) > 0) {
      openxlsx::setColWidths(
        wb,
        sheet = sheet_i,
        cols = 1:ncol(df_i),
        widths = "auto"
      )
    }
  }

  out_path <- file.path(
    ctx$excel_dir,
    paste0("最終アウトプット_統合_", ctx$timestamp, ".xlsx")
  )

  openxlsx::saveWorkbook(
    wb,
    file = out_path,
    overwrite = TRUE
  )

  saveRDS(
    list(
      excel_path = out_path,
      csv_info = csv_info
    ),
    file.path(ctx$rds_dir, "formatting_result.rds")
  )

  cat("\nExcel出力完了:\n")
  cat(out_path, "\n")

  return(out_path)
}

run_formatting <- function(config, ctx, calculation_results = NULL) {
  make_excel_from_result_csv(ctx)
}
