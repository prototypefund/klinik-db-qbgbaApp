#' qbgbaApp: A golem package from the "Klinik-DB" project, which provides
#' example analyses for the German "Qualitaetsberichte der Krankenhaeuser"
#' obtained from the "Gemeinsamer Bundesausschuss" (GBA).
#'
#' This package aims at ...
#'
#'
#' @docType package
#' @name qbgbaApp
#'
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr arrange
#' @importFrom dplyr n
#' @importFrom dplyr if_any
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom dplyr across
#' @importFrom dplyr rename
#' @importFrom dplyr if_else
#' @importFrom dplyr pull
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr fill
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @importFrom purrr map_dfc
#' @importFrom purrr map_dfr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @importFrom purrr map_lgl
#' @importFrom purrr map2_dfc
#' @importFrom purrr map2_chr
#' @importFrom purrr pmap_dfr
#' @importFrom purrr pluck
#' @importFrom purrr possibly
#' @importFrom purrr safely
#' @importFrom rlang is_empty
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#' @importFrom utils data
#' @import sf
#' @import leaflet
#' @import leafdown
#' @import bs4Dash
#' @import echarts4r
#' @import qbgbaExtraData
#'
NULL
utils::globalVariables("where")
utils::globalVariables("last_col")
utils::globalVariables("everything")

