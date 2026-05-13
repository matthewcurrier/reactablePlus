# R/merge_existing_data.R

#' Merge previously saved values onto the display data frame
#'
#' Left-joins \code{existing_data} onto \code{data_df} using the id column
#' declared in \code{row_spec}. Rows in \code{data_df} with no match in
#' \code{existing_data} receive \code{NA} for every col_spec column. Rows
#' in \code{existing_data} that are absent from \code{data_df} are silently
#' dropped.
#'
#' @param data_df       A tibble containing at least the id_col and display
#'   columns declared in \code{row_spec}.
#' @param existing_data A tibble of previously saved values (id_col + col_spec
#'   columns), or \code{NULL} for a blank start.
#' @param row_spec      A validated row_spec list.
#' @param col_spec      A validated col_spec list.
#'
#' @return A tibble with exactly the id_col, display_cols, and col_spec columns
#'   — in that order. One row per row in \code{data_df}.
#' @export
merge_existing_data <- function(data_df, existing_data, row_spec, col_spec) {
  id_col <- row_spec$id_col
  display_col_names <- purrr::map_chr(row_spec$display_cols, \(d) d$col_name)
  input_col_names <- purrr::map_chr(col_spec, \(s) s$col_name)

  # Keep only id + display columns from data_df
  base <- dplyr::select(data_df, dplyr::all_of(c(id_col, display_col_names)))

  if (is.null(existing_data) || nrow(existing_data) == 0L) {
    # No saved data — add NA columns for every input column.
    # df[[col]] <- NA avoids the := operator and works correctly for tibbles.
    result <- purrr::reduce(
      input_col_names,
      function(df, col) {
        df[[col]] <- NA
        df
      },
      .init = base
    )
  } else {
    # Keep only the columns from existing_data that are relevant
    existing_cols_to_join <- input_col_names[
      input_col_names %in% names(existing_data)
    ]
    existing_subset <- dplyr::select(
      existing_data,
      dplyr::all_of(c(id_col, existing_cols_to_join))
    )

    result <- dplyr::left_join(base, existing_subset, by = id_col)

    # Add NA columns for any input columns absent from existing_data
    missing_cols <- input_col_names[!input_col_names %in% names(result)]
    result <- purrr::reduce(
      missing_cols,
      function(df, col) {
        df[[col]] <- NA
        df
      },
      .init = result
    )
  }

  # Guarantee a consistent column order
  expected_cols <- c(id_col, display_col_names, input_col_names)
  dplyr::select(result, dplyr::all_of(expected_cols))
}
