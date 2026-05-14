# R/grade-utils.R
#
# Grade-level utilities for PreK–12 education applications.
# These are domain-specific helpers used by school-history apps
# that build on reactablePlus. They are exported for convenience
# but are NOT required by the core config_table / editable_table
# infrastructure.

#' Grade choices and labels
#'
#' Constants for the 14-grade ladder from PreK through 12th.
#'
#' @return `gradeChoices()` returns a named character vector
#'   (values are two-char keys, names are display labels).
#'   `gradeLabelMap()` returns a named list mapping grade keys
#'   to display labels.
#'
#' @export
gradeChoices <- function() {
  c(
    "PreK" = "PK", "K" = "K",
    "1st"  = "01", "2nd"  = "02", "3rd"  = "03",
    "4th"  = "04", "5th"  = "05", "6th"  = "06",
    "7th"  = "07", "8th"  = "08", "9th"  = "09",
    "10th" = "10", "11th" = "11", "12th" = "12"
  )
}

#' @rdname gradeChoices
#' @export
gradeLabelMap <- function() {
  grades <- gradeChoices()
  stats::setNames(as.list(names(grades)), unname(grades))
}

#' Format a grade key into a display label
#'
#' @param key Character. Grade key, e.g. `"PK"`, `"K"`, `"01"` .. `"12"`.
#' @return Display label string, e.g. `"PreK"`, `"K"`, `"1st"`.
#' @noRd
grade_label <- function(key) {
  map <- gradeLabelMap()
  if (key %in% names(map)) map[[key]] else key
}

#' Grade ordering index
#'
#' Converts a grade key to a numeric index for comparison.
#' PK=0, K=1, 01=2, 02=3, ..., 12=13.
#'
#' @param key Character. One or more grade keys.
#' @return Integer vector.
#'
#' @examples
#' gradeIndex("PK")  # 0
#' gradeIndex("K")   # 1
#' gradeIndex("05")  # 6
#' gradeIndex("12")  # 13
#'
#' @export
gradeIndex <- function(key) {
  vapply(key, function(k) {
    if (k == "PK") return(0L)
    if (k == "K")  return(1L)
    as.integer(k) + 1L
  }, integer(1), USE.NAMES = FALSE)
}

#' Check if a grade falls within a school's grade range
#'
#' @param grade_key Character. The grade to check (e.g. `"05"`).
#' @param low_grade Character. The school's lowest grade (e.g. `"K"`).
#' @param high_grade Character. The school's highest grade (e.g. `"08"`).
#' @return Logical.
#'
#' @examples
#' gradeInRange("05", "K", "08")   # TRUE
#' gradeInRange("09", "K", "08")   # FALSE
#' gradeInRange("PK", "PK", "12")  # TRUE
#'
#' @export
gradeInRange <- function(grade_key, low_grade, high_grade) {
  gi <- gradeIndex(grade_key)
  lo <- gradeIndex(low_grade)
  hi <- gradeIndex(high_grade)
  gi >= lo & gi <= hi
}
