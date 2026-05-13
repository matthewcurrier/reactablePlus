# test-make_input_id.R

describe("make_input_id()", {

  describe("given valid arguments", {

    it("returns a single character string", {
      result <- make_input_id(ns_prefix = "myns", row_id = 6L, col_name = "attendance")
      expect_type(result, "character")
      expect_length(result, 1L)
    })

    it("formats the ID as ns_prefix-row_id-col_name", {
      result <- make_input_id(ns_prefix = "myns", row_id = 6L, col_name = "attendance")
      expect_equal(result, "myns-6-attendance")
    })

    it("handles a character row_id", {
      result <- make_input_id(ns_prefix = "myns", row_id = "row_a", col_name = "suspensions")
      expect_equal(result, "myns-row_a-suspensions")
    })
  })

  describe("uniqueness across row_id values", {
    it("produces distinct IDs for each row_id in a set of grades", {
      ids <- purrr::map_chr(
        c(6L, 7L, 8L),
        \(row_id) make_input_id(ns_prefix = "myns", row_id = row_id, col_name = "attendance")
      )
      expect_equal(length(unique(ids)), 3L)
    })
  })

  describe("uniqueness across col_name values", {
    it("produces distinct IDs for each col_name in a col_spec", {
      col_names <- purrr::map_chr(valid_col_spec(), \(spec) spec$col_name)
      ids <- purrr::map_chr(
        col_names,
        \(col) make_input_id(ns_prefix = "myns", row_id = 6L, col_name = col)
      )
      expect_equal(length(unique(ids)), length(col_names))
    })
  })

  describe("uniqueness across ns_prefix values", {
    it("produces distinct IDs when the same row_id and col_name appear under different namespaces", {
      ids <- purrr::map_chr(
        c("module_a", "module_b"),
        \(ns) make_input_id(ns_prefix = ns, row_id = 6L, col_name = "attendance")
      )
      expect_equal(length(unique(ids)), 2L)
    })
  })

  describe("given invalid arguments", {
    it("throws an error when ns_prefix is not a string", {
      expect_error(
        make_input_id(ns_prefix = 1L, row_id = 6L, col_name = "attendance"),
        regexp = "ns_prefix"
      )
    })

    it("throws an error when col_name is not a string", {
      expect_error(
        make_input_id(ns_prefix = "myns", row_id = 6L, col_name = 99L),
        regexp = "col_name"
      )
    })

    it("throws an error when row_id is NA", {
      expect_error(
        make_input_id(ns_prefix = "myns", row_id = NA_integer_, col_name = "attendance"),
        regexp = "row_id"
      )
    })
  })
})
