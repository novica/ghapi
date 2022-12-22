# don't run with CI, to avoid potential access conflicts.

if (interactive()) {
  ##### --- tests for listing repositories --- #####

  testthat::test_that("list_repositories works", {
    test <- list_repositories()
    expect_s3_class(test, "tbl_df")
    expect_s3_class(test, "data.frame")

  })

  ##### --- tests for listing collaborators --- #####

  testthat::test_that("list_collabs works with correct permissions", {
    owner <- "novica"
    repo <- "tidytuesdaypandas"
    test <- list_collabs(owner, repo, affiliation = "all")
    expect_s3_class(test, "data.frame")
    expect_s3_class(test, "tbl_df")

  })

  testthat::test_that("list_collabs fails with incorrect permissions", {
    owner <- "r-lib"
    repo <- "httr"
    expect_error(list_collabs(owner, repo, affiliation = "all"))

  })

}
