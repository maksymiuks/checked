test_that("check_packages works as expected", {
  # WIP
  expect_no_error(check_packages(c(
    test_path("testing_pkgs", "exampleGood"),
    test_path("testing_pkgs", "exampleBad")
  ), n = 2L, repos = "https://cran.r-project.org/"))
})
