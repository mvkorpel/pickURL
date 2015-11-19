context("Regression tests")
test_regression <- function() {
    pu_url <- pick_urls("URL:foo.bar/a", need_scheme=FALSE, url_pattern="")
    test_that("\"URL:\" without following URL scheme is handled OK", {
        expect_identical(pu_url, "foo.bar/a")
    })
}
test_regression()
