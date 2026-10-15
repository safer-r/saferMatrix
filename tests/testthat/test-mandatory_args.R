# This file tests the "mandatory arg of safer-r functions" checking block present in every saferMatrix function
# This block fires only if the function is written without one of the mandatory formals (lib_path, safer_check, error_text for public functions)
# It cannot be triggered by user values: the test uses a temporary function built from the same body but with the mandatory formal removed from formals()
# No # nocov markers are used, and no R source files are modified: the block is actually executed by the tests

testthat::test_that("mandatory arg block of safer-r functions (public functions)", {

    ## data argument values
    # full calls with all formals except lib_path (the mandatory formal to drop)
    full_calls <- base::list(
        mat_rotate = base::quote(mat_rotate(data = base::matrix(data = 1:10, ncol = 2), safer_check = FALSE, error_text = "")),
        mat_inv = base::quote(mat_inv(mat = base::matrix(data = c(2, 1, 1, 2), ncol = 2), safer_check = FALSE, error_text = "")),
        mat_fill = base::quote(mat_fill(mat = base::matrix(data = c(1, 1, 1, 2, 0, 2, 3, 0, NA, 3, 0, 0, 5, 0, 0, 0), ncol = 4), warn.print = FALSE, safer_check = FALSE, error_text = "")),
        mat_num2color = base::quote(mat_num2color(mat1 = base::matrix(data = c(1/3, 1/3, 1/3, 1/7, 1/3, 1/3, 1/9, 1/3, 1/3, 1/7, 1/3, 1/3, 1/9, 1/3, 1/3, 1/3), ncol = 4), mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, safer_check = FALSE, error_text = "")),
        mat_op = base::quote(mat_op(mat.list = base::list(base::matrix(data = c(1, 1, 1, 2), ncol = 2), base::matrix(data = c(1, 1, 1, 2), ncol = 2)), kind.of.operation = "+", safer_check = FALSE, error_text = ""))
    )
    ## end data argument values

    ## initialization of tests
    # none
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## mandatory arg of safer-r functions
    for(fn in base::names(x = full_calls)){
        f <- base::get(x = fn, envir = base::asNamespace(ns = "saferMatrix"))
        fml <- base::formals(fun = f)
        fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
        tempo_env <- base::new.env(parent = base::environment(fun = f))
        tempo_env[[fn]] <- base::as.function(base::c(fml_dropped, base::list(base::body(fun = f))), envir = tempo_env)
        cl <- full_calls[[fn]]
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = tempo_env),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT", fixed = TRUE), label = base::paste0(fn, ": mandatory-args block message"))
        testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path reported as missing mandatory arg"))
        testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = base::paste0(fn, ": no error before the mandatory-args block"))
    }
    ######## end mandatory arg of safer-r functions

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # restore check: with all formals present, the block must NOT fire
    testthat::expect_no_error(object = saferMatrix::mat_rotate(data = base::matrix(data = 1:10, ncol = 2), safer_check = FALSE, lib_path = NULL, error_text = ""))
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
