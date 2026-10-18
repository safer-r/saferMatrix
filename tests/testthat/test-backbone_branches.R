# tests for the backbone branches shared by all saferMatrix functions:
# - function name block (the "::()" / ":::()" branch is unreachable for exported functions, marked # nocov)
# - management of NULL arguments
# - management of empty non NULL arguments
# - management of NA arguments
# - safer_check argument checking (including the function branch)
# - check of lib_path (non-character, non-existing dir, existing dir with trailing /)
# - check of the required functions (with safer_check = TRUE)
# - management of "" in arguments of mode character (where present)
# Each test also verifies the error text embedding (error_text argument)

## initialization of tests
# minimal valid calls for each function, used as the base for the invalid-argument variants
base_calls <- base::list(
    mat_rotate = base::quote(mat_rotate(data = base::matrix(data = 1:10, ncol = 2), lib_path = NULL, safer_check = FALSE, error_text = "")),
    mat_inv = base::quote(mat_inv(mat = base::matrix(data = c(2, 1, 1, 2), ncol = 2), lib_path = NULL, safer_check = FALSE, error_text = "")),
    mat_fill = base::quote(mat_fill(mat = base::matrix(data = c(1, 1, 1, 2, 0, 2, 3, 0, NA, 3, 0, 0, 5, 0, 0, 0), ncol = 4), empty_cell_string = 0, warn_print = FALSE, lib_path = NULL, safer_check = FALSE, error_text = "")),
    mat_num2color = base::quote(mat_num2color(mat1 = base::matrix(data = c(1/3, 1/3, 1/3, 1/7, 1/3, 1/3, 1/9, 1/3, 1/3, 1/7, 1/3, 1/3, 1/9, 1/3, 1/3, 1/3), ncol = 4), mat_hsv_h = TRUE, notch = 1, s = 1, v = 1, forced_color = NULL, lib_path = NULL, safer_check = FALSE, error_text = "")),
    mat_op = base::quote(mat_op(mat_list = base::list(base::matrix(data = c(1, 1, 1, 2), ncol = 2), base::matrix(data = c(1, 1, 1, 2), ncol = 2)), kind_of_operation = "+", lib_path = NULL, safer_check = FALSE, error_text = ""))
)
## end initialization of tests

# generic runner: evaluates a call with a modified argument, returns the error message or "NO_ERROR"
# BEWARE: cl[[nm]] <- NULL would REMOVE the argument from the call, and base::quote(expr = NULL) returns NULL: use cl[nm] <- base::list(NULL) which sets the argument with a NULL value
run_mod <- function(fn, mod_args){
    cl <- base_calls[[fn]]
    for(nm in base::names(x = mod_args)){
        cl[nm] <- base::list(mod_args[[nm]])
    }
    res <- base::tryCatch(
        expr = base::eval(expr = cl, envir = base::asNamespace(ns = "saferMatrix")),
        error = function(e){base::conditionMessage(e)}
    )
    base::ifelse(test = base::is.character(x = res), yes = res, no = "NO_ERROR")
}

testthat::test_that("management of NULL arguments (backbone)", {
    null_specs <- base::list(
        mat_rotate = "data",
        mat_inv = "mat",
        mat_fill = "mat",
        mat_num2color = "notch",
        mat_op = "kind_of_operation"
    )
    for(fn in base::names(x = null_specs)){
        msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(NULL), null_specs[[fn]]))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE NULL", fixed = TRUE), label = base::paste0(fn, ": NULL ", null_specs[[fn]]))
    }
})

testthat::test_that("management of empty non NULL arguments (backbone)", {
    empty_specs <- base::list(
        mat_rotate = "data",
        mat_inv = "mat",
        mat_fill = "mat",
        mat_num2color = "notch",
        mat_op = "kind_of_operation"
    )
    for(fn in base::names(x = empty_specs)){
        msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(base::logical(length = 0)), empty_specs[[fn]]))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE AN EMPTY NON NULL OBJECT", fixed = TRUE), label = base::paste0(fn, ": empty ", empty_specs[[fn]]))
    }
    # empty lib_path (all functions have lib_path in the empty-non-null list)
    for(fn in base::names(x = base_calls)){
        msg <- run_mod(fn = fn, mod_args = list(lib_path = base::character(length = 0)))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE AN EMPTY NON NULL OBJECT", fixed = TRUE), label = base::paste0(fn, ": empty lib_path"))
    }
})

testthat::test_that("management of NA arguments (backbone)", {
    na_specs <- base::list(
        mat_rotate = "data",
        mat_inv = "mat",
        mat_fill = "warn_print", # empty_cell_string is excluded from the generic NA rule by design (it can be NA)
        mat_num2color = "mat_hsv_h",
        mat_op = "kind_of_operation"
    )
    for(fn in base::names(x = na_specs)){
        msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(NA), na_specs[[fn]]))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE MADE OF NA ONLY", fixed = TRUE), label = base::paste0(fn, ": NA ", na_specs[[fn]]))
    }
    # NA-only list elements also caught: mat_list = list(NA, NA)
    msg <- run_mod(fn = "mat_op", mod_args = list(mat_list = base::list(NA, NA)))
    testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE MADE OF NA ONLY", fixed = TRUE), label = "mat_op: NA mat_list elements (element check)")
    # mat.fill mat arg with NA inside: passed (NA allowed in mat)
    testthat::expect_no_error(object = run_mod(fn = "mat_fill", mod_args = list(mat = base::matrix(data = c(1, 1, 1, 2, 0, 2, 3, 0, NA, 3, 0, 0, 5, 0, 0, 0), ncol = 4)))) # identical to base call: no error
})

testthat::test_that("safer_check argument checking (backbone)", {
    for(fn in base::names(x = base_calls)){
        msg <- run_mod(fn = fn, mod_args = list(safer_check = "BAD"))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE), label = base::paste0(fn, ": safer_check string"))
        # function branch: a function value is deparsed then reported
        msg2 <- run_mod(fn = fn, mod_args = list(safer_check = base::mean))
        testthat::expect_true(object = base::grepl(x = msg2, pattern = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE), label = base::paste0(fn, ": safer_check function"))
        testthat::expect_true(object = base::grepl(x = msg2, pattern = "mean", fixed = TRUE), label = base::paste0(fn, ": safer_check function deparsed"))
        # numeric vector: length != 1
        msg3 <- run_mod(fn = fn, mod_args = list(safer_check = base::c(TRUE, FALSE)))
        testthat::expect_true(object = base::grepl(x = msg3, pattern = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE), label = base::paste0(fn, ": safer_check length 2"))
        # integer: typeof integer rejected
        msg4 <- run_mod(fn = fn, mod_args = list(safer_check = 1L))
        testthat::expect_true(object = base::grepl(x = msg4, pattern = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE), label = base::paste0(fn, ": safer_check integer"))
    }
})

testthat::test_that("check of lib_path (backbone)", {
    for(fn in base::names(x = base_calls)){
        # safer_check must be TRUE for the lib_path block to run
        # non-character lib_path with safer_check = TRUE
        msg <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = 1))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS", fixed = TRUE), label = base::paste0(fn, ": lib_path numeric"))
        # function lib_path: deparsed then reported as non character
        msg2 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = base::mean))
        testthat::expect_true(object = base::grepl(x = msg2, pattern = "MUST BE A VECTOR OF CHARACTERS", fixed = TRUE), label = base::paste0(fn, ": lib_path function"))
        # non-existing directory
        msg3 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = "Z:/BAD_PATH_DOES_NOT_EXIST"))
        testthat::expect_true(object = base::grepl(x = msg3, pattern = "NOT EXIST", fixed = TRUE), label = base::paste0(fn, ": lib_path non existing"))
        # multiple paths: plural message
        msg4 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = base::c("Z:/BAD1", "Z:/BAD2")))
        testthat::expect_true(object = base::grepl(x = msg4, pattern = "PATHS", fixed = TRUE), label = base::paste0(fn, ": lib_path plural"))
        # existing directory: function must run without lib_path error
        tempo_dir <- base::tempdir()
        msg5 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = tempo_dir))
        testthat::expect_false(object = base::grepl(x = msg5, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path existing dir ok"))
        # lib_path = NULL under safer_check = TRUE: default .libPaths() branch
        msg6 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = NULL))
        testthat::expect_false(object = base::grepl(x = msg6, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path NULL ok"))
        # trailing / path is stripped by the .libPaths() call: no error
        msg7 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = paste0(base::tempdir(), "/")))
        testthat::expect_false(object = base::grepl(x = msg7, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path trailing slash ok"))
    }
})

testthat::test_that("check of the required functions and critical operators (backbone)", {
    for(fn in base::names(x = base_calls)){
        # the required-function check and the critical operator check both run under safer_check = TRUE with valid args
        testthat::expect_no_error(object = run_mod(fn = fn, mod_args = list(safer_check = TRUE)))
    }
})

testthat::test_that("management of empty string in character arguments (backbone)", {
    # functions having a "management of \"\" in arguments of mode character" section
    # mat_num2color: forced_color ; mat_op: kind_of_operation
    # kind_of_operation: "" first caught by the options arg_check ("" not in c("+", "-", "*")), so the "" section is reached only through the internal-error branch (nocov) for mat_op.
    msg <- run_mod(fn = "mat_num2color", mod_args = list(forced_color = ""))
    testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE), label = "mat_num2color: empty string forced_color")
    # mat_op: "" kind_of_operation caught by arg_check options
    msg2 <- run_mod(fn = "mat_op", mod_args = list(kind_of_operation = ""))
    testthat::expect_true(object = base::grepl(x = msg2, pattern = "MUST BE SOME OF", fixed = TRUE), label = "mat_op: empty string kind_of_operation caught by arg_check options")
})

testthat::test_that("arg with no default values and all-default calls (backbone)", {
    # missing no-default argument triggers the dedicated branch
    for(fn in base::names(x = base_calls)){
        cl <- base_calls[[fn]]
        # remove the first no-default argument from the call
        no_def <- base::switch(EXPR = fn, mat_rotate = "data", mat_inv = "mat", mat_fill = "mat", mat_num2color = "mat1", mat_op = "mat_list")
        cl[no_def] <- NULL
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = base::asNamespace(ns = "saferMatrix")),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "NO DEFAULT VALUE", fixed = TRUE), label = base::paste0(fn, ": missing ", no_def))
    }
    # all-default user arguments: arg_user_setting_eval <- NULL branch: a bare call with all defaults -> no-default arg error is reached after the NULL branch (the NULL branch must not fail)
    res2 <- base::tryCatch(
        expr = base::eval(expr = base::quote(expr = mat_fill(safer_check = FALSE, error_text = "")), envir = base::asNamespace(ns = "saferMatrix")),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res2, pattern = "NO DEFAULT VALUE", fixed = TRUE), label = "mat_fill: bare call reaches no-default after arg_user_setting_eval NULL branch")
    # full all-default success is covered in the per-function files
})

testthat::test_that("error_text embedding (backbone)", {
    # error_text is embedded in the error message: the arg_check error uses " INSIDE " (embed_error_text)
    msg <- run_mod(fn = "mat_rotate", mod_args = list(data = "BAD"))
    testthat::expect_true(object = base::grepl(x = msg, pattern = "ERROR INSIDE saferMatrix::mat_rotate()", fixed = TRUE), label = "mat_rotate: default error_text embedding")
    # custom error_text
    cl <- base_calls[["mat_rotate"]]
    cl$data <- base::quote(expr = "BAD")
    cl$error_text <- "MY CUSTOM CONTEXT:"
    res <- base::tryCatch(expr = base::eval(expr = cl, envir = base::asNamespace(ns = "saferMatrix")), error = function(e){base::conditionMessage(e)})
    testthat::expect_true(object = base::grepl(x = res, pattern = "ERROR INSIDE saferMatrix::mat_rotate()MY CUSTOM CONTEXT:", fixed = TRUE), label = "mat_rotate: custom error_text")
    # error_text = NULL converted to ""
    cl2 <- base_calls[["mat_rotate"]]
    cl2$data <- base::quote(expr = "BAD")
    cl2$error_text <- NULL
    res2 <- base::tryCatch(expr = base::eval(expr = cl2, envir = base::asNamespace(ns = "saferMatrix")), error = function(e){base::conditionMessage(e)})
    testthat::expect_true(object = base::grepl(x = res2, pattern = "ERROR INSIDE saferMatrix::mat_rotate().", fixed = TRUE), label = "mat_rotate: NULL error_text converted")
})

base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
