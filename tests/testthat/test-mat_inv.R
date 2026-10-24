testthat::test_that("mat_inv()", {

    ## data argument values
    mat_ok <- base::matrix(data = c(2, 1, 1, 2), ncol = 2)
    mat_int <- base::matrix(data = c(2L, 1L, 1L, 2L), ncol = 2)
    mat_inf <- base::matrix(data = c(1, 1, 1, 2, Inf, 5, 9, 8, 9), ncol = 3)
    mat_na <- base::matrix(data = c(1, 1, 1, 2, NA, 5, 9, 8, 9), ncol = 3)
    mat_nonsq <- base::matrix(data = c(1, 2), ncol = 1)
    mat_single0 <- base::matrix(data = 0, ncol = 1)
    mat_sing <- base::matrix(data = c(1, 2, 2, 4), ncol = 2) # singular: svd fallback
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(mat_inv(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(mat_inv(mat = mat_ok, lib_path = NULL, safer_check = TRUE, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferMatrix:::mat_inv()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing mat default, but the function_name block is executed first
    #### end function name

    ######## arg with no default values
    testthat::expect_error(mat_inv())
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(mat_inv(mat = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(mat_inv(mat = base::logical(length = 0), lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(mat_inv(mat = NA, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(mat_inv(mat = mat_ok, lib_path = NULL, safer_check = 1, error_text = ""))
    testthat::expect_error(mat_inv(mat = mat_ok, lib_path = NULL, safer_check = "TRUE", error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(mat_inv(mat = mat_ok, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_inv(mat = mat_ok, lib_path = 1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_inv(mat = mat_ok, lib_path = "PATH_NOT_GOOD", safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(mat_inv(mat = mat_ok, lib_path = base::tempdir(), safer_check = TRUE, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(mat_inv(mat = mat_ok, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(mat_inv(mat = mat_ok, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    testthat::expect_error(mat_inv(mat = "BAD", lib_path = NULL, safer_check = TRUE, error_text = "")) # character rejected
    testthat::expect_error(mat_inv(mat = base::mean, lib_path = NULL, safer_check = TRUE, error_text = "")) # function rejected by arg_check
    testthat::expect_error(mat_inv(mat = base::expression(1), lib_path = NULL, safer_check = TRUE, error_text = "")) # expression rejected by arg_check
    testthat::expect_error(mat_inv(mat = base::as.factor(x = "a"), lib_path = NULL, safer_check = TRUE, error_text = "")) # factor rejected by arg_check
    ######## end argument checking with arg_check()

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## other checkings
    testthat::expect_error(mat_inv(mat = mat_inf), regexp = "MUST BE MADE OF NON INFINITE VALUES", fixed = TRUE)
    testthat::expect_error(mat_inv(mat = mat_na), regexp = "CONTAINS NA WHILE NOT AUTHORIZED", fixed = TRUE)
    testthat::expect_error(mat_inv(mat = mat_nonsq), regexp = "MUST BE A SQUARE MATRIX", fixed = TRUE)
    testthat::expect_error(mat_inv(mat = mat_single0), regexp = "CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE OF 0", fixed = TRUE)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # solve() pathway (well-conditioned matrix)
    inv <- mat_inv(mat = mat_ok)
    testthat::expect_equal(object = inv, expected = base::solve(a = mat_ok))
    testthat::expect_equal(object = mat_ok %*% inv, expected = base::diag(x = 1, nrow = 2, ncol = 2))
    # integer matrix: also solve() pathway
    inv_int <- mat_inv(mat = mat_int)
    testthat::expect_equal(object = inv_int, expected = base::solve(a = base::matrix(data = c(2, 1, 1, 2), ncol = 2)))
    # singular matrix: svd() fallback pathway
    inv_sing <- mat_inv(mat = mat_sing)
    tempo <- base::svd(x = mat_sing)
    Diag_mod <- base::diag(x = 1 / tempo$d)
    val_critique <- base::which(x = tempo$d < 10^-8)
    for(i in val_critique){
        Diag_mod[i, i] <- 0
    }
    testthat::expect_equal(object = inv_sing, expected = tempo$v %*% Diag_mod %*% base::t(x = tempo$u))
    # 1x1 non-zero matrix
    testthat::expect_equal(object = mat_inv(mat = base::matrix(data = 2, ncol = 1)), expected = base::matrix(data = 0.5, ncol = 1))
    #### end main code

    #### warning output
    #### end warning output

    #### output
    #### end output
    ## end tests

    ## other tests
    # error_text variants
    testthat::expect_error(mat_inv(mat = mat_nonsq, lib_path = NULL, safer_check = TRUE, error_text = " IN P1::F1 INSIDE P2::F2."), regexp = "ERROR IN saferMatrix::mat_inv() IN P1::F1 INSIDE P2::F2.", fixed = TRUE)
    testthat::expect_error(mat_inv(mat = mat_nonsq, lib_path = NULL, safer_check = TRUE, error_text = NULL), regexp = "ERROR IN saferMatrix::mat_inv().", fixed = TRUE)
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
