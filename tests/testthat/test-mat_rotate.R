testthat::test_that("mat_rotate()", {

    ## data argument values
    mat1 <- base::matrix(data = c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4)
    mat2 <- base::matrix(data = c(1,1,1,2, "a",2,3,NA, "a","a",0,0, "a","a","a",0), ncol = 4)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(mat_rotate(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(mat_rotate(data = mat1, lib_path = NULL, safer_check = TRUE, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferMatrix:::mat_rotate()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data default, but the function_name block is executed first
    #### end function name

    ######## arg with no default values
    testthat::expect_error(mat_rotate())
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(mat_rotate(data = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(mat_rotate(data = base::logical(length = 0), lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(mat_rotate(data = NA, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(mat_rotate(data = mat1, lib_path = NULL, safer_check = 1, error_text = ""))
    testthat::expect_error(mat_rotate(data = mat1, lib_path = NULL, safer_check = "TRUE", error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(mat_rotate(data = mat1, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_rotate(data = mat1, lib_path = 1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_rotate(data = mat1, lib_path = "PATH_NOT_GOOD", safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(mat_rotate(data = mat1, lib_path = base::tempdir(), safer_check = TRUE, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(mat_rotate(data = mat1, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(mat_rotate(data = mat1, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    testthat::expect_error(mat_rotate(data = "BAD", lib_path = NULL, safer_check = TRUE, error_text = "")) # character rejected
    testthat::expect_error(mat_rotate(data = base::mean, lib_path = NULL, safer_check = TRUE, error_text = "")) # function rejected by arg_check
    testthat::expect_error(mat_rotate(data = base::expression(x = 1), lib_path = NULL, safer_check = TRUE, error_text = "")) # expression rejected by arg_check
    testthat::expect_error(mat_rotate(data = base::as.factor(x = "a"), lib_path = NULL, safer_check = TRUE, error_text = "")) # factor rejected by arg_check
    ######## end argument checking with arg_check()

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## other checkings
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # numeric matrix: full 90° clockwise rotation check
    mat1.rot <- mat_rotate(data = mat1)
    testthat::expect_equal(object = mat1.rot, expected = base::t(x = mat1[base::rev(x = base::seq_len(length.out = base::nrow(x = mat1))), , drop = FALSE]))
    testthat::expect_equal(object = base::dim(x = mat1.rot), expected = base::c(4L, 4L))
    testthat::expect_equal(object = base::sum(x = mat1.rot, na.rm = TRUE), expected = base::sum(x = mat1, na.rm = TRUE))
    testthat::expect_equal(object = base::sum(x = base::is.na(x = mat1.rot)), expected = base::sum(x = base::is.na(x = mat1)))
    # double rotation: mirror matrix
    testthat::expect_equal(object = mat_rotate(data = mat1.rot), expected = mat1[base::rev(x = base::seq_len(length.out = base::nrow(x = mat1))), base::rev(x = base::seq_len(length.out = base::ncol(x = mat1))), drop = FALSE])
    # character matrix with NA
    mat2.rot <- mat_rotate(data = mat2)
    testthat::expect_equal(object = base::mode(x = mat2.rot), expected = "character")
    testthat::expect_true(object = base::any(base::is.na(x = mat2.rot), na.rm = FALSE))
    testthat::expect_equal(object = mat2.rot[1, 1], expected = mat2[4, 1])
    testthat::expect_equal(object = mat2.rot[4, 4], expected = mat2[1, 4])
    # single column and single row matrices: a 4x1 becomes 1x4, a 1x4 becomes 4x1
    testthat::expect_equal(object = mat_rotate(data = base::matrix(data = 1:4, ncol = 1)), expected = base::matrix(data = c(4, 3, 2, 1), nrow = 1))
    testthat::expect_equal(object = mat_rotate(data = base::matrix(data = 1:4, nrow = 1)), expected = base::matrix(data = 1:4, ncol = 1))
    # matrix with dimnames: dimnames rotated
    matd <- base::matrix(data = 1:6, ncol = 2) ; base::dimnames(x = matd) <- base::list(base::c("r1", "r2", "r3"), base::c("A", "B"))
    matd.rot <- mat_rotate(data = matd)
    testthat::expect_equal(object = base::rownames(x = matd.rot), expected = base::c("A", "B"))
    testthat::expect_equal(object = base::colnames(x = matd.rot), expected = base::c("r1", "r2", "r3")) # t() keeps the original rownames as colnames in the same order (the content is reversed, not the names)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    #### end output
    ## end tests

    ## other tests
    # error_text variants
    testthat::expect_error(mat_rotate(data = "BAD", lib_path = NULL, safer_check = TRUE, error_text = " IN P1::F1 INSIDE P2::F2."), regexp = "ERROR INSIDE saferMatrix::mat_rotate() IN P1::F1 INSIDE P2::F2.", fixed = TRUE)
    testthat::expect_error(mat_rotate(data = "BAD", lib_path = NULL, safer_check = TRUE, error_text = NULL), regexp = "ERROR INSIDE saferMatrix::mat_rotate().", fixed = TRUE)
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
