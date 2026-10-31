testthat::test_that("mat_op()", {

    ## data argument values
    mat1 <- base::matrix(data = c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = base::list(LETTERS[1:4], letters[1:2]))
    mat2 <- base::matrix(data = c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = base::list(LETTERS[1:4], letters[1:2]))
    mat3 <- base::matrix(data = c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = base::list(LETTERS[1:4], base::c(NA, NA)))
    mat4 <- base::matrix(data = c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = base::list(base::c("A1", "A2", "A3", "A4"), letters[1:2]))
    mat_noname1 <- base::matrix(data = c(1,2,3,4), ncol = 2)
    mat_noname2 <- base::matrix(data = c(5,6,7,8), ncol = 2)
    mat_noname3 <- base::matrix(data = c(9,10,11,12), ncol = 2)
    mat_rowname1 <- base::matrix(data = c(1,2,3,4), ncol = 2) ; base::rownames(x = mat_rowname1) <- base::c("r1", "r2")
    mat_rowname2 <- base::matrix(data = c(5,6,7,8), ncol = 2) ; base::rownames(x = mat_rowname2) <- base::c("r1", "r2")
    mat_colname1 <- base::matrix(data = c(1,2,3,4), ncol = 2) ; base::colnames(x = mat_colname1) <- base::c("A", "B")
    mat_colname2 <- base::matrix(data = c(5,6,7,8), ncol = 2) ; base::colnames(x = mat_colname2) <- base::c("A", "B")
    fun1 <- base::mean
    factor1 <- base::as.factor(x = "a")
    expr1 <- base::expression(1)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(mat_op(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferMatrix:::mat_op()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing mat_list default, but the function_name block is executed first
    #### end function name

    ######## arg with no default values
    testthat::expect_error(mat_op())
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(mat_op(mat_list = NULL, kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = NULL)) # error_text NULL converted to ""
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(mat_op(mat_list = base::list(), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = base::character(length = 0), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = base::character(length = 0), error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(mat_op(mat_list = NA, kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = NA)) # NA rejected (cannot be made of NA only)
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = base::list("a", "b"))) # list converted to "ab"
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = "TRUE", lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = fun1, lib_path = NULL, error_text = "")) # function deparsed then reported
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = base::tempdir(), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    testthat::expect_error(mat_op(mat_list = mat1, kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")) # matrix rejected (list required)
    testthat::expect_error(mat_op(mat_list = fun1, kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")) # function rejected by arg_check
    testthat::expect_error(mat_op(mat_list = expr1, kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")) # expression rejected by arg_check
    testthat::expect_error(mat_op(mat_list = factor1, kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")) # factor rejected by arg_check
    testthat::expect_no_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = FALSE, lib_path = NULL, error_text = "")) # safer_check = FALSE accepted (kind_of_operation "+" is among options)
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "BAD", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "/", safer_check = TRUE, lib_path = NULL, error_text = "")) # not among options
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    # the "" branch is unreachable (arg_check() options rejects "" first, and the not-character branch is nocov'd), so no test possible here
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## other checkings
    testthat::expect_error(mat_op(mat_list = base::list(mat1), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "AT LEAST 2 MATRICES", fixed = TRUE) # length < 2
    testthat::expect_error(mat_op(mat_list = base::list(mat1, "BAD"), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "ELEMENT 2 OF mat_list ARGUMENT MUST BE A NUMERIC MATRIX", fixed = TRUE)
    testthat::expect_error(mat_op(mat_list = base::list(mat1, fun1), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "ELEMENT 2 OF mat_list ARGUMENT MUST BE A NUMERIC MATRIX", fixed = TRUE)
    testthat::expect_error(mat_op(mat_list = base::list(mat1, factor1), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "ELEMENT 2 OF mat_list ARGUMENT MUST BE A NUMERIC MATRIX", fixed = TRUE) # factor rejected
    testthat::expect_error(mat_op(mat_list = base::list(mat1, base::matrix(data = 1:6, ncol = 3)), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "MATRIX 2 OF mat_list ARGUMENT MUST HAVE THE SAME DIMENSION (2 3) THAN THE MATRIX 1 IN mat_list (4 2)", fixed = TRUE)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # addition with identical names: names kept
    res1 <- mat_op(mat_list = base::list(mat1, mat2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = res1[1, 1], expected = mat1[1, 1] + mat2[1, 1])
    testthat::expect_true(object = base::is.na(x = res1[4, 2])) # 8 + NA = NA
    testthat::expect_equal(object = base::rownames(x = res1), expected = base::rownames(x = mat1))
    testthat::expect_equal(object = base::colnames(x = res1), expected = base::colnames(x = mat1))
    # addition of 3 matrices: cumulative operation
    res3 <- mat_op(mat_list = base::list(mat_noname1, mat_noname2, mat_noname3), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = res3[1, 1], expected = 1 + 5 + 9)
    testthat::expect_equal(object = res3[2, 2], expected = 4 + 8 + 12)
    # subtraction
    res_minus <- mat_op(mat_list = base::list(mat_noname2, mat_noname1), kind_of_operation = "-", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = res_minus[1, 1], expected = 5 - 1)
    testthat::expect_equal(object = res_minus[2, 2], expected = 8 - 4)
    # multiplication
    res_prod <- mat_op(mat_list = base::list(mat_noname1, mat_noname2), kind_of_operation = "*", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = res_prod[1, 1], expected = 1 * 5)
    testthat::expect_equal(object = res_prod[2, 2], expected = 4 * 8)
    # mat3 has colnames c(NA, NA) vs mat2 letters[1:2]: comp_2d gives same.col.name FALSE -> colnames dropped
    res_na_col <- mat_op(mat_list = base::list(mat2, mat3), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = base::colnames(x = res_na_col)) # colnames dropped because not identical
    testthat::expect_equal(object = base::rownames(x = res_na_col), expected = LETTERS[1:4])
    # mat4 has different row names -> rownames dropped
    res_diff_row <- mat_op(mat_list = base::list(mat2, mat2, mat4), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = base::rownames(x = res_diff_row)) # row names differ between mat2 and mat4
    # unnamed matrices: same.row.name and same.col.name NULL -> names of the first matrix kept only if all NULL, thus NULL here
    res_noname <- mat_op(mat_list = base::list(mat_noname1, mat_noname2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = base::rownames(x = res_noname))
    testthat::expect_null(object = base::colnames(x = res_noname))
    # identical rownames on both, no colnames: rownames kept (same.col.name NULL -> skip branch)
    res_rowname <- mat_op(mat_list = base::list(mat_rowname1, mat_rowname2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::rownames(x = res_rowname), expected = base::c("r1", "r2"))
    testthat::expect_null(object = base::colnames(x = res_rowname))
    # identical colnames on both, no rownames: colnames kept (same.row.name NULL -> skip branch)
    res_colname <- mat_op(mat_list = base::list(mat_colname1, mat_colname2), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = base::rownames(x = res_colname))
    testthat::expect_equal(object = base::colnames(x = res_colname), expected = base::c("A", "B"))
    #### end main code

    #### warning output
    #### end warning output

    #### output
    #### end output
    ## end tests

    ## other tests
    # error_text variants
    testthat::expect_error(mat_op(mat_list = base::list(mat1), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = " IN P1::F1 INSIDE P2::F2."), regexp = "ERROR IN saferMatrix::mat_op() IN P1::F1 INSIDE P2::F2.", fixed = TRUE)
    testthat::expect_error(mat_op(mat_list = base::list(mat1), kind_of_operation = "+", safer_check = TRUE, lib_path = NULL, error_text = NULL), regexp = "ERROR IN saferMatrix::mat_op().", fixed = TRUE)
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
