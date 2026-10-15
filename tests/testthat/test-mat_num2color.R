testthat::test_that("mat_num2color()", {

    ## data argument values
    mat1 <- base::matrix(data = c(1,1,1,2, 5,2,3,NA, 5,5,1,1, 5,5,5,1), ncol = 4) ; base::dimnames(x = mat1) <- base::list(LETTERS[1:4], letters[1:4])
    mat2 <- base::matrix(data = c(1/3,1/3,1/3,1/7, 1/3,1/3,1/9,1/3, 1/3,1/7,1/3,1/3, 1/9,1/3,1/3,1/3), ncol = 4)
    mat_neg <- base::matrix(data = c(-1, 2), ncol = 1)
    mat0 <- base::matrix(data = c(0, 2), ncol = 1)
    mat_dec <- base::matrix(data = c(1.5, 2), ncol = 1)
    mat_gt1 <- base::matrix(data = c(2, 3), ncol = 1)
    mat_notchdup <- base::matrix(data = c(0.5, 1.0), ncol = 1)
    factor1 <- base::as.factor(x = "a")
    expr1 <- base::expression(x = 1)
    fun1 <- base::mean
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(mat_num2color(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferMatrix:::mat_num2color()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing mat1 default, but the function_name block is executed first
    #### end function name

    ######## arg with no default values
    testthat::expect_error(mat_num2color())
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(mat_num2color(mat1 = NULL, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = NULL, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = NULL, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = NULL, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = NULL, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(mat_num2color(mat1 = base::logical(length = 0), mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = base::logical(length = 0), notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = base::numeric(length = 0), s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = base::numeric(length = 0), v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = base::numeric(length = 0), forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = base::character(length = 0), lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(mat_num2color(mat1 = NA, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = NA, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = NA, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = NA, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = NA, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NA, lib_path = NULL, safer_check = TRUE, error_text = ""))
    # NA inside mat1 is allowed (with mat.hsv.h = FALSE since mat1 has values > 1)
    testthat::expect_no_error(mat_num2color(mat1 = mat1, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = 1, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = "TRUE", error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = fun1, error_text = "")) # function deparsed then reported
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = 1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = "PATH_NOT_GOOD", safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = base::tempdir(), safer_check = TRUE, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    testthat::expect_error(mat_num2color(mat1 = "BAD", mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # character rejected
    testthat::expect_error(mat_num2color(mat1 = fun1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # function rejected by arg_check
    testthat::expect_error(mat_num2color(mat1 = expr1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # expression rejected by arg_check
    testthat::expect_error(mat_num2color(mat1 = factor1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # factor rejected by arg_check
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = "TRUE", notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1.5, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # notch > 1 rejected by prop
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = -0.5, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")) # negative notch rejected by prop
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 2, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = -1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = 1, lib_path = NULL, safer_check = TRUE, error_text = "")) # forced.color numeric rejected
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = fun1, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = "", lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## other checkings
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = "NOTACOLOR", lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "HEXADECIMAL COLOR", fixed = TRUE)
    testthat::expect_error(mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = "NOTACOLOR", lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "grDevices::colors()", fixed = TRUE)
    testthat::expect_error(mat_num2color(mat1 = mat_gt1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST BE A MATRIX OF PROPORTIONS", fixed = TRUE)
    testthat::expect_error(mat_num2color(mat1 = mat_gt1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "SINCE THE mat.hsv.h ARGUMENT IS SET TO TRUE", fixed = TRUE)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # mat.hsv.h = TRUE: values already between 0 and 1
    res1 <- mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = res1$mat1.name, expected = "mat2")
    testthat::expect_equal(object = base::dim(x = res1$colored.mat), expected = base::c(4L, 4L))
    testthat::expect_true(object = base::all(base::grepl(x = res1$colored.mat[ ! base::is.na(x = res1$colored.mat)], pattern = "^#", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)))
    testthat::expect_true(object = base::is.na(x = res1$colored.mat[base::is.na(x = mat2) & base::is.na(x = res1$colored.mat)][1])) # NA preserved? no: NA positions are also colored
    testthat::expect_null(object = res1$problem)
    testthat::expect_null(object = res1$text.problem)
    # hsv consistency: h = 1/3, s = 1, v = 1 -> same as grDevices::hsv()
    testthat::expect_equal(object = res1$colored.mat[1, 1], expected = grDevices::hsv(h = 1/3, s = 1, v = 1, alpha = 1))
    # mat.hsv.h = FALSE: integer values rescaled by max
    res2 <- mat_num2color(mat1 = mat1, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = res2$colored.mat[1, 1], expected = grDevices::hsv(h = 1/5, s = 1, v = 1, alpha = 1)) # 1/5 = 1/max(5)
    # notch = 0.5: successive unique values shifted
    res3 <- mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 0.5, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_false(object = base::identical(x = res3$colored.mat, y = res1$colored.mat, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE))
    # s and v passed through to grDevices::hsv()
    res4 <- mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 0.5, v = 0.5, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = res4$colored.mat[1, 1], expected = grDevices::hsv(h = 1/3, s = 0.5, v = 0.5, alpha = 1))
    # forced.color: minima replaced, no overlap
    res5 <- mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = "snow", lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_false(object = res5$problem)
    testthat::expect_null(object = res5$text.problem)
    testthat::expect_equal(object = res5$colored.mat[base::which(x = mat2 == base::min(x = mat2, na.rm = TRUE))[1]], expected = "snow")
    # forced.color overlap: problem = TRUE and text.problem filled (hsv(h = 1/3) IS present in mat2, so it is also generated by the function then detected as already present)
    res6 <- mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = grDevices::hsv(h = 1/3, s = 1, v = 1, alpha = 1), lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_true(object = res6$problem)
    testthat::expect_true(object = base::grepl(x = res6$text.problem, pattern = "WHERE ALREADY PRESENT", fixed = TRUE))
    # notch duplication error
    testthat::expect_error(mat_num2color(mat1 = mat_notchdup, mat.hsv.h = TRUE, notch = 0.5, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "DUPLICATED VALUES AFTER USING notch", fixed = TRUE)
    # mat.hsv.h = FALSE errors: 0 and non-integer values
    testthat::expect_error(mat_num2color(mat1 = mat0, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST BE MADE OF INTEGER VALUES WITHOUT 0", fixed = TRUE)
    testthat::expect_error(mat_num2color(mat1 = mat_dec, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST BE MADE OF INTEGER VALUES WITHOUT 0", fixed = TRUE)
    # values between 0 and 1 with mat.hsv.h = TRUE: no rescale (1/9 is at mat2[3,2])
    res7 <- mat_num2color(mat1 = mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = res7$colored.mat[3, 2], expected = grDevices::hsv(h = 1/9, s = 1, v = 1, alpha = 1))
    # forced.color collapse error: hsv(h = 0) and hsv(h = 1) give the same color (#FF0000FF), so after the first forced.color replacement, the second hexa.values.to.change entry is no longer present in mat1
    mat_h01 <- base::matrix(data = c(0, 1), ncol = 1)
    testthat::expect_error(mat_num2color(mat1 = mat_h01, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = base::c("snow", "black"), lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "THE #FF0000FF VALUE FROM hexa.values.to.change IS NOT REPRESENTED IN mat1 : snow", fixed = TRUE)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    # output structure
    testthat::expect_equal(object = base::names(x = res1), expected = base::c("mat1.name", "colored.mat", "problem", "text.problem"))
    #### end output
    ## end tests

    ## other tests
    # error_text variants
    testthat::expect_error(mat_num2color(mat1 = "BAD", mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = " IN P1::F1 INSIDE P2::F2."), regexp = "ERROR INSIDE saferMatrix::mat_num2color() IN P1::F1 INSIDE P2::F2.", fixed = TRUE)
    testthat::expect_error(mat_num2color(mat1 = "BAD", mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL, lib_path = NULL, safer_check = TRUE, error_text = NULL), regexp = "ERROR INSIDE saferMatrix::mat_num2color().", fixed = TRUE)
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
