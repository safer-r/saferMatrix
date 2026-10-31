testthat::test_that("mat_fill()", {

    ## data argument values
    # sector geometry for a 4x4 matrix (from the diag_scan definitions):
    # topleft = cells with row+col <= n ; topright = cells with col > row ;
    # bottomright = cells with row+col >= n+2 ; bottomleft = cells with row > col
    mat_bl <- base::matrix(data = c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4) # mixed sectors
    mat_char <- base::matrix(data = c(1,1,1,2, "a",2,3,NA, "a","a",0,0, "a","a","a",0), ncol = 4)
    mat_2empty <- base::matrix(data = c(0,0,0,2, 0,0,3,0, 0,3,0,0, 5,0,0,0), ncol = 4) # topleft AND bottomright empty (0)
    mat_corner_na <- base::matrix(data = c(1,1,1,1, 1,1,1,0, 1,1,0,0, 1,0,0,NA), ncol = 4) # corner (4,4) NA: bottomright in both lists
    mat_1x1 <- base::matrix(data = c(0), ncol = 1)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(mat_fill(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(mat_fill(mat = mat_bl, lib_path = NULL, safer_check = TRUE, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferMatrix:::mat_fill()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing mat default, but the function_name block is executed first
    #### end function name

    ######## arg with no default values
    testthat::expect_error(mat_fill())
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(mat_fill(mat = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, empty_cell_string = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, warn_print = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(mat_fill(mat = base::logical(length = 0), lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, empty_cell_string = base::character(length = 0), lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, warn_print = base::logical(length = 0), lib_path = NULL, safer_check = TRUE, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(mat_fill(mat = NA, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, warn_print = NA, lib_path = NULL, safer_check = TRUE, error_text = ""))
    # empty_cell_string = NA is allowed by design (excluded from the generic NA rule)
    res_na <- mat_fill(mat = mat_bl, empty_cell_string = NA, warn_print = FALSE, lib_path = NULL, safer_check = FALSE, error_text = "")
    testthat::expect_equal(object = res_na$mat, expected = mat_bl) # no all-NA half in mat_bl: nothing filled
    testthat::expect_true(object = base::grepl(x = res_na$warn, pattern = "ZERO EMPTY HALF PART", fixed = TRUE))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(mat_fill(mat = mat_bl, lib_path = NULL, safer_check = 1, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, lib_path = NULL, safer_check = "TRUE", error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(mat_fill(mat = mat_bl, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, lib_path = 1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(mat_fill(mat = mat_bl, lib_path = "PATH_NOT_GOOD", safer_check = TRUE, error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(mat_fill(mat = mat_bl, lib_path = "PATH_NOT_GOOD", safer_check = FALSE, error_text = ""))
    testthat::expect_no_error(mat_fill(mat = mat_bl, lib_path = base::tempdir(), safer_check = TRUE, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(mat_fill(mat = mat_bl, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(mat_fill(mat = mat_bl, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    testthat::expect_error(mat_fill(mat = base::matrix(data = base::letters[1:4], ncol = 2), lib_path = NULL, safer_check = TRUE, error_text = "")) # character matrix without the empty string cells: caught by the "mat ARGUMENT MATRIX MUST HAVE CELLS WITH THE EMPTY STRING SPECIFIED IN empty_cell_string ARGUMENT" check (character matrices are supported, see the sector tests below)
    testthat::expect_error(mat_fill(mat = base::mean, lib_path = NULL, safer_check = TRUE, error_text = "")) # function rejected by arg_check
    testthat::expect_error(mat_fill(mat = base::expression(1), lib_path = NULL, safer_check = TRUE, error_text = "")) # expression rejected by arg_check
    testthat::expect_error(mat_fill(mat = base::as.factor(x = "a"), lib_path = NULL, safer_check = TRUE, error_text = "")) # factor rejected by arg_check
    testthat::expect_error(mat_fill(mat = mat_bl, warn_print = "TRUE", lib_path = NULL, safer_check = TRUE, error_text = "")) # warn_print must be logical
    testthat::expect_error(mat_fill(mat = mat_bl, warn_print = 1, lib_path = NULL, safer_check = TRUE, error_text = ""))
    # empty_cell_string multi-tempo group: only one combined error text
    testthat::expect_error(mat_fill(mat = mat_bl, empty_cell_string = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "empty_cell_string ARGUMENT MUST BE (1) A SINGLE NUMERIC VALUE, (2) A SINGLE CHARACTER STRING OR (3) NA", fixed = TRUE)
    testthat::expect_error(mat_fill(mat = mat_bl, empty_cell_string = base::c(0, 1), lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST BE (1) A SINGLE NUMERIC VALUE", fixed = TRUE)
    ######## end argument checking with arg_check()

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## other checkings
    testthat::expect_error(mat_fill(mat = base::matrix(data = 1:6, ncol = 2), lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST BE A SQUARE MATRIX", fixed = TRUE)
    testthat::expect_error(mat_fill(mat = base::matrix(data = base::c(TRUE, FALSE, TRUE, FALSE), ncol = 2), lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST BE A NUMERIC OR CHARACTER MATRIX", fixed = TRUE)
    testthat::expect_error(mat_fill(mat = mat_1x1, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE", fixed = TRUE)
    testthat::expect_error(mat_fill(mat = base::matrix(data = c(1,2,3,4), ncol = 2), empty_cell_string = 9, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST HAVE CELLS WITH THE EMPTY STRING", fixed = TRUE)
    testthat::expect_error(mat_fill(mat = base::matrix(data = c(1,2,3,4), ncol = 2), empty_cell_string = NA, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MUST HAVE CELLS WITH THE EMPTY STRING", fixed = TRUE) # no NA in the matrix
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # sector detection: each of the 4 sectors, then symmetrization
    # helper: build a 4x4 matrix filled with a value in a given sector and unique markers elsewhere
    build_sector_mat <- function(empty_val, sector){
        m <- base::outer(X = 1:4, Y = 1:4, FUN = function(r, c){
            if(sector == "topleft"){
                base::ifelse(test = r + c <= 4, yes = empty_val, no = base::paste0("V", r, "_", c))
            }else if(sector == "topright"){
                base::ifelse(test = c > r, yes = empty_val, no = base::paste0("V", r, "_", c))
            }else if(sector == "bottomright"){
                base::ifelse(test = r + c >= 6, yes = empty_val, no = base::paste0("V", r, "_", c))
            }else{
                base::ifelse(test = r > c, yes = empty_val, no = base::paste0("V", r, "_", c)) # bottomleft
            }
        })
        return(m)
    }
    # bottomleft with NA
    mat_na_half <- build_sector_mat(NA, "bottomleft")
    res_na2 <- mat_fill(mat = mat_na_half, empty_cell_string = NA, warn_print = TRUE, lib_path = NULL, safer_check = FALSE, error_text = "")
    testthat::expect_false(object = base::any(base::is.na(x = res_na2$mat), na.rm = FALSE)) # NA half filled
    testthat::expect_equal(object = res_na2$mat[3, 1], expected = res_na2$mat[1, 3]) # transpose symmetry: (r, c) <- (c, r)
    testthat::expect_equal(object = res_na2$mat[4, 1], expected = res_na2$mat[1, 4])
    testthat::expect_equal(object = res_na2$mat[4, 2], expected = res_na2$mat[2, 4])
    testthat::expect_equal(object = res_na2$mat[3, 2], expected = res_na2$mat[2, 3])
    testthat::expect_true(object = base::grepl(x = res_na2$warn, pattern = "EMPTY SECTOR DETECTED ON THE BOTTOMLEFT CORNER, FULL OF NA", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_na2$warn, pattern = "BOTTOMLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL", fixed = TRUE))
    # topleft with 0
    res_tl <- mat_fill(mat = build_sector_mat(0, "topleft"), empty_cell_string = 0, warn_print = TRUE, lib_path = NULL, safer_check = FALSE, error_text = "")
    testthat::expect_equal(object = res_tl$mat[1, 1], expected = res_tl$mat[4, 4])
    testthat::expect_equal(object = res_tl$mat[1, 2], expected = res_tl$mat[3, 4])
    testthat::expect_equal(object = res_tl$mat[1, 3], expected = res_tl$mat[2, 4])
    testthat::expect_false(object = base::any(res_tl$mat == 0, na.rm = TRUE)) # no 0 left
    testthat::expect_true(object = base::grepl(x = res_tl$warn, pattern = "EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_tl$warn, pattern = "TOPLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL", fixed = TRUE))
    # topright with "a" (character matrix)
    res_tr <- mat_fill(mat = build_sector_mat("a", "topright"), empty_cell_string = "a", warn_print = TRUE, lib_path = NULL, safer_check = FALSE, error_text = "")
    testthat::expect_false(object = base::any(res_tr$mat == "a", na.rm = TRUE))
    testthat::expect_equal(object = res_tr$mat[1, 2], expected = res_tr$mat[2, 1])
    testthat::expect_equal(object = res_tr$mat[1, 3], expected = res_tr$mat[3, 1])
    testthat::expect_equal(object = res_tr$mat[2, 4], expected = res_tr$mat[4, 2])
    testthat::expect_true(object = base::grepl(x = res_tr$warn, pattern = "EMPTY SECTOR DETECTED ON THE TOPRIGHT CORNER, FULL OF a", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_tr$warn, pattern = "TOPRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL", fixed = TRUE))
    # bottomright with 0
    res_br <- mat_fill(mat = build_sector_mat(0, "bottomright"), empty_cell_string = 0, warn_print = TRUE, lib_path = NULL, safer_check = FALSE, error_text = "")
    testthat::expect_false(object = base::any(res_br$mat == 0, na.rm = TRUE))
    testthat::expect_equal(object = res_br$mat[2, 4], expected = res_br$mat[1, 3]) # point-reflection symmetry: (r, c) <- (n+1-c, n+1-r)
    testthat::expect_equal(object = res_br$mat[3, 4], expected = res_br$mat[1, 2]) # cells on the anti-diagonal (r + c = n + 1) map to themselves and are not filled
    testthat::expect_equal(object = res_br$mat[4, 3], expected = res_br$mat[2, 1])
    testthat::expect_true(object = base::grepl(x = res_br$warn, pattern = "EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_br$warn, pattern = "BOTTOMRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL", fixed = TRUE))
    # corner NA case: sector detected as both empty and full -> error with the accumulated warning
    testthat::expect_error(mat_fill(mat = mat_corner_na, empty_cell_string = 0, warn_print = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "THE FUNCTION HAS DETECTED EMPTY AND NON EMPTY HALF PART IN THE SAME SECTOR: bottomright", fixed = TRUE)
    # sector whose last scanned diagonal (the corner cell) breaks the loop but the whole sector check fails: "DETECTED AS EMPTY, IS NOT?" error
    mat_notqu <- base::matrix(data = c(
        "9", "a", "a", "x",
        "a", "a", "x", "x",
        "a", "x", "x", "x",
        "x", "x", "x", "x"
    ), ncol = 4, byrow = TRUE)
    testthat::expect_error(mat_fill(mat = mat_notqu, empty_cell_string = "a", lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "THE TOPLEFT SECTOR, DETECTED AS EMPTY, IS NOT?", fixed = TRUE)
    # more than one empty half part -> error with the accumulated warnings
    testthat::expect_error(mat_fill(mat = mat_2empty, empty_cell_string = 0, warn_print = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "MORE THAN ONE EMPTY HALF PART", fixed = TRUE)
    testthat::expect_error(mat_fill(mat = mat_2empty, empty_cell_string = 0, warn_print = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""), regexp = "EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER", fixed = TRUE)
    # zero empty half part -> warning only, matrix returned unchanged
    mat_noempty <- base::matrix(data = c(1,2,3,4, 5,6,7,8, 9,10,11,12, 13,14,15,16), ncol = 4)
    res_ne <- mat_fill(mat = mat_noempty, empty_cell_string = 9, warn_print = TRUE, lib_path = NULL, safer_check = FALSE, error_text = "")
    testthat::expect_equal(object = res_ne$mat, expected = mat_noempty)
    testthat::expect_true(object = base::grepl(x = res_ne$warn, pattern = "ZERO EMPTY HALF PART", fixed = TRUE))
    # warn_print = FALSE: no warning emitted (returned warn is still filled)
    res_wp <- mat_fill(mat = build_sector_mat(0, "topleft"), empty_cell_string = 0, warn_print = FALSE, lib_path = NULL, safer_check = FALSE, error_text = "")
    testthat::expect_true(object = base::grepl(x = res_wp$warn, pattern = "TOPLEFT SECTOR HAS BEEN COMPLETED", fixed = TRUE))
    # options(warning.length) restored after execution
    ini_wl <- base::options()$warning.length
    testthat::expect_equal(object = base::options()$warning.length, expected = ini_wl)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    # output structure
    testthat::expect_equal(object = base::names(x = res_na2), expected = base::c("mat", "warn"))
    #### end output
    ## end tests

    ## other tests
    # error_text variants
    testthat::expect_error(mat_fill(mat = mat_1x1, lib_path = NULL, safer_check = TRUE, error_text = " IN P1::F1 INSIDE P2::F2."), regexp = "ERROR IN saferMatrix::mat_fill() IN P1::F1 INSIDE P2::F2.", fixed = TRUE)
    testthat::expect_error(mat_fill(mat = mat_1x1, lib_path = NULL, safer_check = TRUE, error_text = NULL), regexp = "ERROR IN saferMatrix::mat_fill().", fixed = TRUE)
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
