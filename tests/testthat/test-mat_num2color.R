test_that("mat_num2color function works correctly", {
    # Example datasets
    mat1 <- matrix(c(1,1,1,2, 5,2,3,NA, 5,5,1,1, 5,5,5,1), ncol = 4)
    mat2 <- matrix(c(1/3,1/3,1/3,1/7, 1/3,1/3,1/9,1/3, 1/3,1/7,1/3,1/3, 1/9,1/3,1/3,1/3), ncol = 4)
    
    # Test cases
    
    # Simple examples
    expect_no_error(mat_num2color(mat2))
    
    # Argument mat.hsv.h
    expect_no_error(mat_num2color(mat1, mat.hsv.h = FALSE))
    expect_no_error(mat_num2color(mat2, mat.hsv.h = TRUE))
    
    # Argument notch
    expect_no_error(mat_num2color(mat2, notch = 0.2))
    expect_no_error(mat_num2color(mat2, notch = 0.5))
    expect_no_error(mat_num2color(mat2, notch = 0.7))
    
    # Argument s
    expect_no_error(mat_num2color(mat2, s = 0.2))
    expect_no_error(mat_num2color(mat2, s = 0.5))
    expect_no_error(mat_num2color(mat2, s = 0.7))
    
    # Argument v
    expect_no_error(mat_num2color(mat2, v = 0.2))
    expect_no_error(mat_num2color(mat2, v = 0.5))
    expect_no_error(mat_num2color(mat2, v = 0.7))
    
    # Argument forced.color
    expect_no_error(mat_num2color(mat2, forced.color = "snow"))
    
    # All the arguments
    expect_no_error(mat_num2color(mat1, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL))
    expect_no_error(mat_num2color(mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL))
    
})