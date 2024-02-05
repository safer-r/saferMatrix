test_that("mat_num2color function works correctly", {
    # Example datasets
    mat1 <- matrix(c(1,1,1,2, 5,2,3,NA, 5,5,1,1, 5,5,5,1), ncol = 4)
    mat2 <- matrix(c(1/3,1/3,1/3,1/7, 1/3,1/3,1/9,1/3, 1/3,1/7,1/3,1/3, 1/9,1/3,1/3,1/3), ncol = 4)
    
    # Test cases
    
    # Simple examples
    expect_s3_class(mat_num2color(mat2), "character")
    
    # Argument mat.hsv.h
    expect_s3_class(mat_num2color(mat1, mat.hsv.h = FALSE), "character")
    expect_s3_class(mat_num2color(mat2, mat.hsv.h = TRUE), "character")
    
    # Argument notch
    expect_s3_class(mat_num2color(mat2, notch = 0.2), "character")
    expect_s3_class(mat_num2color(mat2, notch = 0.5), "character")
    expect_s3_class(mat_num2color(mat2, notch = 0.7), "character")
    
    # Argument s
    expect_s3_class(mat_num2color(mat2, s = 0.2), "character")
    expect_s3_class(mat_num2color(mat2, s = 0.5), "character")
    expect_s3_class(mat_num2color(mat2, s = 0.7), "character")
    
    # Argument v
    expect_s3_class(mat_num2color(mat2, v = 0.2), "character")
    expect_s3_class(mat_num2color(mat2, v = 0.5), "character")
    expect_s3_class(mat_num2color(mat2, v = 0.7), "character")
    
    # Argument forced.color
    expect_s3_class(mat_num2color(mat2, forced.color = "snow"), "character")
    
    # All the arguments
    expect_s3_class(mat_num2color(mat1, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL), "character")
    expect_s3_class(mat_num2color(mat2, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL), "character")
    
})