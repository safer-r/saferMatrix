test_that("mat_op function works correctly", {
    # Example datasets
    mat1 <- matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2]))
    mat2 <- matrix(c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2]))
    mat3 <- matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(LETTERS[1:4], c(NA, NA)))
    mat4 <- matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(c("A1", "A2", "A3", "A4"), letters[1:2]))
    
    # Test cases
    
    # Simple examples
    expect_s3_class(mat_op(list(mat1, mat2)), "matrix")
    expect_s3_class(mat_op(list(mat2, mat3)), "matrix")
    expect_s3_class(mat_op(list(mat2, mat2, mat4)), "matrix")
    
    # All the arguments
    expect_s3_class(mat_op(list(mat1, mat2), kind.of.operation = "*"), "matrix")
    expect_s3_class(mat_op(list(mat2, mat3), kind.of.operation = "-"), "matrix")
    expect_s3_class(mat_op(list(mat2, mat2, mat4), kind.of.operation = "+"), "matrix")
    
})