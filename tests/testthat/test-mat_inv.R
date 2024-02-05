test_that("mat_inv function works correctly", {
    # Example datasets
    mat1 <- matrix(c(0,0,0,0,0,0,0,0,0), ncol = 3)
    mat2 <- matrix(c(1,1,1,2,Inf,5,9,8,9), ncol = 3)
    mat3 <- matrix(c(1,1,1,2,NA,5,9,8,9), ncol = 3)
    mat4 <- matrix(c(1,2), ncol = 1)
    mat5 <- matrix(0, ncol = 1)
    
    # Test cases
    
    # All the arguments
    expect_no_error(mat_inv(mat1))
    
    # Error examples
    expect_error(mat_inv(mat2), message = "The matrix contains Inf or NA values.")
    expect_error(mat_inv(mat3), message = "The matrix contains Inf or NA values.")
    expect_error(mat_inv(mat4), message = "The input matrix must be a square matrix.")
    expect_error(mat_inv(mat5), message = "The input matrix must be a non-zero square matrix.")
    
})