test_that("mat_rotate function works correctly", {
    # Example datasets
    mat1 <- matrix(c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4)
    mat2 <- matrix(c(1,1,1,2, "a",2,3,NA, "a","a",0,0, "a","a","a",0), ncol = 4)
    mat3 <- matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4)
    mat4 <- matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,0, 5,0,0,0), ncol = 4)
    
    # Test cases
    # All the arguments
    expect_no_error(mat_rotate(data = mat1))
    expect_no_error(mat_rotate(data = mat2))
    expect_no_error(mat_rotate(data = mat3))
    expect_no_error(mat_rotate(data = mat4))
    
})
