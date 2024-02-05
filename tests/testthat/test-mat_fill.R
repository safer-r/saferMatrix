test_that("mat_fill function works correctly", {
    # Example datasets
    mat1 <- matrix(c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4)
    mat2 <- matrix(c(1,1,1,2, "a",2,3,NA, "a","a",0,0, "a","a","a",0), ncol = 4)
    mat3 <- matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4)
    mat4 <- matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,0, 5,0,0,0), ncol = 4)
    
    # Test cases
    
    # Simple examples
    expect_s3_class(mat_fill(mat1), "matrix")
    expect_s3_class(mat_fill(mat2), "matrix")
    expect_s3_class(mat_fill(mat3), "matrix")
    
    # Argument: empty.cell.string
    expect_s3_class(mat_fill(mat1, empty.cell.string = 0), "matrix")
    expect_s3_class(mat_fill(mat2, empty.cell.string = "a"), "matrix")
    expect_s3_class(mat_fill(mat3, empty.cell.string = 0), "matrix")
    
    # Argument: warn.print
    expect_s3_class(mat_fill(mat1, warn.print = TRUE), "matrix")
    expect_s3_class(mat_fill(mat2, warn.print = TRUE), "matrix")
    expect_s3_class(mat_fill(mat3, warn.print = TRUE), "matrix")
    
    # All the arguments
    expect_s3_class(mat_fill(mat1, empty.cell.string = 0, warn.print = TRUE), "matrix")
    expect_s3_class(mat_fill(mat2, empty.cell.string = "a", warn.print = TRUE), "matrix")
    expect_s3_class(mat_fill(mat3, empty.cell.string = 0, warn.print = TRUE), "matrix")
    
    # Error examples
    expect_error(mat_fill(mat1, empty.cell.string = NA, warn.print = TRUE), message = "The 'empty.cell.string' argument must be of the same type as the matrix.")
    expect_error(mat_fill(mat4, empty.cell.string = 0, warn.print = TRUE), message = "The 'empty.cell.string' argument must be of the same type as the matrix.")
    
})
