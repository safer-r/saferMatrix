test_that("mat_fill function works correctly", {
    # Example datasets
    mat1 <- matrix(c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4)
    mat2 <- matrix(c(1,1,1,2, "a",2,3,NA, "a","a",0,0, "a","a","a",0), ncol = 4)
    mat3 <- matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4)
    mat4 <- matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,0, 5,0,0,0), ncol = 4)
    
    # Test cases
    
    # Simple examples
    expect_no_error(mat_fill(mat1))
    expect_no_error(mat_fill(mat2))
    expect_no_error(mat_fill(mat3))
    
    # Argument: empty.cell.string
    expect_no_error(mat_fill(mat1, empty.cell.string = 0))
    expect_no_error(mat_fill(mat2, empty.cell.string = "a"))
    expect_no_error(mat_fill(mat3, empty.cell.string = 0))
    
    # Argument: warn.print
    expect_no_error(mat_fill(mat1, warn.print = TRUE))
    expect_no_error(mat_fill(mat2, warn.print = TRUE))
    expect_no_error(mat_fill(mat3, warn.print = TRUE))
    
    # All the arguments
    expect_no_error(mat_fill(mat1, empty.cell.string = 0, warn.print = TRUE))
    expect_no_error(mat_fill(mat2, empty.cell.string = "a", warn.print = TRUE))
    expect_no_error(mat_fill(mat3, empty.cell.string = 0, warn.print = TRUE))
    
    # Error examples
    expect_error(mat_fill(mat1, empty.cell.string = NA, warn.print = TRUE))
    expect_error(mat_fill(mat4, empty.cell.string = 0, warn.print = TRUE))
    
})
