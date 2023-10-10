######## fun_mat_inv() #### return the inverse of a square matrix


fun_mat_inv <- function(mat){
    # AIM
    # return the inverse of a square matrix when solve() cannot
    # ARGUMENTS:
    # mat: a square numeric matrix without NULL, NA, Inf or single case (dimension 1, 1) of 0
    # RETURN
    # the inversed matrix
    # REQUIRED PACKAGES
    # none
    # REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
    # fun_check()
    # EXAMPLES
    # mat1 = matrix(c(1,1,1,2,1,5,9,8,9), ncol = 3) ; fun_mat_inv(mat = mat1) # use solve()
    # mat1 = matrix(c(0,0,0,0,0,0,0,0,0), ncol = 3) ; fun_mat_inv(mat = mat1) # use the trick
    # mat1 = matrix(c(1,1,1,2,Inf,5,9,8,9), ncol = 3) ; fun_mat_inv(mat = mat1)
    # mat1 = matrix(c(1,1,1,2,NA,5,9,8,9), ncol = 3) ; fun_mat_inv(mat = mat1)
    # mat1 = matrix(c(1,2), ncol = 1) ; fun_mat_inv(mat = mat1)
    # mat1 = matrix(0, ncol = 1) ; fun_mat_inv(mat = mat1)
    # mat1 = matrix(2, ncol = 1) ; fun_mat_inv(mat = mat1)
    # DEBUGGING
    # mat = matrix(c(1,1,1,2,1,5,9,8,9), ncol = 3) # for function debugging
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    # end function name
    # required function checking
    if(length(utils::find("fun_check", mode = "function")) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    # argument checking
    # argument checking with fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = mat, class = "matrix", mode = "numeric", fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with fun_check()
    # argument checking without fun_check()
    if(ncol(mat) != nrow(mat)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT MUST BE A SQUARE MATRIX")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(mat %in% c(Inf, -Inf, NA))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT MUST BE A MATRIX WITHOUT Inf, -Inf OR NA")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(all(mat == 0L) & ncol(mat) == 1L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE OF 0")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end argument checking without fun_check()
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
    # main code
    if(any(grepl(x = try(solve(mat), silent = TRUE)[], pattern = "[Ee]rror"))){
        tempo <- svd(mat)
        val.critique <- which(tempo$d < 10^-8)
        Diag.mod <- diag(1 / tempo$d)
        for(i in val.critique){
            Diag.mod[i, i] <- 0
        }
        return(tempo$v %*% Diag.mod %*% t(tempo$u))
    }else{
        return(solve(mat))
    }
}