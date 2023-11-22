#' @title mat_inv
#' @description
#' Return the inverse of a square matrix when solve() cannot.
#' @param mat A square numeric matrix without NULL, NA, Inf or single case (dimension 1, 1) of 0.
#' @returns The inversed matrix.
#' @details
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' check()
#' 
#' @examples
#' mat1 = matrix(c(1,1,1,2,1,5,9,8,9), ncol = 3) ; mat_inv(mat = mat1) # use solve()
#' 
#' mat1 = matrix(c(0,0,0,0,0,0,0,0,0), ncol = 3) ; mat_inv(mat = mat1) # use the trick
#' 
#' mat1 = matrix(c(1,1,1,2,Inf,5,9,8,9), ncol = 3) ; mat_inv(mat = mat1)
#' 
#' mat1 = matrix(c(1,1,1,2,NA,5,9,8,9), ncol = 3) ; mat_inv(mat = mat1)
#' 
#' mat1 = matrix(c(1,2), ncol = 1) ; mat_inv(mat = mat1)
#' 
#' mat1 = matrix(0, ncol = 1) ; mat_inv(mat = mat1)
#' 
#' mat1 = matrix(2, ncol = 1) ; mat_inv(mat = mat1)
#' @seealso The page pkgdown html.
#' @export
mat_inv <- function(
        mat
){
    # DEBUGGING
    # mat = matrix(c(1,1,1,2,1,5,9,8,9), ncol = 3) # for function debugging
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    req.function <- c(
        "check"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
            tempo <- c(tempo, i1)
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nREQUIRED cute FUNCTION", ifelse(length(tempo) > 1, "S ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    
    # arg with no default values
    mandat.args <- c(
        "mat"
    )
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, collapse = ") | missing("), ")")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument primary checking
    # argument checking with fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = mat, class = "matrix", mode = "numeric", fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with fun_check()
    
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-c(
        "mat"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    if(ncol(mat) != nrow(mat)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT MUST BE A SQUARE MATRIX")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(any(mat %in% c(Inf, -Inf, NA))){ # no NA with %in%
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT MUST BE A MATRIX WITHOUT Inf, -Inf OR NA")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(all(mat == 0L, na.rm = TRUE) & ncol(mat) == 1L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE OF 0")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other checkings
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation
    # package checking
    # end package checking
    
    # main code
    if(any(grepl(x = try(solve(mat), silent = TRUE)[], pattern = "[Ee]rror"), na.rm = TRUE)){
        tempo <- svd(mat)
        val.critique <- which(tempo$d < 10^-8)
        Diag.mod <- diag(1 / tempo$d)
        for(i in val.critique){
            Diag.mod[i, i] <- 0
        }
        # output
        return(tempo$v %*% Diag.mod %*% t(tempo$u))
    }else{
        return(solve(mat))
    }
    # end output
    # end main code
}