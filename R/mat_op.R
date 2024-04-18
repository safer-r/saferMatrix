#' @title mat_op
#' @description
#' Assemble several matrices of same dimensions by performing by case operation. For instance, with kind.of.operation = "+", the function will add the value of all the case 1 (row1 & column1) of the matrices and put it in the case 1 of a new matrix M, according to the formula
#' 
#' \deqn{c_{ij} = \sum_{k=1}^{k=z}c_{ijk}}
#' c: case
#' 
#' i: row number
#' 
#' j: column number
#' 
#' k: matrix number
#' 
#' z: number of matrices
#' @param mat.list List of matrices.
#' @param kind.of.operation Either "+" (by case addition), "-" (by case subtraction) or "*" (by case multiplication).
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns The assembled matrix, with row and/or column names only if all the matrices have identical row/column names.
#' @examples
#' mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2) ; 
#' mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; 
#' mat_op(mat.list = list(mat1, mat2), kind.of.operation = "+")
#' @importFrom saferDev arg_check
#' @importFrom saferTool comp_2d
#' @export
mat_op <- function(
        mat.list, 
        kind.of.operation = "+",
        safer_check = TRUE
){
    # DEBUGGING
    # mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; mat.list = list(mat1, mat2) ; kind.of.operation = "+" ; safer_check = TRUE # for function debugging
    # mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(LETTERS[1:4], c(NA, NA))) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2])) ; mat.list = list(mat1, mat2) ; kind.of.operation = "*" ; safer_check = TRUE # for function debugging
    # package name
    package.name <- "saferMatrix"
    # end package name
    # function name
    ini <- base::match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
    )
    }
    # end critical operator checking
    
    # package checking
    # check of lib.path
    # end check of lib.path
    
    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check",
            "saferTool::comp_2d"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "mat.list"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument checking with saferDev::arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = mat.list, class = "list", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = kind.of.operation, options = base::c("+", "-", "*"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with saferDev::arg_check()
    
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "mat.list",
        "kind.of.operation",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # argument checking without saferDev::arg_check()
    if(base::length(mat.list) < 2){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat.list ARGUMENT MUST BE A LIST CONTAINING AT LEAST 2 MATRICES")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    for(i1 in 1:base::length(mat.list)){
        tempo <- saferDev::arg_check(data = mat.list[[i1]], class = "matrix", mode = "numeric", na.contain = TRUE, safer_check = FALSE)
        if(tempo$problem == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: ELEMENT ", i1, " OF mat.list ARGUMENT MUST BE A NUMERIC MATRIX")
            
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    ident.row.names <- TRUE
    ident.col.names <- TRUE
    for(i1 in 2:base::length(mat.list)){
        tempo <- saferTool::comp_2d(data1 = mat.list[[1]], data2 = mat.list[[i1]], safer_check = FALSE)
        if(tempo$same.dim == FALSE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: MATRIX ", i1, " OF mat.list ARGUMENT MUST HAVE THE SAME DIMENSION (", base::paste(base::dim(mat.list[[i1]]), collapse = " "), ") THAN THE MATRIX 1 IN mat.list (", base::paste(base::dim(mat.list[[1]]), collapse = " "), ")")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! base::is.null(tempo$same.row.name)){
            if(tempo$same.row.name != TRUE){ # != TRUE to deal with NA
                ident.row.names <- FALSE
            }
        }
        if( ! base::is.null(tempo$same.col.name)){
            if(tempo$same.col.name != TRUE){ # != TRUE to deal with NA
                ident.col.names <- FALSE
            }
        }
    }
    # end argument checking without saferDev::arg_check()
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    # output
    # warning output
    # end warning output
    output <- mat.list[[1]]
    for(i1 in 2:base::length(mat.list)){
        output <- base::get(kind.of.operation)(output, mat.list[[i1]]) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    base::dimnames(output) <- NULL
    if(ident.row.names == TRUE){
        base::rownames(output) <- base::rownames(mat.list[[1]])
    }
    if(ident.col.names == TRUE){
        base::colnames(output) <- base::colnames(mat.list[[1]])
    }
    base::return(output)
    # end output
    # end main code
}
