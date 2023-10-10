######## mat_rotate() #### 90° clockwise matrix rotation


#' @title mat_rotate
#' @description
#' 90° clockwise matrix rotation.
#' 
#' Applied twice, the function provide the mirror matrix, according to vertical and horizontal symmetry.
#' @param data Matrix (matrix class).
#' @returns The modified matrix.
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
#' obs <- matrix(1:10, ncol = 1) ; obs ; mat_rotate(obs)
#' obs <- matrix(LETTERS[1:10], ncol = 5) ; obs ; mat_rotate(obs)
#' @seealso The page pkgdown html.
#' @export
mat_rotate <- function(data){

    # DEBUGGING
    # data = matrix(1:10, ncol = 1)
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
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = data, class = "matrix", fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
    # main code
    for (i in 1:ncol(data)){data[,i] <- rev(data[,i])}
    data <- t(data)
    return(data)
}
