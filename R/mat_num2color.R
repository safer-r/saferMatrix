#' @title mat_num2color
#' @description
#' Convert a matrix made of numbers into a hexadecimal matrix for rgb colorization.
#' @param mat1 Matrix 1 of non negative numerical values that has to be colored (matrix class). NA allowed.
#' @param mat.hsv.h Single logical value. Is mat1 the h of hsv colors ? (if TRUE, mat1 must be between zero and 1). If FALSE, mat1 must be made of positive integer values without 0.
#' @param notch Single value between 0 and 1 to shift the successive colors on the hsv circle by + notch.
#' @param s S argument of hsv(). Single numeric value between 0 and 1.
#' @param v V argument of hsv(). Single numeric value between 0 and 1.
#' @param forced.color Must be NULL or hexadecimal color code or name given by colors(). The first minimal values of mat1 will be these colors. All the color of mat1 can be forced using this argument.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns
#' A list containing:
#' 
#' - $mat1.name: name of mat1
#' 
#' - $colored.mat: colors of mat1 in hexa
#' 
#' - $problem: logical. Is any colors of forced.color overlap the colors designed by the function. NULL if forced.color = NULL
#' 
#' - $text.problem: text when overlapping colors. NULL if forced.color = NULL or problem == FALSE
#' @examples
#' mat1 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; 
#' dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]) ; 
#' mat_num2color(mat1, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL)
#' @importFrom saferDev arg_check
#' @export
mat_num2color <- function(
        mat1, 
        mat.hsv.h = TRUE, 
        notch = 1, 
        s = 1, 
        v = 1, 
        forced.color = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # mat1 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]); mat.hsv.h = FALSE ; notch = 1 ; s = 1 ; v = 1 ; forced.color = c(grDevices::hsv(1,1,1), grDevices::hsv(0,0,0)) ; safer_check = TRUE # for function debugging
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
            "saferDev::arg_check"
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
        "mat1"
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
    tempo <- saferDev::arg_check(data = mat1, mode = "numeric", class = "matrix", na.contain = TRUE, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = mat.hsv.h, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = notch, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = s, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = v, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with saferDev::arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
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
        "mat1",
        "mat.hsv.h",
        "notch",  
        "s", 
        "v",
        # "forced.color", # inactivated because can be null 
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
    if(mat.hsv.h == TRUE & saferDev::arg_check(data = mat1, mode = "numeric", prop = TRUE, safer_check = FALSE)$problem == TRUE){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat1 ARGUMENT MUST BE A MATRIX OF PROPORTIONS SINCE THE mat.hsv.h ARGUMENT IS SET TO TRUE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! base::is.null(forced.color)){
        tempo <- saferDev::arg_check(data = forced.color, class = "character", safer_check = FALSE)
        if(base::any(tempo$problem == TRUE, na.rm = TRUE)){
            base::paste0("\n\n================\n\n", base::paste(tempo$text[tempo$problem], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! base::all(forced.color %in% grDevices::colors() | base::grepl(pattern = "^#", forced.color), na.rm = TRUE)){ # check that all strings of forced.color start by #
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: forced.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end argument checking without saferDev::arg_check()
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    problem <- NULL
    text.problem <- NULL
    mat1.name <- base::deparse(base::substitute(mat1))
    # change the scale of the plotted matrix
    if(mat.hsv.h == TRUE){
        if(base::any(base::min(mat1, na.rm = TRUE) < 0 | base::max(mat1, na.rm = TRUE) > 1, na.rm = TRUE)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat1 MUST BE MADE OF VALUES BETWEEN 0 AND 1 BECAUSE mat.hsv.h ARGUMENT SET TO TRUE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        if(base::any(mat1 - base::floor(mat1) > 0, na.rm = TRUE) | base::any(mat1 == 0L, na.rm = TRUE)){ # no need of isTRUE(all.equal()) because we do not require approx here but strictly 0, thus == is ok
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat1 MUST BE MADE OF INTEGER VALUES WITHOUT 0 BECAUSE mat.hsv.h ARGUMENT SET TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            mat1 <- mat1 / base::max(mat1, na.rm = TRUE)
        }
    }
    if(notch != 1){
        different.color <- base::unique(base::as.vector(mat1))
        different.color <- different.color[ ! base::is.na(different.color)]
        tempo.different.color <- different.color + base::c(0, base::cumsum(base::rep(notch, base::length(different.color) - 1)))
        tempo.different.color <- tempo.different.color - base::floor(tempo.different.color)
        if(base::any(base::duplicated(tempo.different.color) == TRUE, na.rm = TRUE)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DUPLICATED VALUES AFTER USING notch (", base::paste(tempo.different.color[base::duplicated(tempo.different.color)], collapse = " "), "). TRY ANOTHER notch VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(base::length(different.color) != base::length(tempo.different.color)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: LENGTH OF different.color (", base::paste(different.color, collapse = " "), ") DIFFERENT FROM LENGTH OF tempo.different.color (", base::paste(tempo.different.color, collapse = " "), ")")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            for(i in 1:base::length(different.color)){
                mat1[mat1 == different.color[i]] <- tempo.different.color[i] # no need of isTRUE(all.equal()) because different.color comes from mat1
            }
        }
    }
    if( ! base::is.null(forced.color)){
        hexa.values.to.change <- grDevices::hsv(base::unique(base::sort(mat1))[1:base::length(forced.color)], s, v)
    }
    mat1[ ! base::is.na(mat1)] <- grDevices::hsv(mat1[ ! base::is.na(mat1)], s, v)
    if( ! base::is.null(forced.color)){
        if(base::any(forced.color %in% mat1, na.rm = TRUE)){
            problem <- TRUE
            text.problem <- base::paste0("THE FOLLOWING grDevices::colors WHERE INTRODUCED USING forced.color BUT WHERE ALREADY PRESENT IN THE COLORED MATRIX :", base::paste(forced.color[forced.color %in% mat1], collapse = " "))
        }else{
            problem <- FALSE
        }
        for(i in 1:base::length(hexa.values.to.change)){
            if( ! base::any(mat1 == hexa.values.to.change[i], na.rm = TRUE)){# no need of isTRUE(all.equal()) because character
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE ", hexa.values.to.change[i], " VALUE FROM hexa.values.to.change IS NOT REPRESENTED IN mat1 : ", base::paste(base::unique(base::as.vector(mat1)), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                mat1[base::which(mat1 == hexa.values.to.change[i])] <- forced.color[i] # no need of isTRUE(all.equal()) because character
            }
        }
    }
    # output
    # warning output
    # end warning output
    output <- base::list(mat1.name = mat1.name, colored.mat = mat1, problem = problem, text.problem = text.problem)
    base::return(output)
    # end output
    # end main code
}