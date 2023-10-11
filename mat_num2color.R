######## mat_num2color() #### convert a numeric matrix into hexadecimal color matrix

# todo list check OK
# Check r_debugging_tools-v1.4.R 
# Check fun_test() 20201107 (see cute_checks.docx) 
# example sheet 
# check all and any OK
# -> clear to go Apollo
# -> transferred into the cute package

#' @title mat_num2color
#' @description
#' Convert a matrix made of numbers into a hexadecimal matrix for rgb colorization.
#' @param mat1 Matrix 1 of non negative numerical values that has to be colored (matrix class). NA allowed.
#' @param mat.hsv.h Single logical value. Is mat1 the h of hsv colors ? (if TRUE, mat1 must be between zero and 1). If FALSE, mat1 must be made of positive integer values without 0.
#' @param notch Single value between 0 and 1 to shift the successive colors on the hsv circle by + notch.
#' @param s S argument of hsv(). Must be between 0 and 1.
#' @param v V argument of hsv(). Must be between 0 and 1.
#' @param forced.color Must be NULL or hexadecimal color code or name given by colors(). The first minimal values of mat1 will be these colors. All the color of mat1 can be forced using this argument.
#' @returns
#' A list containing:
#' - $mat1.name: name of mat1
#' - $colored.mat: colors of mat1 in hexa
#' - $problem: logical. Is any colors of forced.color overlap the colors designed by the function. NULL if forced.color = NULL
#' - $text.problem: text when overlapping colors. NULL if forced.color = NULL or problem == FALSE
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
#' mat1 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; 
#' dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]) ; 
#' mat_num2color(mat1, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL)
#' @seealso The page pkgdown html.
#' @export
mat_num2color <- function(
        mat1, 
        mat.hsv.h = TRUE, 
        notch = 1, 
        s = 1, 
        v = 1, 
        forced.color = NULL
){
    # DEBUGGING
    # mat1 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]); mat.hsv.h = FALSE ; notch = 1 ; s = 1 ; v = 1 ; forced.color = c(hsv(1,1,1), hsv(0,0,0)) # for function debugging
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # required function checking
    req.function <- c(
        "fun_check"
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
        "mat1"
        
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
    tempo <- fun_check(data = mat1, mode = "numeric", class = "matrix", na.contain = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = mat.hsv.h, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = notch, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = s, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = v, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with fun_check()
    # argument checking without fun_check()
    if(mat.hsv.h == TRUE & fun_check(data = mat1, mode = "numeric", prop = TRUE)$problem == TRUE){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat1 ARGUMENT MUST BE A MATRIX OF PROPORTIONS SINCE THE mat.hsv.h ARGUMENT IS SET TO TRUE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! is.null(forced.color)){
        tempo <- fun_check(data = forced.color, class = "character")
        if(any(tempo$problem == TRUE, na.rm = TRUE)){
            paste0("\n\n================\n\n", paste(tempo$text[tempo$problem], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! all(forced.color %in% colors() | grepl(pattern = "^#", forced.color))){ # check that all strings of forced.color start by #
            tempo.cat <- paste0("ERROR IN ", function.name, ": forced.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors()")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end argument checking without fun_check()
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
        "mat1"
        # "mat.hsv.h", # inactivated because can be null 
        # "notch", # inactivated because can be null 
        # "s", # inactivated because can be null 
        # "v", # inactivated because can be null 
        # "forced.color", # inactivated because can be null 
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
    # end other checkings
    
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation
    
    # package checking
    # end package checking
    
    
    
    # main code
    problem <- NULL
    text.problem <- NULL
    mat1.name <- deparse(substitute(mat1))
    # change the scale of the plotted matrix
    if(mat.hsv.h == TRUE){
        if(any(min(mat1, na.rm = TRUE) < 0 | max(mat1, na.rm = TRUE) > 1, na.rm = TRUE)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": mat1 MUST BE MADE OF VALUES BETWEEN 0 AND 1 BECAUSE mat.hsv.h ARGUMENT SET TO TRUE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        if(any(mat1 - floor(mat1) > 0, na.rm = TRUE) | any(mat1 == 0L, na.rm = TRUE)){ # no need of isTRUE(all.equal()) because we do not require approx here but strictly 0, thus == is ok
            tempo.cat <- paste0("ERROR IN ", function.name, ": mat1 MUST BE MADE OF INTEGER VALUES WITHOUT 0 BECAUSE mat.hsv.h ARGUMENT SET TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            mat1 <- mat1 / max(mat1, na.rm = TRUE)
        }
    }
    if(notch != 1){
        different.color <- unique(as.vector(mat1))
        different.color <- different.color[ ! is.na(different.color)]
        tempo.different.color <- different.color + c(0, cumsum(rep(notch, length(different.color) - 1)))
        tempo.different.color <- tempo.different.color - floor(tempo.different.color)
        if(any(duplicated(tempo.different.color) == TRUE, na.rm = TRUE)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": DUPLICATED VALUES AFTER USING notch (", paste(tempo.different.color[duplicated(tempo.different.color)], collapse = " "), "). TRY ANOTHER notch VALUE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(length(different.color) != length(tempo.different.color)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF different.color (", paste(different.color, collapse = " "), ") DIFFERENT FROM LENGTH OF tempo.different.color (", paste(tempo.different.color, collapse = " "), ")")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            for(i in 1:length(different.color)){
                mat1[mat1 == different.color[i]] <- tempo.different.color[i] # no need of isTRUE(all.equal()) because different.color comes from mat1
            }
        }
    }
    if( ! is.null(forced.color)){
        hexa.values.to.change <- hsv(unique(sort(mat1))[1:length(forced.color)], s, v)
    }
    mat1[ ! is.na(mat1)] <- hsv(mat1[ ! is.na(mat1)], s, v)
    if( ! is.null(forced.color)){
        if(any(forced.color %in% mat1, na.rm = TRUE)){
            problem <- TRUE
            text.problem <- paste0("THE FOLLOWING COLORS WHERE INTRODUCED USING forced.color BUT WHERE ALREADY PRESENT IN THE COLORED MATRIX :", paste(forced.color[forced.color %in% mat1], collapse = " "))
        }else{
            problem <- FALSE
        }
        for(i in 1:length(hexa.values.to.change)){
            if( ! any(mat1 == hexa.values.to.change[i], na.rm = TRUE)){# no need of isTRUE(all.equal()) because character
                tempo.cat <- paste0("ERROR IN ", function.name, ": THE ", hexa.values.to.change[i], " VALUE FROM hexa.values.to.change IS NOT REPRESENTED IN mat1 : ", paste(unique(as.vector(mat1)), collapse = " "))
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                mat1[which(mat1 == hexa.values.to.change[i])] <- forced.color[i] # no need of isTRUE(all.equal()) because character
            }
        }
    }
    # output
    output <- list(mat1.name = mat1.name, colored.mat = mat1, problem = problem, text.problem = text.problem)
    return(output)
    # end output
    # end main code
}