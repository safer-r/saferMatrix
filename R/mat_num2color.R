#' @title mat_num2color
#' @description
#' Convert a matrix made of numbers into a hexadecimal matrix for rgb colorization.
#' @param mat1 Matrix 1 of non negative numerical values that has to be colored (matrix class). NA allowed.
#' @param mat_hsv_h Single logical value. Is mat1 the h of hsv colors ? (if TRUE, mat1 must be between zero and 1). If FALSE, mat1 must be made of positive integer values without 0.
#' @param notch Single numeric value between 0 and 1 to shift the successive colors on the hsv circle by + notch.
#' @param s Single numeric value between 0 and 1 (s argument of \code{grDevices::hsv()}).
#' @param v Single numeric value between 0 and 1 (v argument of \code{grDevices::hsv()}).
#' @param forced_color Must be NULL or hexadecimal color code or name given by \code{grDevices::colors()}. The first minimal values of mat1 will be these colors. All the color of mat1 can be forced using this argument.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A list containing:
#'
#' - $mat1.name: name of mat1
#'
#' - $colored.mat: colors of mat1 in hexa
#'
#' - $problem: logical. Is any colors of forced_color overlap the colors designed by the function. NULL if forced_color = NULL
#'
#' - $text.problem: text when overlapping colors. NULL if forced_color = NULL or problem == FALSE
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link
#' # because of a particular environment. Please, copy-paste in a local environment.
#' mat1 <- base::matrix(data = c(1,1,1,2, 5,2,3,NA, 5,5,1,1, 5,5,5,1), ncol = 4) ;
#' base::dimnames(x = mat1) <- base::list(LETTERS[1:4], letters[1:4]) ;
#' mat_num2color(mat1 = mat1, mat_hsv_h = FALSE, notch = 1, s = 1, v = 1, forced_color = NULL)
#' @importFrom saferDev arg_check
#' @importFrom grDevices colors
#' @importFrom grDevices hsv
#' @export
mat_num2color <- function(
        mat1,
        mat_hsv_h = TRUE,
        notch = 1,
        s = 1,
        v = 1,
        forced_color = NULL,
        lib_path = NULL,
        safer_check = TRUE,
        error_text = ""
){

    # DEBUGGING
    # mat1 = matrix(c(1,1,1,2, 5,2,3,NA, 5,5,1,1, 5,5,5,1), ncol = 4) ; dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]) ; mat_hsv_h = FALSE ; notch = 1 ; s = 1 ; v = 1 ; forced_color = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # for function debugging
    # mat1 = matrix(c(1,1,1,2, 5,2,3,NA, 5,5,1,1, 5,5,5,1), ncol = 4) ; dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]) ; mat_hsv_h = FALSE ; notch = 1 ; s = 1 ; v = 1 ; forced_color = c(grDevices::hsv(h = 1, s = 1, v = 1), grDevices::hsv(h = 0, s = 0, v = 0)) ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # for function debugging

    #### package name
    package_name <- "saferMatrix" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE)
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names,
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of arguments if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)),
        function_name,
        collapse = NULL,
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name,
        base::ifelse(test = error_text == "", yes = ".", no = error_text),
        "\n\n",
        collapse = NULL,
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name,
        base::ifelse(test = error_text == "", yes = ".", no = error_text),
        "\n\n",
        collapse = NULL,
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present in your main code
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start,
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(x = tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function. Covered by tests/testthat/test-mandatory_args.R, which builds a temporary function with the formal removed
        tempo_cat <- base::paste0(
            error_text_start,
            "FOLLOWING ARGUMENT",
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"),
            " MANDATORY IN SAFER-R FUNCTIONS:\n",
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    no_def_args <- base::c(
        "mat1"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start,
            "FOLLOWING ARGUMENT",
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"),
            " NO DEFAULT VALUE AND REQUIRE ONE:\n",
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <- base::c(
        "mat1",
        "mat_hsv_h",
        "notch",
        "s",
        "v"
        # "forced_color", # inactivated because can be NULL
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is mat_num2color's frame.
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start,
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"),
            " CANNOT BE NULL:\n",
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <- base::c(
        "mat1",
        "mat_hsv_h",
        "notch",
        "s",
        "v",
        "forced_color",
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval,
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                },
                simplify = TRUE,
                USE.NAMES = TRUE
            ),
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start,
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"),
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n",
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval,
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ),
                FUN = function(x){
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                },
                simplify = TRUE,
                USE.NAMES = TRUE
            ),
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start,
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"),
                " CANNOT BE MADE OF NA ONLY:\n",
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start,
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n",
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if(safer_check == TRUE){ # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code)
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
                if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                    lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
                }
                tempo_cat <- base::paste0(
                    error_text_start,
                    "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n",
                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
                tempo_log <- ! base::dir.exists(paths = lib_path)
                tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
                tempo_cat_b[tempo_cat_b == ""] <- "\"\""
                tempo_cat <- base::paste0(
                    error_text_start,
                    "THE DIRECTORY PATH",
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""),
                    " INDICATED IN THE lib_path ARGUMENT DO",
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"),
                    " NOT EXIST:\n",
                    base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    } # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code)
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check"
                , "grDevices::colors"
                , "grDevices::hsv"
                # end functions required in this code
                # internal functions required in this code
                , "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions
    # not required: the internal functions of saferDev are used through saferDev:::.base_op_check() (and saferDev:::.pack_and_function_check() above), which does not trigger the CRAN NOTE about ::: usage inside the CRAN-checking code of the saferDev package itself. Only ::: usage of the developed package own internal functions would trigger the NOTE.
    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debugging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    tempo <- saferDev::arg_check(data = mat1, class = c("matrix", "array"), typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = mat_hsv_h, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # notch, s and v must be a single numeric value between 0 and 1 (1 included): prop = TRUE covers the whole range, so a single arg_check() call is enough
    tempo <- saferDev::arg_check(data = notch, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = s, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = v, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = forced_color)){
        tempo <- saferDev::arg_check(data = forced_color, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    # optional section: remove the code if you do not want to check if arguments of mode character of your own function cannot contain ""
    tempo_arg <- base::c(
        "forced_color" # NULL allowed, checked by the is.null() condition below
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    if( ! base::is.null(x = forced_color)){
        tempo_log <- ! base::all(base::mode(x = forced_color) == "character", na.rm = TRUE)
        # nocov start
        # codecov inactivated because this branch is already covered by the arg_check() call above (typeof character), so it can never be reached with argument values.
        if(base::any(tempo_log, na.rm = TRUE)){
            # This check is here in case the developer has not correctly fill tempo_arg
            tempo_cat <- base::paste0(
                "INTERNAL ERROR IN THE BACKBONE PART OF ",
                intern_error_text_start,
                "IN THE SECTION \"management of \"\" in arguments of mode character\"\n",
                "THIS ARGUMENT IS",
                " NOT CLASS \"character\":\n",
                "forced_color",
                "\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\n.HERE, ARGUMENTS ARE TYPE:\n",
                "forced_color: \"", base::typeof(x = forced_color), "\"",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }else{
            tempo_log <- base::any(forced_color == "", na.rm = TRUE)
            if(base::any(tempo_log, na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    error_text_start,
                    "THIS ARGUMENT\n",
                    "forced_color",
                    "\nCANNOT CONTAIN EMPTY STRING \"\".",
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    if(mat_hsv_h == TRUE & saferDev::arg_check(data = mat1, class = c("matrix", "array"), typeof = NULL, mode = "numeric", length = NULL, prop = TRUE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem == TRUE){
        tempo_cat <- base::paste0(
            error_text_start,
            "mat1 ARGUMENT MUST BE A MATRIX OF PROPORTIONS SINCE THE mat_hsv_h ARGUMENT IS SET TO TRUE",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! base::is.null(x = forced_color)){
        if( ! base::all(forced_color %in% grDevices::colors() | base::grepl(pattern = "^#", x = forced_color, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE)){ # check that all strings of forced_color start by #
            tempo_cat <- base::paste0(
                error_text_start,
                "forced_color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    problem <- NULL
    text.problem <- NULL
    mat1.name <- base::deparse(expr = base::substitute(expr = mat1, env = base::environment(fun = NULL)), width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    # change the scale of the plotted matrix
    if(mat_hsv_h == TRUE){
        if(base::any(base::min(x = mat1, na.rm = TRUE) < 0 | base::max(x = mat1, na.rm = TRUE) > 1, na.rm = TRUE)){
            # nocov start
            # codecov inactivated because this branch is unreachable with argument values: the arg_check() call of the "other checkings" section (line below the backbone sections), guarded by mat_hsv_h == TRUE, uses prop = TRUE on mat1, which already ensures that all the values of mat1 are between 0 and 1 (inclusive); the all-NA case is separately excluded by the backbone NA-only rule. Hence, this min/max check can never report a problem
            tempo_cat <- base::paste0(
                error_text_start,
                "mat1 MUST BE MADE OF VALUES BETWEEN 0 AND 1 BECAUSE mat_hsv_h ARGUMENT SET TO TRUE",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }else{
        if(base::any(mat1 - base::floor(x = mat1) > 0, na.rm = TRUE) | base::any(mat1 == 0L, na.rm = TRUE)){ # no need of isTRUE(all.equal()) because we do not require approx here but strictly 0, thus == is ok
            tempo_cat <- base::paste0(
                error_text_start,
                "mat1 MUST BE MADE OF INTEGER VALUES WITHOUT 0 BECAUSE mat_hsv_h ARGUMENT SET TO FALSE",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            mat1 <- mat1 / base::max(x = mat1, na.rm = TRUE)
        }
    }
    if(notch != 1){
        different.color <- base::unique(x = base::as.vector(x = mat1, mode = "any"))
        different.color <- different.color[ ! base::is.na(x = different.color)]
        tempo.different.color <- different.color + base::c(0, base::cumsum(base::rep(x = notch, times = base::length(x = different.color) - 1L)))
        tempo.different.color <- tempo.different.color - base::floor(x = tempo.different.color)
        if(base::any(base::duplicated(x = tempo.different.color, incomparables = FALSE, fromLast = FALSE) == TRUE, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start,
                "DUPLICATED VALUES AFTER USING notch (", base::paste0(tempo.different.color[base::duplicated(x = tempo.different.color, incomparables = FALSE, fromLast = FALSE)], collapse = " ", recycle0 = FALSE), "). TRY ANOTHER notch VALUE",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)        }else if(base::length(x = different.color) != base::length(x = tempo.different.color)){
            # nocov start
            # codecov inactivated because this branch is unreachable: different.color and tempo.different.color are always of the same length by construction (cumsum of a length - 1 vector added to a length 1 initial value)
            tempo_cat <- base::paste0(
                error_text_start,
                "LENGTH OF different.color (", base::paste0(different.color, collapse = " ", recycle0 = FALSE), ") DIFFERENT FROM LENGTH OF tempo.different.color (", base::paste0(tempo.different.color, collapse = " ", recycle0 = FALSE), ")",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }else{
            for(i in 1:base::length(x = different.color)){
                mat1[mat1 == different.color[i]] <- tempo.different.color[i] # no need of isTRUE(all.equal()) because different.color comes from mat1
            }
        }
    }
    if( ! base::is.null(x = forced_color)){
        hexa.values.to.change <- grDevices::hsv(h = base::unique(x = base::sort(x = mat1, decreasing = FALSE, na.last = NA))[1:base::length(x = forced_color)], s = s, v = v, alpha = 1)
    }
    mat1[ ! base::is.na(x = mat1)] <- grDevices::hsv(h = mat1[ ! base::is.na(x = mat1)], s = s, v = v, alpha = 1)
    if( ! base::is.null(x = forced_color)){
        if(base::any(forced_color %in% mat1, na.rm = TRUE)){
            problem <- TRUE
            text.problem <- base::paste0("THE FOLLOWING grDevices::colors WHERE INTRODUCED USING forced_color BUT WHERE ALREADY PRESENT IN THE COLORED MATRIX :", base::paste0(forced_color[forced_color %in% mat1], collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        }else{
            problem <- FALSE
        }
        for(i in 1:base::length(x = hexa.values.to.change)){
            if( ! base::any(mat1 == hexa.values.to.change[i], na.rm = TRUE)){ # no need of isTRUE(all.equal()) because character
                # reachable branch: hexa.values.to.change is built from the minimal unique sorted values of mat1, but values can disappear from mat1 when a previous forced_color replacement has already modified them. This notably happens when hsv() collapses different hues onto the same color (e.g. h = 0 and h = 1 both give #FF0000FF), or when the same minimal value appears only once but two forced colors must replace it
                tempo_cat <- base::paste0(
                    error_text_start,
                    "THE ", hexa.values.to.change[i], " VALUE FROM hexa.values.to.change IS NOT REPRESENTED IN mat1 : ", base::paste0(base::unique(x = base::as.vector(x = mat1, mode = "any")), collapse = " ", recycle0 = FALSE),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                mat1[base::which(mat1 == hexa.values.to.change[i])] <- forced_color[i] # no need of isTRUE(all.equal()) because character
            }
        }
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    output <- base::list(mat1.name = mat1.name, colored.mat = mat1, problem = problem, text.problem = text.problem)
    base::return(output)
    #### end output
}

#### end of function
