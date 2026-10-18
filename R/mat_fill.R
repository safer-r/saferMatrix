#' @title mat_fill
#' @description
#' Detect the empty half part of a symmetric square matrix (either topleft, topright, bottomleft or bottomright).
#'
#' Fill this empty half part using the other symmetric half part of the matrix.
#' @param mat A numeric or character square matrix with the half part (according to the grand diagonal) filled with NA (any kind of matrix), "0" (character matrix) or 0 (numeric matrix) exclusively (not a mix of 0 and NA in the empty part).
#' @param empty_cell_string A numeric, character or NA (no quotes) indicating what empty cells are filled with.
#' @param warn_print Single logical value. Print warnings at the end of the execution? No print if no warning messages.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A list containing:
#'
#' - $mat: The filled matrix.
#'
#' - $warn: The warning messages. Use cat() for proper display. NULL if no warning.
#' @details
#' WARNINGS
#'
#' A plot verification using gg_heatmap() is recommanded.
#'
#' 2x2 matrices are not supported: each sector being a single cell, the sector detection always ends with the "DETECTED AS EMPTY, IS NOT?" error, even for a valid single-empty-half matrix (pre-existing behavior, kept unchanged during the migration).
#'
#' The \code{empty_cell_string} argument accepts NA as only argument, which breaks the safer-r rule. The "management of NA arguments" section has been personalized accordingly and the code of this function has been checked so that \code{empty_cell_string} is not used in \code{if(empty_cell_string){}}.
#'
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link
#' # because of a particular environment. Please, copy-paste in a local environment.
#' # bottomleft example
#' mat1 <- matrix(data = c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ;
#' mat1 ;
#' mat_fill(mat = mat1, empty_cell_string = NA, warn_print = TRUE)
#' @importFrom saferDev arg_check
#' @export
mat_fill <- function(
        mat,
        empty_cell_string = 0,
        warn_print = FALSE,
        lib_path = NULL,
        safer_check = TRUE,
        error_text = ""
){

    # DEBUGGING
    # mat = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; empty_cell_string = NA ; warn_print = TRUE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # for function debugging
    # mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; empty_cell_string = 0 ; warn_print = TRUE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # for function debugging # topleft example
    # mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,0, 5,0,0,0), ncol = 4) ; empty_cell_string = 0 ; warn_print = TRUE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # for function debugging # zero empty half part example

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
        "mat"
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
        "mat",
        "empty_cell_string",
        "warn_print"
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is mat_fill's frame.
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
        "mat",
        "empty_cell_string",
        "warn_print",
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
    # personalized here: empty_cell_string can be NA by design (see the empty_cell_string argument documentation and the arg_check() group below)
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
        ) # no argument provided by the user can be just made of NA, except empty_cell_string. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        tempo_log[base::names(x = tempo_log) == "empty_cell_string"] <- FALSE # empty_cell_string can be NA by design (excluded from the generic NA rule)
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
    tempo <- saferDev::arg_check(data = mat, class = c("matrix", "array"), typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = warn_print, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # empty_cell_string can be (1) a single numeric value, (2) a single character string or (3) NA: NA is excluded from the arg_check() calls below because saferDev::arg_check() rejects NA-only values in its own backbone NA management (before na_contain applies), so the NA case is accepted by design here and checked separately. Length check first to avoid a length > 1 condition in if()
    if( ! (base::length(x = empty_cell_string) == 1L && base::is.na(x = empty_cell_string))){
        tempo1 <- saferDev::arg_check(data = empty_cell_string, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
        tempo2 <- saferDev::arg_check(data = empty_cell_string, class = NULL, typeof = NULL, mode = "character", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text,
                "empty_cell_string ARGUMENT MUST BE (1) A SINGLE NUMERIC VALUE, (2) A SINGLE CHARACTER STRING OR (3) NA",
                collapse = NULL,
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
            checked_arg_names <- base::c(checked_arg_names, tempo1$object.name)
        }
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
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE) # registered here so that warning.length is restored even if an error occurs before the warning-output on.exit() registrations below
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    if(base::ncol(x = mat) != base::nrow(x = mat)){
        tempo_cat <- base::paste0(
            error_text_start,
            "mat ARGUMENT MUST BE A SQUARE MATRIX",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! (base::mode(x = mat) %in% base::c("numeric", "character"))){
        tempo_cat <- base::paste0(
            error_text_start,
            "mat ARGUMENT MUST BE A NUMERIC OR CHARACTER MATRIX",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::nrow(x = mat) == 1L & base::ncol(x = mat) == 1L){
        tempo_cat <- base::paste0(
            error_text_start,
            "mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::ifelse(test = base::is.na(x = empty_cell_string), yes = ! base::any(base::is.na(x = mat)), no = ! base::any(mat == empty_cell_string, na.rm = TRUE))){
        tempo_cat <- base::paste0(
            error_text_start,
            "mat ARGUMENT MATRIX MUST HAVE CELLS WITH THE EMPTY STRING SPECIFIED IN empty_cell_string ARGUMENT",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    list_diag <- base::vector(mode = "list", length = base::nrow(x = mat) - 1L)
    for(i1 in 1:(base::nrow(x = mat) - 1L)){
        list_diag[[i1]] <- base::numeric(length = base::nrow(x = mat) - i1) # list made of zero
    }
    sector <- base::c("topleft", "topright", "bottomright", "bottomleft")
    diag_scan <- base::c( # same order as sector. Recover each diag from center to corner
        "mat[base::as.matrix(x = base::as.data.frame(x = base::list(1:(base::nrow(x = mat) - i2), (base::ncol(x = mat) -i2):1), row.names = NULL, optional = FALSE, make.names = TRUE, stringsAsFactors = TRUE), rownames.force = NA)]", # topleft part
        "mat[base::as.matrix(x = base::as.data.frame(x = base::list(1:(base::nrow(x = mat) - i2), (1:base::ncol(x = mat))[-(1:i2)]), row.names = NULL, optional = FALSE, make.names = TRUE, stringsAsFactors = TRUE), rownames.force = NA)]", # topright part
        "mat[base::as.matrix(x = base::as.data.frame(x = base::list((1 + i2):base::nrow(x = mat), base::ncol(x = mat):(1 + i2)), row.names = NULL, optional = FALSE, make.names = TRUE, stringsAsFactors = TRUE), rownames.force = NA)]", # bottomright part
        "mat[base::as.matrix(x = base::as.data.frame(x = base::list((1 + i2):base::nrow(x = mat), 1:(base::ncol(x = mat) -i2)), row.names = NULL, optional = FALSE, make.names = TRUE, stringsAsFactors = TRUE), rownames.force = NA)]" # bottomleft part
    )
    # empty part detection
    empty_sector <- NULL
    full_sector <- NULL
    for(i1 in 1:base::length(x = sector)){
        tempo_list_diag <- list_diag
        for(i2 in 1:(base::nrow(x = mat) - 1L)){
            tempo_list_diag[[i2]] <- base::eval(expr = base::parse(text = diag_scan[i1], file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            if(base::ifelse(test = base::is.na(x = empty_cell_string), yes = ! base::all(base::is.na(x = tempo_list_diag[[i2]])), no = ! (base::all(tempo_list_diag[[i2]] == empty_cell_string, na.rm = TRUE) & ! (base::is.na(base::all(tempo_list_diag[[i2]] == empty_cell_string, na.rm = FALSE)))))){ # I had to add this ! (is.na(all(tempo_list_diag[[i2]] == empty_cell_string, na.rm = FALSE))) because all(tempo_list_diag[[i2]] == empty_cell_string, na.rm = FALSE) gives NA and not FALSE if one NA in tempo_list_diag[[i2]] -> not good for if()
                full_sector <- base::c(full_sector, sector[i1])
                break
            }
        }
        if(i2 == base::nrow(x = mat) - 1L){
            if(base::all(base::unlist(x = base::lapply(X = tempo_list_diag, FUN = function(x){if(base::is.na(x = empty_cell_string)){base::is.na(x = x)}else{x == empty_cell_string}}), recursive = TRUE, use.names = TRUE), na.rm = TRUE)){
                empty_sector <- base::c(empty_sector, sector[i1])
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") EMPTY SECTOR DETECTED ON THE ", base::toupper(x = sector[i1]), " CORNER, FULL OF ", empty_cell_string, collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }else{
                tempo_cat <- base::paste0(
                    error_text_start,
                    "THE ", base::toupper(x = sector[i1]), " SECTOR, DETECTED AS EMPTY, IS NOT? DIFFERENT VALUES IN THIS SECTOR:\n", base::paste0(base::names(base::table(base::unlist(x = tempo_list_diag, recursive = TRUE, use.names = TRUE), useNA = "ifany")), collapse = " ", recycle0 = FALSE),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # end empty part detection
    if(base::length(x = empty_sector) == 0L){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") ACCORDING TO empty_cell_string ARGUMENT (", empty_cell_string, "), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }else{
        if(base::length(x = empty_sector) > 1){
            tempo_cat <- base::paste0(
                error_text_start,
                "ACCORDING TO empty_cell_string ARGUMENT (", empty_cell_string, "), mat ARGUMENT MATRIX HAS MORE THAN ONE EMPTY HALF PART (ACCORDING TO THE GRAND DIAGONAL): ", base::paste0(empty_sector, collapse = " ", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::any(full_sector %in% empty_sector, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start,
                "THE FUNCTION HAS DETECTED EMPTY AND NON EMPTY HALF PART IN THE SAME SECTOR: ", base::paste0(full_sector[full_sector %in% empty_sector], collapse = " ", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::length(x = empty_sector) + base::length(x = full_sector) != 4){
            # nocov start
            # codecov inactivated because this branch is unreachable with argument values: each of the 4 sectors ends in exactly one of empty_sector or full_sector (a sector either breaks the diag loop -> full_sector, or completes the loop without break -> either passes the sector confirmation -> empty_sector, or stops the function with the "DETECTED AS EMPTY, IS NOT?" error; a sector that breaks at the last diag with NA-mixed values lands in both, which is caught by the "EMPTY AND NON EMPTY HALF PART" check above). Therefore, when length(empty_sector) == 1, full_sector always has exactly 3 entries and the sum is always 4. Note: 2x2 matrices, where each sector is a single cell, systematically take the "DETECTED AS EMPTY, IS NOT?" error path (pre-existing behavior, see the tests)
            tempo_cat <- base::paste0(
                error_text_start,
                "THE FUNCTION HAS DETECTED MORE OR LESS SECTORS THAN 4:\nHALF SECTORS:", base::paste0(empty_sector, collapse = " ", recycle0 = FALSE), "\nFULL SECTORS:", base::paste0(full_sector, collapse = " ", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }else{
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") ", base::toupper(x = empty_sector), " SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        # matrix filling
        for(i2 in 1:(base::nrow(x = mat) - 1L)){
            if(empty_sector == "topleft"){
                base::eval(expr = base::parse(text = base::paste0(diag_scan[1], " <- ", diag_scan[3], collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            }else if(empty_sector == "topright"){
                base::eval(expr = base::parse(text = base::paste0(diag_scan[2], " <- ", diag_scan[4], collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            }else if(empty_sector == "bottomright"){
                base::eval(expr = base::parse(text = base::paste0(diag_scan[3], " <- ", diag_scan[1], collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            }else if(empty_sector == "bottomleft"){
                base::eval(expr = base::parse(text = base::paste0(diag_scan[4], " <- ", diag_scan[2], collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            }
        }
        # end matrix filling
    }
    #### end main code

    #### warning output
    # must be before return()
    if(warn_print == TRUE & ! base::is.null(x = warn)){
        base::on.exit(
            expr = base::warning(
                base::paste0(
                    base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
                    warn,
                    collapse = NULL,
                    recycle0 = FALSE
                ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL
            ), add = TRUE, after = TRUE
        )
    }
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    #### end warning output

    #### output
    base::return(base::list(mat = mat, warn = warn))
    #### end output
}

#### end of function
