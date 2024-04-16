#' @title mat_fill
#' @description
#' Detect the empty half part of a symmetric square matrix (either topleft, topright, bottomleft or bottomright).
#' 
#' Fill this empty half part using the other symmetric half part of the matrix.
#' @param mat A numeric or character square matrix with the half part (according to the grand diagonal) filled with NA (any kind of matrix), "0" (character matrix) or 0 (numeric matrix) exclusively (not a mix of 0 and NA in the empty part).
#' @param empty.cell.string A numeric, character or NA (no quotes) indicating what empty cells are filled with.
#' @param warn.print Single logical value. Print warnings at the end of the execution? No print if no warning messages.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
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
#' @examples
#' # bottomleft example
#' mat1 = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; 
#' mat1 ; 
#' mat_fill(mat = mat1, empty.cell.string = NA, warn.print = TRUE) 
#' @importFrom saferDev arg_check
#' @export
mat_fill <- function(
        mat, 
        empty.cell.string = 0, 
        warn.print = FALSE,
        safer_check = TRUE
){
    # DEBUGGING
    # mat = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; empty.cell.string = NA ; warn.print = TRUE ; safer_check = TRUE # for function debugging
    # mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; empty.cell.string = 0 ; warn.print = TRUE ; safer_check = TRUE# for function debugging # topleft example
    # mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; empty.cell.string = NA ; warn.print = TRUE ; safer_check = TRUE# for function debugging # topleft example
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
        "mat"
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
    tempo <- saferDev::arg_check(data = mat, class = "matrix", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = empty.cell.string, class = "vector", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
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
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.test <- tempo.arg == "empty.cell.string"
        if(base::any(tempo.test, na.rm = TRUE) == TRUE){
            tempo.arg <- tempo.arg[ ! tempo.test] 
        }
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "mat",
        "empty.cell.string",
        "warn.print",
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
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    
    # other checkings
    # argument checking without saferDev::arg_check()
    if(base::ncol(mat) != base::nrow(mat)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat ARGUMENT MUST BE A SQUARE MATRIX")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if( ! (base::mode(mat) %in% base::c("numeric", "character"))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat ARGUMENT MUST BE A NUMERIC OR CHARACTER MATRIX")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(base::nrow(mat) == 1L & base::ncol(mat) == 1L){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(base::ifelse(base::is.na(empty.cell.string), ! base::any(base::is.na(mat)), ! base::any(mat == empty.cell.string, na.rm = TRUE))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: mat ARGUMENT MATRIX MUST HAVE CELLS WITH THE EMPTY STRING SPECIFIED IN empty.cell.string ARGUMENT")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    # end argument checking without saferDev::arg_check()
    # end other checkings
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    
    # end second round of checking and data preparation
    
    # main code
    list.diag <- base::vector("list", length = base::nrow(mat) - 1) 
    for(i1 in 1:(base::nrow(mat) - 1)){
        list.diag[[i1]] <- base::numeric(length = base::nrow(mat) - i1) # list made of zero
    }
    sector <- base::c("topleft", "topright", "bottomright", "bottomleft")
    diag.scan <-base::c( # same order as sector. Recover each diag from center to corner
        "mat[base::as.matrix(base::as.data.frame(base::list(1:(base::nrow(mat) - i2), (base::ncol(mat) -i2):1), stringsAsFactors = TRUE))]", # topleft part
        "mat[base::as.matrix(base::as.data.frame(base::list(1:(base::nrow(mat) - i2), (1:base::ncol(mat))[-(1:i2)]), stringsAsFactors = TRUE))]", # topright part
        "mat[base::as.matrix(base::as.data.frame(base::list((1 + i2):base::nrow(mat), base::ncol(mat):(1 + i2)), stringsAsFactors = TRUE))]", # bottomright part
        "mat[base::as.matrix(base::as.data.frame(base::list((1 + i2):base::nrow(mat), 1:(base::ncol(mat) -i2)), stringsAsFactors = TRUE))]" # bottomleft part
    )
    # empty part detection
    empty.sector <- NULL
    full.sector <- NULL
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    for(i1 in 1:base::length(sector)){
        tempo.list.diag <- list.diag
        for(i2 in 1:(base::nrow(mat) - 1)){
            tempo.list.diag[[i2]] <- base::eval(base::parse(text = diag.scan[i1]))
            if(base::ifelse(base::is.na(empty.cell.string), ! base::all(base::is.na(tempo.list.diag[[i2]])), ! (base::all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = TRUE) & ! (base::is.na(base::all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE)))))){ # I had to add this ! (is.na(all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE))) because all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE) gives NA and not FALSE if one NA in tempo.list.diag[[i2]] -> not good for if()
                full.sector <- base::c(full.sector, sector[i1])
                break
            }
        }
        if(i2 == base::nrow(mat) - 1){
            if(base::all(base::unlist(base::lapply(tempo.list.diag, FUN = function(x){if(base::is.na(empty.cell.string)){base::is.na(x)}else{x == empty.cell.string}})), na.rm = TRUE)){
                empty.sector <- base::c(empty.sector, sector[i1])
                warn.count <- warn.count + 1
                tempo.warn <- base::paste0("(", warn.count,") EMPTY SECTOR DETECTED ON THE ", base::toupper(sector[i1]), " CORNER, FULL OF ", empty.cell.string)
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            }else{
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE ", base::toupper(sector[i1]), " SECTOR, DETECTED AS EMPTY, IS NOT? DIFFERENT VALUES IN THIS SECTOR:\n", base::paste(base::names(base::table(base::unlist(tempo.list.diag), useNA = "ifany")), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
    }
    # end empty part detection
    if(base::length(empty.sector) == 0L){
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") ACCORDING TO empty.cell.string ARGUMENT (", empty.cell.string, "), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }else{
        if(base::length(empty.sector) > 1){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: ACCORDING TO empty.cell.string ARGUMENT (", empty.cell.string, "), mat ARGUMENT MATRIX HAS MORE THAN ONE EMPTY HALF PART (ACCORDING TO THE GRAND DIAGONAL): ", base::paste(empty.sector, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else if(base::any(full.sector %in% empty.sector, na.rm = TRUE)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE FUNCTION HAS DETECTED EMPTY AND NON EMPTY HALF PART IN THE SAME SECTOR: ", base::paste(full.sector[full.sector %in% empty.sector], collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else if(base::length(empty.sector) + base::length(full.sector)!= 4){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE FUNCTION HAS DETECTED MORE OR LESS SECTORS THAN 4:\nHALF SECTORS:", base::paste(empty.sector, collapse = " "), "\nFULL SECTORS:", base::paste(full.sector, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") ", base::toupper(empty.sector), " SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
        }
        # matrix filling
        for(i2 in 1:(base::nrow(mat) - 1)){
            if(empty.sector == "topleft"){
                base::eval(base::parse(text = base::paste0(diag.scan[1], " <- ", diag.scan[3])))
            }else if(empty.sector == "topright"){
                base::eval(base::parse(text = base::paste0(diag.scan[2], " <- ", diag.scan[4])))
            }else if(empty.sector == "bottomright"){
                base::eval(base::parse(text = base::paste0(diag.scan[3], " <- ", diag.scan[1])))
            }else if(empty.sector == "bottomleft"){
                base::eval(base::parse(text = base::paste0(diag.scan[4], " <- ", diag.scan[2])))
            }
        }
        # end matrix filling
    }
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    # output
    # warning output
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
      }
      base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    base::return(base::list(mat = mat, warn = warn))
    # end output
    # end main code
}
