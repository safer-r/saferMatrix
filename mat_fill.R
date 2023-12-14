#' @title mat_fill
#' @description
#' Detect the empty half part of a symmetric square matrix (either topleft, topright, bottomleft or bottomright).
#' 
#' Fill this empty half part using the other symmetric half part of the matrix.
#' @param mat A numeric or character square matrix with the half part (according to the grand diagonal) filled with NA (any kind of matrix), "0" (character matrix) or 0 (numeric matrix) exclusively (not a mix of 0 and NA in the empty part).
#' @param empty.cell.string A numeric, character or NA (no quotes) indicating what empty cells are filled with.
#' @param warn.print Single logical value. Print warnings at the end of the execution? No print if no warning messages.
#' @returns
#' A list containing:
#' - $mat: The filled matrix.
#' - $warn: The warning messages. Use cat() for proper display. NULL if no warning.
#' @details
#' WARNINGS
#' 
#' A plot verification using gg_heatmap() is recommanded.
#' 
#' 
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' arg_check()
#' @examples
#' mat1 = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; 
#' mat1 ; 
#' mat_fill(mat = mat1, empty.cell.string = NA, warn.print = TRUE) 
#' # bottomleft example
#' 
#' mat1 = matrix(c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4) ; 
#' mat1 ; 
#' mat_fill(mat = mat1, empty.cell.string = NA, warn.print = TRUE) 
#' # error example
#' 
#' mat1 = matrix(c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4) ; 
#' mat1 ; 
#' mat_fill(mat = mat1, empty.cell.string = 0, warn.print = TRUE) 
#' # bottomright example
#' 
#' mat1 = matrix(c(1,1,1,2, "a",2,3,NA, "a","a",0,0, "a","a","a",0), ncol = 4) ; 
#' mat1 ; 
#' mat_fill(mat = mat1, empty.cell.string = "a", warn.print = TRUE) 
#' # topright example
#' 
#' mat1 = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; 
#' mat1 ; 
#' mat_fill(mat = mat1, empty.cell.string = 0, warn.print = TRUE) 
#' # topleft example
#' 
#' mat1 = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,0, 5,0,0,0), ncol = 4) ; 
#' mat1 ; 
#' mat_fill(mat = mat1, empty.cell.string = 0, warn.print = TRUE) 
#' # error example 
#' @importFrom cuteDev arg_check
#' @export
mat_fill <- function(
        mat, 
        empty.cell.string = 0, 
        warn.print = FALSE
){
    # DEBUGGING
    # mat = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; empty.cell.string = NA ; warn.print = TRUE # for function debugging
    # mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; empty.cell.string = 0 ; warn.print = TRUE # for function debugging # topleft example
    # mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; empty.cell.string = NA ; warn.print = TRUE # for function debugging # topleft example
    # function name
    ini <- match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    
    # package checking
    # check of lib.path
    # end check of lib.path
    
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = c(
            "cuteDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
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
    
    # argument checking with cuteDev::arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check = c(argum.check, tempo$problem) , text.check = c(text.check, tempo$text) , checked.arg.names = c(checked.arg.names, tempo$object.name))
    tempo <- cuteDev::arg_check(data = mat, class = "matrix", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = empty.cell.string, class = "vector", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with cuteDev::arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using cuteDev::arg_check()
    # end check with r_debugging_tools
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
        "mat",
        "empty.cell.string",
        "warn.print"
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
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    
    # other checkings
    # argument checking without cuteDev::arg_check()
    if(ncol(mat) != nrow(mat)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT MUST BE A SQUARE MATRIX")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (base::mode(mat) %in% c("numeric", "character"))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT MUST BE A NUMERIC OR CHARACTER MATRIX")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(nrow(mat) == 1L & ncol(mat) == 1L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(ifelse(is.na(empty.cell.string), ! any(is.na(mat)), ! any(mat == empty.cell.string, na.rm = TRUE))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": mat ARGUMENT MATRIX MUST HAVE CELLS WITH THE EMPTY STRING SPECIFIED IN empty.cell.string ARGUMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end argument checking without cuteDev::arg_check()
    # end other checkings
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    
    # end second round of checking and data preparation
    
    # main code
    list.diag <- vector("list", length = nrow(mat) - 1) 
    for(i1 in 1:(nrow(mat) - 1)){
        list.diag[[i1]] <- numeric(length = nrow(mat) - i1) # list made of zero
    }
    sector <- c("topleft", "topright", "bottomright", "bottomleft")
    diag.scan <-c( # same order as sector. Recover each diag from center to corner
        "mat[as.matrix(as.data.frame(list(1:(nrow(mat) - i2), (ncol(mat) -i2):1), stringsAsFactors = TRUE))]", # topleft part
        "mat[as.matrix(as.data.frame(list(1:(nrow(mat) - i2), (1:ncol(mat))[-(1:i2)]), stringsAsFactors = TRUE))]", # topright part
        "mat[as.matrix(as.data.frame(list((1 + i2):nrow(mat), ncol(mat):(1 + i2)), stringsAsFactors = TRUE))]", # bottomright part
        "mat[as.matrix(as.data.frame(list((1 + i2):nrow(mat), 1:(ncol(mat) -i2)), stringsAsFactors = TRUE))]" # bottomleft part
    )
    # empty part detection
    empty.sector <- NULL
    full.sector <- NULL
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    for(i1 in 1:length(sector)){
        tempo.list.diag <- list.diag
        for(i2 in 1:(nrow(mat) - 1)){
            tempo.list.diag[[i2]] <- eval(parse(text = diag.scan[i1]))
            if(ifelse(is.na(empty.cell.string), ! all(is.na(tempo.list.diag[[i2]])), ! (all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = TRUE) & ! (is.na(all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE)))))){ # I had to add this ! (is.na(all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE))) because all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE) gives NA and not FALSE if one NA in tempo.list.diag[[i2]] -> not good for if()
                full.sector <- c(full.sector, sector[i1])
                break
            }
        }
        if(i2 == nrow(mat) - 1){
            if(all(unlist(lapply(tempo.list.diag, FUN = function(x){if(is.na(empty.cell.string)){is.na(x)}else{x == empty.cell.string}})), na.rm = TRUE)){
                empty.sector <- c(empty.sector, sector[i1])
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") EMPTY SECTOR DETECTED ON THE ", toupper(sector[i1]), " CORNER, FULL OF ", empty.cell.string)
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }else{
                tempo.cat <- paste0("ERROR IN ", function.name, ": THE ", toupper(sector[i1]), " SECTOR, DETECTED AS EMPTY, IS NOT? DIFFERENT VALUES IN THIS SECTOR:\n", paste(names(table(unlist(tempo.list.diag), useNA = "ifany")), collapse = " "))
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
    }
    # end empty part detection
    if(length(empty.sector) == 0L){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") ACCORDING TO empty.cell.string ARGUMENT (", empty.cell.string, "), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }else{
        if(length(empty.sector) > 1){
            tempo.cat <- paste0("ERROR IN ", function.name, ": ACCORDING TO empty.cell.string ARGUMENT (", empty.cell.string, "), mat ARGUMENT MATRIX HAS MORE THAN ONE EMPTY HALF PART (ACCORDING TO THE GRAND DIAGONAL): ", paste(empty.sector, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(any(full.sector %in% empty.sector, na.rm = TRUE)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE FUNCTION HAS DETECTED EMPTY AND NON EMPTY HALF PART IN THE SAME SECTOR: ", paste(full.sector[full.sector %in% empty.sector], collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(length(empty.sector) + length(full.sector)!= 4){
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE FUNCTION HAS DETECTED MORE OR LESS SECTORS THAN 4:\nHALF SECTORS:", paste(empty.sector, collapse = " "), "\nFULL SECTORS:", paste(full.sector, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") ", toupper(empty.sector), " SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        # matrix filling
        for(i2 in 1:(nrow(mat) - 1)){
            if(empty.sector == "topleft"){
                eval(parse(text = paste0(diag.scan[1], " <- ", diag.scan[3])))
            }else if(empty.sector == "topright"){
                eval(parse(text = paste0(diag.scan[2], " <- ", diag.scan[4])))
            }else if(empty.sector == "bottomright"){
                eval(parse(text = paste0(diag.scan[3], " <- ", diag.scan[1])))
            }else if(empty.sector == "bottomleft"){
                eval(parse(text = paste0(diag.scan[4], " <- ", diag.scan[2])))
            }
        }
        # end matrix filling
    }
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    on.exit(expr = options(warning.length = ini.warning.length), add = TRUE)
    # output
    # warning output
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
      }
      on.exit(expr = options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    return(list(mat = mat, warn = warn))
    # end output
    # end main code
}
