\name{mat_fill}
\alias{mat_fill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mat_fill(mat, empty.cell.string = 0, warn.print = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{empty.cell.string}{
%%     ~~Describe \code{empty.cell.string} here~~
}
  \item{warn.print}{
%%     ~~Describe \code{warn.print} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mat, empty.cell.string = 0, warn.print = FALSE) 
{
    package.name <- "cuteMatrix"
    ini <- match.call(expand.dots = FALSE)
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    if (function.name[1] == "::()") {
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    .pack_and_function_check(fun = c("cuteDev::arg_check"), lib.path = NULL, 
        external.function.name = function.name)
    mandat.args <- c("mat")
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, 
        collapse = ") | missing("), ")")))
    if (any(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: \nFOLLOWING ARGUMENT", ifelse(sum(tempo, 
                na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(argum.check = c(argum.check, tempo$problem), 
        text.check = c(text.check, tempo$text), checked.arg.names = c(checked.arg.names, 
            tempo$object.name))
    tempo <- cuteDev::arg_check(data = mat, class = "matrix", 
        na.contain = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = empty.cell.string, class = "vector", 
        na.contain = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = warn.print, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(argum.check)) {
        if (any(argum.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
    }
    if (!(all(class(arg.user.setting) == "list", na.rm = TRUE) & 
        length(arg.user.setting) == 0)) {
        tempo.arg <- names(arg.user.setting)
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = length) == 1L
        if (any(tempo.log) == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: \n", ifelse(sum(tempo.log, 
                  na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), 
                " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("mat", "empty.cell.string", "warn.print")
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), 
        inherit = FALSE), FUN = is.null)
    if (any(tempo.log) == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE:\n", ifelse(sum(tempo.log, 
                na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), 
            paste0(tempo.arg[tempo.log], collapse = "\n"), "\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    if (ncol(mat) != nrow(mat)) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: mat ARGUMENT MUST BE A SQUARE MATRIX")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (!(base::mode(mat) \%in\% c("numeric", "character"))) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: mat ARGUMENT MUST BE A NUMERIC OR CHARACTER MATRIX")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (nrow(mat) == 1L & ncol(mat) == 1L) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (ifelse(is.na(empty.cell.string), !any(is.na(mat)), !any(mat == 
        empty.cell.string, na.rm = TRUE))) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: mat ARGUMENT MATRIX MUST HAVE CELLS WITH THE EMPTY STRING SPECIFIED IN empty.cell.string ARGUMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    list.diag <- vector("list", length = nrow(mat) - 1)
    for (i1 in 1:(nrow(mat) - 1)) {
        list.diag[[i1]] <- numeric(length = nrow(mat) - i1)
    }
    sector <- c("topleft", "topright", "bottomright", "bottomleft")
    diag.scan <- c("mat[as.matrix(as.data.frame(list(1:(nrow(mat) - i2), (ncol(mat) -i2):1), stringsAsFactors = TRUE))]", 
        "mat[as.matrix(as.data.frame(list(1:(nrow(mat) - i2), (1:ncol(mat))[-(1:i2)]), stringsAsFactors = TRUE))]", 
        "mat[as.matrix(as.data.frame(list((1 + i2):nrow(mat), ncol(mat):(1 + i2)), stringsAsFactors = TRUE))]", 
        "mat[as.matrix(as.data.frame(list((1 + i2):nrow(mat), 1:(ncol(mat) -i2)), stringsAsFactors = TRUE))]")
    empty.sector <- NULL
    full.sector <- NULL
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    for (i1 in 1:length(sector)) {
        tempo.list.diag <- list.diag
        for (i2 in 1:(nrow(mat) - 1)) {
            tempo.list.diag[[i2]] <- eval(parse(text = diag.scan[i1]))
            if (ifelse(is.na(empty.cell.string), !all(is.na(tempo.list.diag[[i2]])), 
                !(all(tempo.list.diag[[i2]] == empty.cell.string, 
                  na.rm = TRUE) & !(is.na(all(tempo.list.diag[[i2]] == 
                  empty.cell.string, na.rm = FALSE)))))) {
                full.sector <- c(full.sector, sector[i1])
                break
            }
        }
        if (i2 == nrow(mat) - 1) {
            if (all(unlist(lapply(tempo.list.diag, FUN = function(x) {
                if (is.na(empty.cell.string)) {
                  is.na(x)
                } else {
                  x == empty.cell.string
                }
            })), na.rm = TRUE)) {
                empty.sector <- c(empty.sector, sector[i1])
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count, ") EMPTY SECTOR DETECTED ON THE ", 
                  toupper(sector[i1]), " CORNER, FULL OF ", empty.cell.string)
                warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                  paste0(warn, "\n\n", tempo.warn)))
            }
            else {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  " OF THE ", package.name, " PACKAGE: THE ", 
                  toupper(sector[i1]), " SECTOR, DETECTED AS EMPTY, IS NOT? DIFFERENT VALUES IN THIS SECTOR:\n", 
                  paste(names(table(unlist(tempo.list.diag), 
                    useNA = "ifany")), collapse = " "))
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n", ifelse(is.null(warn), 
                    "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                      1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
    }
    if (length(empty.sector) == 0L) {
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count, ") ACCORDING TO empty.cell.string ARGUMENT (", 
            empty.cell.string, "), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, 
            "\n\n", tempo.warn)))
    }
    else {
        if (length(empty.sector) > 1) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: ACCORDING TO empty.cell.string ARGUMENT (", 
                empty.cell.string, "), mat ARGUMENT MATRIX HAS MORE THAN ONE EMPTY HALF PART (ACCORDING TO THE GRAND DIAGONAL): ", 
                paste(empty.sector, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        else if (any(full.sector \%in\% empty.sector, na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: THE FUNCTION HAS DETECTED EMPTY AND NON EMPTY HALF PART IN THE SAME SECTOR: ", 
                paste(full.sector[full.sector \%in\% empty.sector], 
                  collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        else if (length(empty.sector) + length(full.sector) != 
            4) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: THE FUNCTION HAS DETECTED MORE OR LESS SECTORS THAN 4:\nHALF SECTORS:", 
                paste(empty.sector, collapse = " "), "\nFULL SECTORS:", 
                paste(full.sector, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        else {
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count, ") ", toupper(empty.sector), 
                " SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                paste0(warn, "\n\n", tempo.warn)))
        }
        for (i2 in 1:(nrow(mat) - 1)) {
            if (empty.sector == "topleft") {
                eval(parse(text = paste0(diag.scan[1], " <- ", 
                  diag.scan[3])))
            }
            else if (empty.sector == "topright") {
                eval(parse(text = paste0(diag.scan[2], " <- ", 
                  diag.scan[4])))
            }
            else if (empty.sector == "bottomright") {
                eval(parse(text = paste0(diag.scan[3], " <- ", 
                  diag.scan[1])))
            }
            else if (empty.sector == "bottomleft") {
                eval(parse(text = paste0(diag.scan[4], " <- ", 
                  diag.scan[2])))
            }
        }
    }
    if (warn.print == TRUE & !is.null(warn)) {
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
            warn), call. = FALSE))
    }
    on.exit(expr = options(warning.length = ini.warning.length), 
        add = TRUE)
    if (warn.print == TRUE & !is.null(warn)) {
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
            warn), call. = FALSE))
    }
    on.exit(exp = options(warning.length = ini.warning.length), 
        add = TRUE)
    return(list(mat = mat, warn = warn))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
