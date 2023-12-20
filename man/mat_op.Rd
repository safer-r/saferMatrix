\name{mat_op}
\alias{mat_op}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mat_op(mat.list, kind.of.operation = "+")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat.list}{
%%     ~~Describe \code{mat.list} here~~
}
  \item{kind.of.operation}{
%%     ~~Describe \code{kind.of.operation} here~~
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
function (mat.list, kind.of.operation = "+") 
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
    .pack_and_function_check(fun = c("cuteDev::arg_check", "cuteTool::comp_2d"), 
        lib.path = NULL, external.function.name = function.name)
    mandat.args <- c("mat.list")
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
    tempo <- cuteDev::arg_check(data = mat.list, class = "list", 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = kind.of.operation, options = c("+", 
        "-", "*"), length = 1, fun.name = function.name)
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
    tempo.arg <- c("mat.list", "kind.of.operation")
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
    if (length(mat.list) < 2) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: mat.list ARGUMENT MUST BE A LIST CONTAINING AT LEAST 2 MATRICES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    for (i1 in 1:length(mat.list)) {
        tempo <- cuteDev::arg_check(data = mat.list[[i1]], class = "matrix", 
            mode = "numeric", na.contain = TRUE)
        if (tempo$problem == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: ELEMENT ", i1, " OF mat.list ARGUMENT MUST BE A NUMERIC MATRIX")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    ident.row.names <- TRUE
    ident.col.names <- TRUE
    for (i1 in 2:length(mat.list)) {
        tempo <- cuteTool::comp_2d(data1 = mat.list[[1]], data2 = mat.list[[i1]])
        if (tempo$same.dim == FALSE) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: MATRIX ", i1, " OF mat.list ARGUMENT MUST HAVE THE SAME DIMENSION (", 
                paste(dim(mat.list[[i1]]), collapse = " "), ") THAN THE MATRIX 1 IN mat.list (", 
                paste(dim(mat.list[[1]]), collapse = " "), ")")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (!is.null(tempo$same.row.name)) {
            if (tempo$same.row.name != TRUE) {
                ident.row.names <- FALSE
            }
        }
        if (!is.null(tempo$same.col.name)) {
            if (tempo$same.col.name != TRUE) {
                ident.col.names <- FALSE
            }
        }
    }
    output <- mat.list[[1]]
    for (i1 in 2:length(mat.list)) {
        output <- get(kind.of.operation)(output, mat.list[[i1]])
    }
    dimnames(output) <- NULL
    if (ident.row.names == TRUE) {
        rownames(output) <- rownames(mat.list[[1]])
    }
    if (ident.col.names == TRUE) {
        colnames(output) <- colnames(mat.list[[1]])
    }
    return(output)
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
