\name{mat_num2color}
\alias{mat_num2color}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mat_num2color(mat1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat1}{
%%     ~~Describe \code{mat1} here~~
}
  \item{mat.hsv.h}{
%%     ~~Describe \code{mat.hsv.h} here~~
}
  \item{notch}{
%%     ~~Describe \code{notch} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
}
  \item{forced.color}{
%%     ~~Describe \code{forced.color} here~~
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
function (mat1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL) 
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
    mandat.args <- c("mat1")
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
    tempo <- cuteDev::arg_check(data = mat1, mode = "numeric", 
        class = "matrix", na.contain = TRUE, neg.values = FALSE, 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = mat.hsv.h, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = notch, class = "vector", 
        mode = "numeric", length = 1, prop = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = s, class = "vector", mode = "numeric", 
        length = 1, prop = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = v, class = "vector", mode = "numeric", 
        length = 1, prop = TRUE, fun.name = function.name)
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
    tempo.arg <- c("mat1", "mat.hsv.h", "notch", "s", "v")
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
    if (mat.hsv.h == TRUE & cuteDev::arg_check(data = mat1, mode = "numeric", 
        prop = TRUE)$problem == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
            package.name, " PACKAGE: mat1 ARGUMENT MUST BE A MATRIX OF PROPORTIONS SINCE THE mat.hsv.h ARGUMENT IS SET TO TRUE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!is.null(forced.color)) {
        tempo <- cuteDev::arg_check(data = forced.color, class = "character")
        if (any(tempo$problem == TRUE, na.rm = TRUE)) {
            paste0("\n\n================\n\n", paste(tempo$text[tempo$problem], 
                collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (!all(forced.color \%in\% colors() | grepl(pattern = "^#", 
            forced.color), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: forced.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors()")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    problem <- NULL
    text.problem <- NULL
    mat1.name <- deparse(substitute(mat1))
    if (mat.hsv.h == TRUE) {
        if (any(min(mat1, na.rm = TRUE) < 0 | max(mat1, na.rm = TRUE) > 
            1, na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: mat1 MUST BE MADE OF VALUES BETWEEN 0 AND 1 BECAUSE mat.hsv.h ARGUMENT SET TO TRUE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    else {
        if (any(mat1 - floor(mat1) > 0, na.rm = TRUE) | any(mat1 == 
            0L, na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: mat1 MUST BE MADE OF INTEGER VALUES WITHOUT 0 BECAUSE mat.hsv.h ARGUMENT SET TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            mat1 <- mat1/max(mat1, na.rm = TRUE)
        }
    }
    if (notch != 1) {
        different.color <- unique(as.vector(mat1))
        different.color <- different.color[!is.na(different.color)]
        tempo.different.color <- different.color + c(0, cumsum(rep(notch, 
            length(different.color) - 1)))
        tempo.different.color <- tempo.different.color - floor(tempo.different.color)
        if (any(duplicated(tempo.different.color) == TRUE, na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: DUPLICATED VALUES AFTER USING notch (", 
                paste(tempo.different.color[duplicated(tempo.different.color)], 
                  collapse = " "), "). TRY ANOTHER notch VALUE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else if (length(different.color) != length(tempo.different.color)) {
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", 
                package.name, " PACKAGE: LENGTH OF different.color (", 
                paste(different.color, collapse = " "), ") DIFFERENT FROM LENGTH OF tempo.different.color (", 
                paste(tempo.different.color, collapse = " "), 
                ")")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            for (i in 1:length(different.color)) {
                mat1[mat1 == different.color[i]] <- tempo.different.color[i]
            }
        }
    }
    if (!is.null(forced.color)) {
        hexa.values.to.change <- hsv(unique(sort(mat1))[1:length(forced.color)], 
            s, v)
    }
    mat1[!is.na(mat1)] <- hsv(mat1[!is.na(mat1)], s, v)
    if (!is.null(forced.color)) {
        if (any(forced.color \%in\% mat1, na.rm = TRUE)) {
            problem <- TRUE
            text.problem <- paste0("THE FOLLOWING COLORS WHERE INTRODUCED USING forced.color BUT WHERE ALREADY PRESENT IN THE COLORED MATRIX :", 
                paste(forced.color[forced.color \%in\% mat1], collapse = " "))
        }
        else {
            problem <- FALSE
        }
        for (i in 1:length(hexa.values.to.change)) {
            if (!any(mat1 == hexa.values.to.change[i], na.rm = TRUE)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  " OF THE", package.name, " PACKAGE: THE ", 
                  hexa.values.to.change[i], " VALUE FROM hexa.values.to.change IS NOT REPRESENTED IN mat1 : ", 
                  paste(unique(as.vector(mat1)), collapse = " "))
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            else {
                mat1[which(mat1 == hexa.values.to.change[i])] <- forced.color[i]
            }
        }
    }
    output <- list(mat1.name = mat1.name, colored.mat = mat1, 
        problem = problem, text.problem = text.problem)
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
