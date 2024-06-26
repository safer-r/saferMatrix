% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mat_fill.R
\name{mat_fill}
\alias{mat_fill}
\title{mat_fill}
\usage{
mat_fill(mat, empty.cell.string = 0, warn.print = FALSE, safer_check = TRUE)
}
\arguments{
\item{mat}{A numeric or character square matrix with the half part (according to the grand diagonal) filled with NA (any kind of matrix), "0" (character matrix) or 0 (numeric matrix) exclusively (not a mix of 0 and NA in the empty part).}

\item{empty.cell.string}{A numeric, character or NA (no quotes) indicating what empty cells are filled with.}

\item{warn.print}{Single logical value. Print warnings at the end of the execution? No print if no warning messages.}

\item{safer_check}{Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.}
}
\value{
A list containing:

- $mat: The filled matrix.

- $warn: The warning messages. Use cat() for proper display. NULL if no warning.
}
\description{
Detect the empty half part of a symmetric square matrix (either topleft, topright, bottomleft or bottomright).

Fill this empty half part using the other symmetric half part of the matrix.
}
\details{
WARNINGS

A plot verification using gg_heatmap() is recommanded.
}
\examples{
# bottomleft example
mat1 = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; 
mat1 ; 
mat_fill(mat = mat1, empty.cell.string = NA, warn.print = TRUE) 
}
\author{
Gael Millot <gael.millot@pasteur.fr>

Yushi Han <yushi.han2000@gmail.com>

Haiding Wang <wanghaiding442@gmail.com>
}
