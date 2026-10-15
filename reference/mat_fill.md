# mat_fill

Detect the empty half part of a symmetric square matrix (either topleft,
topright, bottomleft or bottomright).

Fill this empty half part using the other symmetric half part of the
matrix.

## Usage

``` r
mat_fill(
  mat,
  empty.cell.string = 0,
  warn.print = FALSE,
  lib_path = NULL,
  safer_check = TRUE,
  error_text = ""
)
```

## Arguments

- mat:

  A numeric or character square matrix with the half part (according to
  the grand diagonal) filled with NA (any kind of matrix), "0"
  (character matrix) or 0 (numeric matrix) exclusively (not a mix of 0
  and NA in the empty part).

- empty.cell.string:

  A numeric, character or NA (no quotes) indicating what empty cells are
  filled with.

- warn.print:

  Single logical value. Print warnings at the end of the execution? No
  print if no warning messages.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `unique(c(new, .Library.site, .Library))`. Warning:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored to
  the initial paths, after function execution. Ignored if `NULL`
  (default) or if the `safer_check` argument is `FALSE`: only the
  pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- safer_check:

  Single logical value. Perform the "safer" checks? If `TRUE`, checkings
  are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Warning: must
  be set to `FALSE` if this function is used inside another "safer"
  function to avoid pointless multiple checkings.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

A list containing:

\- \$mat: The filled matrix.

\- \$warn: The warning messages. Use cat() for proper display. NULL if
no warning.

## Details

WARNINGS

A plot verification using gg_heatmap() is recommanded.

2x2 matrices are not supported: each sector being a single cell, the
sector detection always ends with the "DETECTED AS EMPTY, IS NOT?"
error, even for a valid single-empty-half matrix (pre-existing behavior,
kept unchanged during the migration).

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
# bottomleft example
mat1 <- matrix(data = c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ;
mat1 ;
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0   NA    5
#> [2,]   NA    2    3    6
#> [3,]   NA   NA    4    7
#> [4,]   NA   NA   NA    8
mat_fill(mat = mat1, empty.cell.string = NA, warn.print = TRUE)
#> Warning: FROM saferMatrix::mat_fill().
#> 
#> (1) EMPTY SECTOR DETECTED ON THE BOTTOMLEFT CORNER, FULL OF NA
#> 
#> (2) BOTTOMLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL
#> $mat
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0   NA    5
#> [2,]    0    2    3    6
#> [3,]   NA    3    4    7
#> [4,]    5    6    7    8
#> 
#> $warn
#> [1] "(1) EMPTY SECTOR DETECTED ON THE BOTTOMLEFT CORNER, FULL OF NA\n\n(2) BOTTOMLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
#> 
```
