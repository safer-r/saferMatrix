# mat_fill

\
[`library`](https://rdrr.io/r/base/library.html)`(``saferMatrix``)`

\

### Datasets

\
`mat1`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``, ``0``,``2``,``3``,``0``, ``NA``,``3``,``0``,``0``, ``5``,``0``,``0``,``0``)``, ncol ``=`` ``4``)`\
`mat2`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``, ``"a"``,``2``,``3``,``NA``, ``"a"``,``"a"``,``0``,``0``, ``"a"``,``"a"``,``"a"``,``0``)``, ncol ``=`` ``4``)`\
`mat3`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``,``0``,``0``,``2``, ``0``,``0``,``3``,``0``, ``0``,``3``,``0``,``NA``, ``5``,``0``,``0``,``0``)``, ncol ``=`` ``4``)`\
`mat4`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``,``0``,``0``,``2``, ``0``,``0``,``3``,``0``, ``0``,``3``,``0``,``0``, ``5``,``0``,``0``,``0``)``, ncol ``=`` ``4``)`

\

### Datasets info

``` r
mat1
>      [,1] [,2] [,3] [,4]
> [1,]    1    0   NA    5
> [2,]    1    2    3    0
> [3,]    1    3    0    0
> [4,]    2    0    0    0
mat2
>      [,1] [,2] [,3] [,4]
> [1,] "1"  "a"  "a"  "a" 
> [2,] "1"  "2"  "a"  "a" 
> [3,] "1"  "3"  "0"  "a" 
> [4,] "2"  NA   "0"  "0"
mat3
>      [,1] [,2] [,3] [,4]
> [1,]    0    0    0    5
> [2,]    0    0    3    0
> [3,]    0    3    0    0
> [4,]    2    0   NA    0
mat4
>      [,1] [,2] [,3] [,4]
> [1,]    0    0    0    5
> [2,]    0    0    3    0
> [3,]    0    3    0    0
> [4,]    2    0    0    0
```

\

### Simple examples

``` r
mat_fill(mat = mat1)
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    1    0   NA    5
> [2,]    1    2    3   NA
> [3,]    1    3    2    0
> [4,]    2    1    1    1
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0\n\n(2) BOTTOMRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
mat_fill(mat = mat2)
> $mat
>      [,1] [,2] [,3] [,4]
> [1,] "1"  "a"  "a"  "a" 
> [2,] "1"  "2"  "a"  "a" 
> [3,] "1"  "3"  "0"  "a" 
> [4,] "2"  NA   "0"  "0" 
> 
> $warn
> [1] "(1) ACCORDING TO empty_cell_string ARGUMENT (0), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART"
mat_fill(mat = mat3)
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    0    0    0    5
> [2,]   NA    0    3    0
> [3,]    0    3    0    0
> [4,]    2    0   NA    0
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0\n\n(2) TOPLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
```

\

### Argument empty_cell_string

``` r
mat_fill(mat = mat1, empty_cell_string = 0)
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    1    0   NA    5
> [2,]    1    2    3   NA
> [3,]    1    3    2    0
> [4,]    2    1    1    1
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0\n\n(2) BOTTOMRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
mat_fill(mat = mat2, empty_cell_string = "a")
> $mat
>      [,1] [,2] [,3] [,4]
> [1,] "1"  "1"  "1"  "2" 
> [2,] "1"  "2"  "3"  NA  
> [3,] "1"  "3"  "0"  "0" 
> [4,] "2"  NA   "0"  "0" 
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE TOPRIGHT CORNER, FULL OF a\n\n(2) TOPRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
mat_fill(mat = mat3, empty_cell_string = 0)
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    0    0    0    5
> [2,]   NA    0    3    0
> [3,]    0    3    0    0
> [4,]    2    0   NA    0
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0\n\n(2) TOPLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
```

\

### Argument warn_print

``` r
 mat_fill(mat = mat1, warn_print = TRUE)
> Warning: FROM saferMatrix::mat_fill().
> 
> (1) EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0
> 
> (2) BOTTOMRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    1    0   NA    5
> [2,]    1    2    3   NA
> [3,]    1    3    2    0
> [4,]    2    1    1    1
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0\n\n(2) BOTTOMRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
 mat_fill(mat = mat2, warn_print = TRUE)
> Warning: FROM saferMatrix::mat_fill().
> 
> (1) ACCORDING TO empty_cell_string ARGUMENT (0), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART
> $mat
>      [,1] [,2] [,3] [,4]
> [1,] "1"  "a"  "a"  "a" 
> [2,] "1"  "2"  "a"  "a" 
> [3,] "1"  "3"  "0"  "a" 
> [4,] "2"  NA   "0"  "0" 
> 
> $warn
> [1] "(1) ACCORDING TO empty_cell_string ARGUMENT (0), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART"
 mat_fill(mat = mat3, warn_print = TRUE)
> Warning: FROM saferMatrix::mat_fill().
> 
> (1) EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0
> 
> (2) TOPLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    0    0    0    5
> [2,]   NA    0    3    0
> [3,]    0    3    0    0
> [4,]    2    0   NA    0
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0\n\n(2) TOPLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
```

\

### All the arguments

``` r
# bottomright example
mat_fill(mat = mat1, empty_cell_string = 0, warn_print = TRUE) 
> Warning: FROM saferMatrix::mat_fill().
> 
> (1) EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0
> 
> (2) BOTTOMRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    1    0   NA    5
> [2,]    1    2    3   NA
> [3,]    1    3    2    0
> [4,]    2    1    1    1
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0\n\n(2) BOTTOMRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"

# topright example
mat_fill(mat = mat2, empty_cell_string = "a", warn_print = TRUE) 
> Warning: FROM saferMatrix::mat_fill().
> 
> (1) EMPTY SECTOR DETECTED ON THE TOPRIGHT CORNER, FULL OF a
> 
> (2) TOPRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL
> $mat
>      [,1] [,2] [,3] [,4]
> [1,] "1"  "1"  "1"  "2" 
> [2,] "1"  "2"  "3"  NA  
> [3,] "1"  "3"  "0"  "0" 
> [4,] "2"  NA   "0"  "0" 
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE TOPRIGHT CORNER, FULL OF a\n\n(2) TOPRIGHT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"

# topleft example
mat_fill(mat = mat3, empty_cell_string = 0, warn_print = TRUE) 
> Warning: FROM saferMatrix::mat_fill().
> 
> (1) EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0
> 
> (2) TOPLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    0    0    0    5
> [2,]   NA    0    3    0
> [3,]    0    3    0    0
> [4,]    2    0   NA    0
> 
> $warn
> [1] "(1) EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0\n\n(2) TOPLEFT SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL"
```

\

### Error examples

``` r
mat_fill(mat = mat1, empty_cell_string = NA, warn_print = TRUE) 
> Warning: FROM saferMatrix::mat_fill().
> 
> (1) ACCORDING TO empty_cell_string ARGUMENT (NA), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART
> $mat
>      [,1] [,2] [,3] [,4]
> [1,]    1    0   NA    5
> [2,]    1    2    3    0
> [3,]    1    3    0    0
> [4,]    2    0    0    0
> 
> $warn
> [1] "(1) ACCORDING TO empty_cell_string ARGUMENT (NA), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART"

mat_fill(mat = mat4, empty_cell_string = 0, warn_print = TRUE) 
>  [1m [33mError [39m: [22m
>  [33m! [39m 
> 
> ================
> 
> ERROR IN saferMatrix::mat_fill().
> 
> ACCORDING TO empty_cell_string ARGUMENT (0), mat ARGUMENT MATRIX HAS MORE THAN ONE EMPTY HALF PART (ACCORDING TO THE GRAND DIAGONAL): topleft bottomright
> 
> ================
> 
> IN ADDITION
> WARNINGS:
> 
> (1) EMPTY SECTOR DETECTED ON THE TOPLEFT CORNER, FULL OF 0
> 
> (2) EMPTY SECTOR DETECTED ON THE BOTTOMRIGHT CORNER, FULL OF 0
```
