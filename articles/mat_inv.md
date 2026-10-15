# mat_inv

\
[`library`](https://rdrr.io/r/base/library.html)`(``saferMatrix``)`

\

### Datasets

\
`mat1`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``,``0``,``0``,``0``,``0``,``0``,``0``,``0``,``0``)``, ncol ``=`` ``3``)`\
`mat2`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``,``Inf``,``5``,``9``,``8``,``9``)``, ncol ``=`` ``3``)`\
`mat3`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1``,``2``,``NA``,``5``,``9``,``8``,``9``)``, ncol ``=`` ``3``)`\
`mat4`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``2``)``, ncol ``=`` ``1``)`\
`mat5`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(``0``, ncol ``=`` ``1``)`

\

### Datasets info

``` r
mat1
>      [,1] [,2] [,3]
> [1,]    0    0    0
> [2,]    0    0    0
> [3,]    0    0    0
mat2
>      [,1] [,2] [,3]
> [1,]    1    2    9
> [2,]    1  Inf    8
> [3,]    1    5    9
mat3
>      [,1] [,2] [,3]
> [1,]    1    2    9
> [2,]    1   NA    8
> [3,]    1    5    9
mat4
>      [,1]
> [1,]    1
> [2,]    2
mat5
>      [,1]
> [1,]    0
```

\

### All the arguments

``` r
mat_inv(mat = mat1) # use the trick
>      [,1] [,2] [,3]
> [1,]    0    0    0
> [2,]    0    0    0
> [3,]    0    0    0
```

\

### Error examples

``` r
# examples with errors because of Inf and NA
mat_inv(mat = mat2)
>  [1m [33mError [39m: [22m
>  [33m! [39m 
> 
> ================
> 
> ERROR INSIDE saferMatrix::mat_inv().
> 
> THE mat ARGUMENT MUST BE MADE OF NON INFINITE VALUES.
> 
> ================
mat_inv(mat = mat3)
>  [1m [33mError [39m: [22m
>  [33m! [39m 
> 
> ================
> 
> ERROR INSIDE saferMatrix::mat_inv().
> 
> THE mat ARGUMENT CONTAINS NA WHILE NOT AUTHORIZED.
> 
> ================

# example with error because not a square matrix
mat_inv(mat = mat4)
>  [1m [33mError [39m: [22m
>  [33m! [39m 
> 
> ================
> 
> ERROR IN saferMatrix::mat_inv().
> 
> mat ARGUMENT MUST BE A SQUARE MATRIX
> 
> ================

# example with error because cannot be a square matrix made of a single case of 0
mat_inv(mat = mat5) 
>  [1m [33mError [39m: [22m
>  [33m! [39m 
> 
> ================
> 
> ERROR IN saferMatrix::mat_inv().
> 
> mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE OF 0
> 
> ================
```
