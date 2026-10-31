# saferMatrix 1.0.0

* Backbone v21.1: the invented `x` argument names (absent from the called function signature, for instance `base::any(x = ...)`, `base::all(x = ...)`, `base::sum(x = ...)`, `base::order(x = ...)`) are removed from all the function calls.
* Backbone v21.2: the lib_path argument is now checked whatever the value of safer_check (the `if(safer_check == TRUE){` and `}` lines around the check of lib_path are commented, because lib_path is used outside the safer_check checkings, for instance in the `saferDev::arg_check()` calls): an incorrect lib_path now returns an error even with `safer_check = FALSE`. The documentation of the safer_check argument no longer lists the lib_path check, and the documentation of the lib_path argument no longer says that lib_path is ignored if safer_check is FALSE.
* Migrated to the safer-r backbone v21.0: all 5 functions (mat_fill, mat_inv, mat_num2color, mat_op, mat_rotate) now follow the saferDev 2.0.0 structure, with arg_check() v2.0.0 semantics (exact class/typeof/mode comparison; class = c("matrix", "array") for matrices, prop = TRUE for [0,1] proportion arguments, mode = "numeric" preferred for numeric arguments).
* All functions carry the mandatory lib_path, safer_check and error_text reserved arguments.
* Internal functions (.base_op_check, .pack_and_function_check) are now imported from saferDev (>= 2.0.0) instead of being duplicated locally; internal_functions.R and its man page removed.
* Bug fixes found during the migration: mat_op now restores row and column names only when identical across all the matrices of mat_list; mat_num2color no longer crashes on integer matrices with mat_hsv_h = TRUE (clear error message instead).
* Complete test suite rewritten in the saferDev style (tests/testthat), including a test-mandatory_args.R replicating the saferDev pattern; code coverage brought to 100% with # nocov markers on unreachable internal-error branches.

# saferMatrix 1.0

* Initial version (backbone v19.0).
