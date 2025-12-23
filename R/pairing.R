#' Randomly sample values under an inequality constraint (with replacement)
#'
#' For each element of `x`, randomly samples one value from `y` that satisfies
#' `sampled >= x[i]` (default) or `sampled > x[i]` (`strict = TRUE`). Sampling
#' is performed with replacement, so values in `y` may be reused.
#'
#' @param x Numeric vector to be matched (typically the lower component).
#' @param y Numeric vector providing candidate matches (typically the upper component).
#' @param strict Logical. If `TRUE`, enforce `sampled > x[i]`. If `FALSE`, enforce `sampled >= x[i]`.
#'
#' @return A data frame with columns `x` and `sampled`.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(10, 20, 30)
#' y <- c(5, 10, 15, 25, 50)
#' out <- random_sample_greater(x, y)
#' stopifnot(all(out$sampled >= out$x))


random_sample_greater <- function(x, y, strict = FALSE) {
  stopifnot(is.numeric(x), is.numeric(y))

  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  if (length(y) == 0) {
    stop("`y` must contain at least one non-missing value.", call. = FALSE)
  }

  cmp <- if (strict) `>` else `>=`

  feasible <- vapply(x, \(xi) any(cmp(y, xi)), logical(1))
  if (any(!feasible)) {
    stop("Some values in `x` cannot be matched with any value in `y`.", call. = FALSE)
  }

  sampled <- vapply(
    x,
    function(xi) {
      candidates <- y[cmp(y, xi)]
      sample(candidates, 1)
    },
    numeric(1)
  )

  data.frame(x = x, sampled = sampled)
}

#' Randomly sample values under an inequality constraint (without replacement)
#'
#' Iterates over `a` and, for each element `a[i]`, randomly selects one value from `b`
#' that satisfies `b[j] >= a[i]` (default) or `b[j] > a[i]` (`strict = TRUE`). The chosen
#' value is removed from `b` so it cannot be reused.
#'
#' If no candidate exists for a given `a[i]`, the function leaves `a[i]` unchanged.
#' If you prefer `NA`, replace `next` with `a_out[i] <- NA_real_`.
#'
#' @param a Numeric vector to be matched (typically the lower component).
#' @param b Numeric vector providing candidate matches (finite pool).
#' @param strict Logical. If `TRUE`, enforce `b[j] > a[i]`. If `FALSE`, enforce `b[j] >= a[i]`.
#'
#' @return A data frame with columns `original_a` and `replaced_a`.
#' @export
#'
#' @examples
#' set.seed(123)
#' a <- c(10, 20, 30)
#' b <- c(10, 20, 25, 35, 50)
#' out <- random_sample_greater_no_replace(a, b)
#' stopifnot(all(out$replaced_a >= out$original_a))


random_sample_greater_no_replace <- function(a, b, strict = FALSE) {
  stopifnot(is.numeric(a), is.numeric(b))

  a <- a[!is.na(a)]
  b <- b[!is.na(b)]

  cmp <- if (strict) `>` else `>=`

  a_out <- a
  b_pool <- b

  for (i in seq_along(a_out)) {
    candidates <- which(cmp(b_pool, a_out[i]))

    if (length(candidates) == 0) {
      next
    }

    j <- sample(candidates, 1)
    a_out[i] <- b_pool[j]
    b_pool <- b_pool[-j]
  }

  data.frame(original_a = a, replaced_a = a_out)
}


