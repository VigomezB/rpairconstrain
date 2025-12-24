#' Randomly sample values under an inequality constraint
#' (c) 2025 by Leonardo Hansa, Adrian Manzanal Oliva, and Víctor M. Gómez-Blanco
#'
#'
#' Purpose: randomly sample values under an inequality constraint (with replacement)
#' For each element of `x`, randomly samples one value from `y` that satisfies
#' `sampled >= x[i]` (default) or `sampled > x[i]` (`strict = TRUE`). Sampling
#' is performed with replacement, so values in `y` may be reused.
#'
#' @param x Numeric vector to be matched (typically the lower component).
#' @param y Numeric vector providing candidate matches (typically the upper component).
#' @param strict Logical. If `TRUE`, enforce `sampled > x[i]`. If `FALSE`, enforce `sampled >= x[i]`.
#' @param na_rm Logical. If `TRUE` (default), remove `NA` values from `x` and `y`.
#'
#' @return A data frame with columns `x` and `sampled`.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(10, 20, 30)
#' y <- c(10, 20, 30, 40, 50)
#' out <- random_sample_greater(x, y, strict = FALSE)
#' stopifnot(all(out$sampled >= out$x))
#'
#' set.seed(123)
#' out2 <- random_sample_greater(x, y, strict = TRUE)
#' stopifnot(all(out2$sampled > out2$x))
#'
random_sample_greater <- function(x, y, strict = FALSE, na_rm = TRUE) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` must be numeric vectors.", call. = FALSE)
  }

  if (na_rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  if (length(x) == 0) {
    stop("`x` must contain at least one value.", call. = FALSE)
  }
  if (length(y) == 0) {
    stop("`y` must contain at least one non-missing value.", call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop("`x` must contain only finite values.", call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop("`y` must contain only finite values.", call. = FALSE)
  }

  cmp <- if (strict) `>` else `>=`

  feasible <- vapply(x, function(xi) any(cmp(y, xi)), logical(1))
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


#' Randomly match values under an inequality constraint (without replacement)
#'
#' Iterates over `x` and attempts to replace each `x[i]` with one value from `y`
#' satisfying the constraint (`>=` by default, or `>` if `strict = TRUE`).
#' Sampling is done **without replacement**: once a `y` value is used, it is removed.
#'
#' if no feasible value exists for a given `x[i]`, it leaves `x[i]` unchanged.
#'
#' @param x Numeric vector to be matched.
#' @param y Numeric vector providing candidate matches (finite pool).
#' @param strict Logical. If `TRUE`, enforce `>`; otherwise enforce `>=`.
#' @param max_tries Integer. Maximum number of random draws per `x[i]` before giving up
#'   and leaving `x[i]` unchanged.
#' @param na_rm Logical. If `TRUE` (default), remove `NA` values from `x` and `y`.
#'
#' @return A data frame with columns `original_x` and `matched_x`.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(10, 20, 30, 40)
#' y <- c(10, 20, 30, 40, 50)
#' out <- random_sample_greater_no_replace(x, y, strict = FALSE, max_tries = 1000)
#' stopifnot(all(out$matched_x >= out$original_x))
#' stopifnot(length(unique(out$matched_x)) == length(out$matched_x))
#'
random_sample_greater_no_replace <- function(x,
                                             y,
                                             strict = FALSE,
                                             max_tries = 1000L,
                                             na_rm = TRUE) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` must be numeric vectors.", call. = FALSE)
  }
  if (!is.numeric(max_tries) || length(max_tries) != 1 || max_tries < 1) {
    stop("`max_tries` must be a single integer >= 1.", call. = FALSE)
  }
  max_tries <- as.integer(max_tries)

  if (na_rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  if (length(x) == 0) {
    stop("`x` must contain at least one value.", call. = FALSE)
  }
  if (length(y) == 0) {
    stop("`y` must contain at least one non-missing value.", call. = FALSE)
  }
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    stop("`x` and `y` must contain only finite values.", call. = FALSE)
  }

  cmp <- if (strict) `>` else `>=`

  # Work on copies
  x_out <- x
  y_pool <- y

  # Helper: attempt to replace x_out[i] by randomly drawing from y_pool
  replace_one <- function(i, x_out, y_pool) {
    # If no feasible candidate exists, leave unchanged
    if (!any(cmp(y_pool, x_out[i]))) {
      return(list(x_out = x_out, y_pool = y_pool))
    }

    tries <- 0L
    repeat {
      tries <- tries + 1L
      if (tries > max_tries) {
        # Give up for this i (same behavior as "leave unchanged")
        return(list(x_out = x_out, y_pool = y_pool))
      }

      j <- sample.int(length(y_pool), 1)
      if (cmp(y_pool[j], x_out[i])) {
        x_out[i] <- y_pool[j]
        y_pool <- y_pool[-j]
        return(list(x_out = x_out, y_pool = y_pool))
      }
    }
  }

  for (i in seq_along(x_out)) {
    res <- replace_one(i, x_out, y_pool)
    x_out <- res$x_out
    y_pool <- res$y_pool
  }

  data.frame(original_x = x, matched_x = x_out)
}

