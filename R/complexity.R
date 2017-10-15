
#' @export
complexity <- function(...) {
  UseMethod("complexity")
}

#' @rdname complexity
#' @export
complexity.default <- function(x, y, groups="all", ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(groups[1] == "all") {
    groups <- ls.complexity()
  }

  groups <- match.arg(groups, ls.complexity(), TRUE)
  colnames(x) <- make.names(colnames(x))

  unlist(
    sapply(groups, function(group) {
      do.call(group, list(x=x, y=y, ...))
    }, simplify=FALSE)
  )
}

#' @rdname complexity
#' @export
complexity.formula <- function(formula, data, groups="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  complexity.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    groups, ...)
}

#' @export
ls.complexity <- function() {
  c("overlapping", "neighborhood", "linearity", "dimensionality", "balance",
    "network")
}
