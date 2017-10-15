
#' @export
balance <- function(...) {
  UseMethod("balance")
}

#' @rdname balance
#' @export
balance.default <- function(x, y, measures="all", ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.balance()
  }

  measures <- match.arg(measures, ls.balance(), TRUE)

  sapply(measures, function(f) {
    eval(call(f, y=y))
  })
}

#' @rdname balance
#' @export
balance.formula <- function(formula, data, measures="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  balance.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

#' @export
ls.balance <- function() {
  c("C1", "C2")
}

C1 <- function(y) {
  c <- -1/log(nlevels(y))
  i <- table(y)/length(y)
  aux <- c*sum(i*log(i))
  return(aux)
}

C2 <- function(y) {
  ii <- summary(y)
  nc <- length(ii)
  aux <- ((nc - 1)/nc) * sum(ii/(length(y) - ii))
  return(aux)
}
