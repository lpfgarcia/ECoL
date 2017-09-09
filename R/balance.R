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

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.balance()
  }

  measures <- match.arg(measures, ls.balance(), TRUE)

  data <- data.frame(x, class=y)

  sapply(measures, function(f) {
    eval(call(f, data=data))
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
  c("c1", "c2")
}

c1 <- function(data) {
  c <- (-1/log(nlevels(data$class)))
  i <- table(data$class)/nrow(data)
  aux <- c*sum(i*log(i))
  return(aux)
}

c2 <- function(data) {
  aux <- summary(data$class)
  return(min(aux)/max(aux))
}

c3 <- function(data) {

  nc <- nlevels(data$class)
  ii <- summary(data$class)
  aux <- ((nc - 1)/nc) * sum(ii/(nrow(data) - ii))
  return(aux)
}

