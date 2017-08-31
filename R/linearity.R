
#' @export
linearity <- function(...) {
  UseMethod("linearity")
}

#' @rdname linearity
#' @export
linearity.default <- function(x, y, measures="all", ...) {
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
    measures <- ls.linearity()
  }

  measures <- match.arg(measures, ls.linearity(), TRUE)

  data <- data.frame(x, class=y)
  data <- binarize(data)
  data <- ovo(data)

  model <- lapply(data, smo)

  sapply(measures, function(f) {
    eval(call(f, model=model, data=data))
  })
}

#' @rdname linearity
#' @export
linearity.formula <- function(formula, data, measures="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  linearity.default(modFrame[, -1], modFrame[, 1], measures, ...)
}

#' @export
ls.linearity <- function() {
  c("l1", "l2", "l3")
}

smo <- function(data) {
  e1071::svm(class ~ ., data, scale=TRUE, kernel="linear")
}

hyperretangle <- function(data) {
  data <- data[,-ncol(data), drop=FALSE]
  aux <- sqrt(sum((colMax(data)-colMin(data))^2))
  return(aux)
}

l1 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- predict(m, d, decision.values=TRUE)
    err <- rownames(d[prd != d$class,])
    dst <- attr(prd, "decision.values")[err,]
    sum(abs(dst))/(nrow(d)*hyperretangle(d))
  }, m=model, d=data)

  aux <- mean(aux)
  return(aux)
}

error <- function(pred, class) {
  1 - sum(diag(table(class, pred)))/sum(table(class, pred))
}

l2 <- function(model, data) {

  aux <- mapply(function(m, d) {
    pred <- predict(m, d)
    error(pred, d$class)
  }, m=model, d=data)

  return(mean(aux))
}

l3 <- function(model, data) {

  aux <- mapply(function(m, d) {
    tmp <- generate(d)
    pred <- predict(m, tmp)
    error(pred, tmp$class)
  }, m=model, d=data)

  return(mean(aux))
}

