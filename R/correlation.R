#' Measures of feature correlation
#'
#' Regression task. These measures calculate the correlation of the values of 
#' the features to the outputs. If at least one feature is highly correlated to 
#' the output, this indicates that simpler functions can be fitted to the data.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A response vector with one value for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the output column.
#' @param data A data.frame dataset contained the input and output attributes.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{summarization} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"C1"}{Maximum feature correlation to the output (C1) calculate the 
#'      maximum absolute value of the Spearman correlation between each feature 
#'      and the outputs.}
#'    \item{"C2"}{Average feature correlation to the output (C2) computes the 
#'      average of the Spearman correlations of all features to the output.}
#'    \item{"C3"}{Individual feature efficiency (C3) calculates, for each 
#'      feature, the number of examples that must be removed from the dataset 
#'      until a high Spearman correlation value to the output is achieved.}
#'    \item{"C4"}{Collective feature efficiency (C4) computes the ratio of 
#'      examples removed from the dataset based on an iterative process of 
#'      linear fitting between the features and the target attribute.}
#'  }
#' @return A list named by the requested correlation measure.
#'
#' @references
#'  Ana C Lorena and Aron I Maciel and Pericles B C Miranda and Ivan G Costa and
#'    Ricardo B C Prudencio. (2018). Data complexity meta-features for 
#'    regression problems. Machine Learning, 107, 1, 209--246.
#'
#' @examples
#' ## Extract all correlation measures for regression task
#' data(cars)
#' correlation(speed ~ ., cars)
#' @export
correlation <- function(...) {
  UseMethod("correlation")
}

#' @rdname correlation
#' @export
correlation.default <- function(x, y, measures="all", summary=c("mean", "sd"), 
                                ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  if(is.factor(y)) {
    stop("label attribute needs to be numeric")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.correlation()
  }

  measures <- match.arg(measures, ls.correlation(), TRUE)

  if (length(summary) == 0) {
    summary <- "return"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  x <- normalize(binarize(x))
  y <- normalize(y)[,1]

  sapply(measures, function(f) {
    measure = eval(call(paste("r", f, sep="."), x=x, y=y))
    summarization(measure, summary, f %in% ls.correlation.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname correlation
#' @export
correlation.formula <- function(formula, data, measures="all", 
                                summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  correlation.default(modFrame[, -1,drop=FALSE], modFrame[, 1,drop=FALSE],
    measures, summary, ...)
}

ls.correlation <- function() {
  c("C2", "C3", "C4")
}

ls.correlation.multiples <- function() {
  c("C2", "C3")
}

r.C2 <- function(x, y) {
  #mean(abs(stats::cor(x, y, method="spearman")))
  as.numeric(abs(stats::cor(x, y, method="spearman")))
}

remove <- function(x, y, c) {

  remainingRows <- length(x)
  xorder <- rank(x)
  yorder <- rank(y)

  diff <- xorder - yorder
  correlation <- spearman(diff)

  if(correlation < 0) {
    yorder <- rank(-y)
    diff <- xorder - yorder
    correlation <- spearman(diff)
  }

  while(abs(correlation) < c && !is.na(correlation)) {

    maxPosition <- which.max(abs(diff))

    diff <- diff + ((yorder > yorder[maxPosition]) - 
      (xorder > xorder[maxPosition]))

    yorder <- yorder[-maxPosition]
    xorder <- xorder[-maxPosition]
    diff <- diff[-maxPosition]
    remainingRows <- remainingRows - 1
    correlation <- spearman(diff)

    if(is.na(correlation))
      correlation
  }

  (length(x) - remainingRows)/length(x)
}

r.C3 <- function(x, y, c=0.9) {
  #min(apply(x, 2, remove, y, c))
  as.numeric(apply(x, 2, remove, y, c))
}

r.C4 <- function(x, y, r=0.1) {

  aux <- 0
  rows <- nrow(x)

  repeat {

    aux <- aux + 1
    tmp <- stats::cor(y, x, method="spearman")
    idx <- rev(order(abs(tmp)))[1]

    model <- stats::lm(y ~ x[,idx])
    remove <- abs(model$residuals) > r

    x <- x[remove,,drop=FALSE]
    y <- y[remove]

    if(length(y) <= 1)
      return(0)

    if(sum(!remove) == length(remove) | aux == ncol(x))
      break
  }

  return(length(y)/rows)
}
