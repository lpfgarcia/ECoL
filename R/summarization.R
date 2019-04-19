#' Post processing complexity measures
#'
#' Post-processing alternatives to deal with multiples values. This method is 
#' used by the complexity measures to summarize the obtained values.
#'
#' @param measure A list with the complexity measures values.
#' @param summary The functions to post processing the data. See the details
#'   to more information. Default: \code{c("mean", "sd")}
#' @param multiple A logical value defining if the measure should return
#'   multiple values. (Default: \code{TRUE})
#' @param ... Extra values used to the functions of summarization.
#' @details
#'  The post processing functions are used to summarize the complexity measures.
#'  They are organized into three groups: return, descriptive statistic and 
#'  distribution. Currently, the hypothesis testing post processing are not 
#'  supported.
#'
#'  In practice, there are no difference among the types, so that more than one
#'  type and functions can be combined. Usually, these function are used to
#'  summarize a set of values for each complexity measures. For instance, a 
#'  measure computed for each attribute can be summarized using the 
#'  \code{"mean"} and/or \code{"sd"}.
#'
#'  In addition to the native functions available in R, the following functions
#'  can be used:
#'  \describe{
#'    \item{"histogram"}{Computes a histogram of the given data value. The extra
#'       parameters '\code{bins}' can be used to define the number of values to
#'       be returned. The parameters '\code{max}' and '\code{min}' are used to
#'       define the range of the data. The default value for these parameters
#'       are respectively \code{10, min(x)} and \code{max(x)}.}
#'    \item{"kurtosis"}{See \code{\link[e1071]{kurtosis}}}
#'    \item{"max"}{See \code{\link{max}}}
#'    \item{"mean"}{See \code{\link{mean}}}
#'    \item{"median"}{See \code{\link{median}}}
#'    \item{"min"}{See \code{\link{min}}}
#'    \item{"quantiles"}{See \code{\link{quantile}}}
#'    \item{"sd"}{See \code{\link{sd}}}
#'    \item{"skewness"}{See \code{\link[e1071]{skewness}}}
#'    \item{"var"}{See \code{\link{var}}}
#'    \item{"return"}{Returns the original value(s) of the complexity measure.}
#'  }
#'  These functions are not restrictive, thus another functions can be applied
#'  as post-processing summarization function.
#'
#' @return A list with the post-processed complexity measures.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat 
#'    Ramon Llull.
#'
#' @examples
#' summarization(runif(15))
#' summarization(runif(15), c("min", "max"))
#' summarization(runif(15), c("quantiles", "skewness"))
#' @export
summarization <- function(measure, summary=c("mean", "sd"), multiple=TRUE, 
                          ...) {

  if(length(measure) == 0) {
    return(NA)
  }

  if(!multiple) {
    if(length(measure) > 1) {
      warning("More than one value was obtained for a single measure")
    }
    measure = as.numeric(measure[1])
    return(measure)
  }

  measure = measure[is.finite(measure)]

  res = sapply(summary, function(s) {
    do.call(s, list(measure, ...))
  }, simplify=FALSE)

  unlist(res)
}

skewness <- function(x, na.rm=FALSE, type=3, ...) {
  e1071::skewness(x, na.rm, type)
}

kurtosis <- function(x, na.rm=FALSE, type=3, ...) {
  e1071::kurtosis(x, na.rm, type)
}

quantiles <- function(x, type=1, ...) {
  tryCatch(
    stats::quantile(x, type=type, ...),
    error=function(e) stats::quantile(NA, na.rm=TRUE, ...)
  )
}

iqr <- function(x, na.rm=FALSE, ...) {
   if (!na.rm & any(is.na(x))) NA
   else stats::IQR(x, na.rm = na.rm)
}

histogram <- function(x, bins=10, min=base::min(x, na.rm=TRUE),
                 max=base::max(x, na.rm=TRUE), ...) {
  breaks <- seq(ifelse(is.finite(min), min, 0),
                ifelse(is.finite(max), max, bins), length.out=bins + 1)
  graphics::hist(as.numeric(x), breaks=breaks, plot=FALSE)$counts / length(x)
}
