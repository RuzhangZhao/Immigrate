

#' BIM
#'
#' This function performs BIM algorithm.
#' @param xx model matrix of explanatory variables
#' @param yy label vector
#' @param nBoost number of classifiers in BIM, default to be 3
#' @param max_iter maximum number of iteration for IMMIRGATE classifier, default to be 5
#' @param removesmall whether remove features with small weights, default to be FALSE
#' @param sigstart start of sigma used in algorithm, default to be 0.02
#' @param sigend end of sigma used in algorithm, default to be 4
#' @keywords BIM
#' @return \item{matrix}{list of matrix weights}
#' @return \item{weights}{coefficient vector for classifiers}
#' @return \item{sample_wt}{sample weights}
#' @import Rcpp

#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' re<-BIM(xx,yy)
#' 
BIM<-function(xx, yy, nBoost = 3, max_iter = 5, removesmall = FALSE, sigstart=0.02, sigend=4){
  suppressWarnings(
    res<-(BIMCpp(oneboostImmigrate = one.boost.Immigrate, xx, yy, 
                        nIter = nBoost, max_iter = max_iter, removesmall = removesmall, sigstart=sigstart, sigend=sigend)))
  class(res)<-"BIM"
  return(res)
}
