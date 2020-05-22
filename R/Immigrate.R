

#' Immigrate
#'
#' This function performs Immigrate(Iterative Max-Min Entropy Margin-Maximization with Interaction Terms ) algorithm.
#' @param xx model matrix of explanatory variables
#' @param yy label vector
#' @param w0 initial weights
#' @param epsilon criterion for stopping iteration
#' @param sig sigma used in algorithm, default to be 1
#' @param max_iter maximum number of iteration 
#' @param removesmall whether to remove features with small weights, default to be FALSE
#' @param randomw0 whether to use randomly initial weights, default to be FALSE
#' @keywords Immigrate
#' @return \item{w}{new weight after one loop}
#' @return \item{iter_num}{ number of iteration for convergence}
#' @return \item{final_c}{final cost}
#' @import Rcpp
#' @importFrom stats runif

#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' re<-Immigrate(xx,yy)
#' print(re)
#' @references Zhao R, Hong P, Liu J S. IMMIGRATE: A Margin-based Feature Selection Method with Interaction Terms[J]. arXiv preprint arXiv:1810.02658, 2018.
#' @seealso \url{https://arxiv.org/abs/1810.02658} for more details.
Immigrate<-function(xx,yy,w0,epsilon=0.01,
                    sig=1, max_iter=10,removesmall=FALSE,randomw0 = FALSE){
  p<-ncol(xx)
  if (missing(w0)){
    if (randomw0){
      w0 <-matrix(runif(p*p),p,p)
    }else{
      w0 <-diag(1,p,p)
    }
    w0<-w0/sqrt(sum(w0^2))
  }
  suppressWarnings(
  res<-ImmigrateCpp(oneImmigrate=one.Immigrate,xx,yy,w0 = w0,epsilon=epsilon,
                      sig=sig, max_iter=max_iter,removesmall=removesmall))
  class(res)<-"Immigrate"
  return(res)
}
