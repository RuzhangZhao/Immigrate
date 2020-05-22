#' predict.LFE
#'
#' This function performs predition for LFE(Local Feature Extraction) algorithm.
#' @param object weight 
#' @param xx model matrix of explanatory variables
#' @param yy label vector 
#' @param newx model matrix to be preded 
#' @param ... further arguments passed to or from other methods
#' @keywords predict LFE
#' @return predicted labels
#' @importFrom stats predict

#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' w<-LFE(xx,yy)
#' pred<-predict(w,xx,yy,xx)
#' print(pred)

predict.LFE<-function(object,xx,yy,newx,...){
  w<-object
  yy<-as.numeric(yy)
  if (ncol(xx) != ncol(newx)){
    stop("xx and newx have different lengths of explanatory variables")
  }
  pred<-sapply(c(1:nrow(newx)), function(i){
    yy[as.numeric(which.min(rowSums(crossprod(abs(t(xx)-as.numeric(newx[i,])),w)*t(abs(t(xx)-as.numeric(newx[i,])))))[1])]
  })
  return(pred)
}
