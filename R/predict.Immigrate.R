#' predict.Immigrate
#'
#' This function performs the predition for Immigrate(Iterative Max-Min Entropy Margin-Maximization with Interaction Terms) algorithm.
#' @param object result of Immigrate algorithm
#' @param xx model matrix of explanatory variables
#' @param yy label vector 
#' @param newx model matrix to be preded 
#' @param sig sigma used in algorithm, default to be 1
#' @param type the form of final output, default to be "both"
#' @param ... further arguments passed to or from other methods
#' @keywords predict Immigrate
#' @return \item{response}{preded probabilities for newx}
#' @return \item{class}{preded class for newx}
#' @importFrom stats predict

#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' index<-c(1:floor(nrow(xx)*0.3))
#' train_xx<-xx[-index,]
#' test_xx<-xx[index,]
#' train_yy<-yy[-index]
#' test_yy<-yy[index]
#' re<-Immigrate(train_xx,train_yy)
#' res<-predict(re,train_xx,train_yy,test_xx,type="class")
#' print(res)
#' 
predict.Immigrate<-function(object,xx,yy,newx,sig = 1, type = "both",...){
  TYPES <- c("both","response","class") 
  typeIdx <- pmatch(type, TYPES)
  if (is.na(typeIdx)){
    stop("Invalid type")
  }
  yy<-as.numeric(yy)
  if ((!(is.matrix(newx)))&length(newx)==ncol(xx)){
    newx<-matrix(newx,nrow = 1)
  }
  if (ncol(xx) != ncol(newx)){
    stop("xx and newx have different lengths of explanatory variables")
  }
  if(is.list(object)){
    w<-object$w
  }else{
    w<-object
  }
  label<-unique(yy)
  v<-sapply(c(1:length(label)),function(j){
    sapply(c(1:nrow(newx)), function(i){
      yyy<-abs(yy-label[j])
      y<-rep(0,length(yyy))
      y[which(yyy==0)]<-1
      tmp<-matrix((y)*exp(-(rowSums(crossprod(abs(t(xx)-as.numeric(newx[i,])),w)*
                                      t(abs(t(xx)-as.numeric(newx[i,]))))/sig) ))[,1]
      s<-sum(tmp)
      if(s!=0) tmp<-tmp/s
      rowSums(crossprod(abs(t(xx)-as.numeric(newx[i,])),w)*
                t(abs(t(xx)-as.numeric(newx[i,]))))%*%tmp
    })
  })
  
  if (nrow(newx)==1){
    v<-v/sum(v)
    pred<-label[which.min(v)]
  }else{ 
  myfun<-sapply(c(1:nrow(newx)), function(i){
    v[i,]<<-v[i,]/sum(v[i,])
  })
  pred<-sapply(c(1:nrow(newx)),function(i){
    label[which.min(v[i,])]
  })
  }
  if (type == "both"){
    newList<-list("class"=pred,"prob"=v)
    return(newList) 
  }else if(type == "response"){
    return(v)
  }else if(type == "class"){
    return(pred)
  }else{
    stop("use wrong type")
  }
  
}
