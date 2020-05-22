#' predict.BIM
#'
#' This function performs the predition for BIM algorithm.
#' @param object result of BIM algorithm
#' @param xx model matrix of explanatory variables
#' @param yy label vector
#' @param newx test data
#' @param type the form of final output
#' @param ... further arguments passed to or from other methods
#' @keywords predict BIM
#' @return \item{response}{preded probabilities for xx}
#' @return \item{class}{preded class for xx}
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
#' re<-BIM(train_xx,train_yy)
#' res<-predict(re,train_xx,train_yy,test_xx,type="class")
#' print(res)
#' 

predict.BIM<-function(object,xx,yy,newx,type = "both",...){
  TYPES <- c("both","response","class") 
  typeIdx <- pmatch(type, TYPES)
  if (is.na(typeIdx)){
    stop("Invalid type")
  }
  if ("weights" %in% names(object)){
    label<-unique(yy)
    nIter <- length(object$weights)
    num_examples = nrow(newx)
    predict_class<-matrix(0,nrow = num_examples,ncol = nIter)
    predict_class2<-matrix(0,nrow = num_examples,ncol = nIter)
    
    myfun<-sapply(c(1:nIter),function(i){
      class(object$matrix[[i]])<-"Immigrate"
      tmp <- predict(object$matrix[[i]],xx,yy, newx, sig = object$sig[i],type= "response")
      predict_class[,i]<<-tmp[,1]
      predict_class2[,i]<<-tmp[,2]
    })
    v<-sapply(c(1:num_examples),function(i){
      c(predict_class[i,]%*%object$weights,predict_class2[i,]%*%object$weights)
    })
    v<-t(v)
    myfun<-sapply(c(1:num_examples), function(i){
      v[i,]<<-v[i,]/sum(v[i,])
    })
    pred<-sapply(c(1:num_examples),function(i){
      label[which.min(v[i,])]
    })
    if (missing(type)){
      newList<-list("class"=pred,"prob"=v)
      return(newList) 
    }else if(type == "response"){
      return(v)
    }else if(type == "class"){
      return(pred)
    }else{
      stop("use wrong type")
    }
  }else{
    label<-unique(yy)
    nIter <- length(object$matrix)
    num_examples = nrow(newx)
    myfun<-sapply(c(1:nIter),function(i){
      class(object$matrix[[i]])<-"Immigrate"
      predict(object$matrix[[i]],xx,yy, newx, sig = object$sig[i], type= "response")[,1]
    })
    v<-(cbind(rowSums(myfun)/nIter,1-rowSums(myfun)/nIter))
    pred<-sapply(c(1:num_examples),function(i){
      label[which.min(v[i,])]
    })
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
}

