% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pred.values.R
\name{pred.values}
\alias{pred.values}
\title{pred.values}
\usage{
pred.values(y_train, y_test, pred_train, pred_test)
}
\arguments{
\item{y_train}{label vector for training data}

\item{y_test}{label vector for test data}

\item{pred_train}{predicted probabilities for training data}

\item{pred_test}{predicted probabilities for test data}
}
\value{
\item{AUC_train}{AUC for training data}

\item{AUC_test}{ AUC for test data}

\item{accuracy_test}{ accuracy for test data}

\item{precision_test}{ precision for test data}

\item{recall_test}{ recall for test data}

\item{F1_test}{ F1 score for test data}

\item{thre}{ threshold to separate two labels, obtained from training data}
}
\description{
This function performs some statistical value prediction
}
\examples{
y_train<-c(0,1,0,1,0,1)
y_test<-c(0,1,0,1)
pred_train<-c(0.77,0.89,0.32,0.96,0.10,0.67)
pred_test<-c(0.68,0.75,0.50,0.81)
re<-pred.values(y_train,y_test,pred_train,pred_test)
print(re)

}
\keyword{predict}
