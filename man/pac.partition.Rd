% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.r
\name{pac.partition}
\alias{pac.partition}
\title{Machine learning data partition}
\usage{
pac.partition(x, y, l, train_size = 0.7, rand_state = sample(1:2^15, 1))
}
\arguments{
\item{x, y}{Numeric data}

\item{l}{Numeric labels data}

\item{train_size}{Fraction of total data that the SVM will train on}

\item{rand_state}{Value of the random state used to set the seed}
}
\value{
Two data frames and a list of indicies for the training set
}
\description{
A method of partitioning data between training and testing sets based on the fraction of data used for training
}
\keyword{data}
\keyword{machine-learning}
