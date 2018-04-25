\name{list.constants}
\alias{list.constants}
\title{
List all constants required for BrCa absolute risk projections
}
\description{
A function to create a text file under user's working directory
which contains all constants required for 
BrCa absolute risk projections.
}
\usage{
list.constants(BrCa_lambda1, BrCa_lambda2, BrCa_beta, BrCa_1_AR)
}
\arguments{
  \item{BrCa_lambda1}{
  Breast Cancer Composite Incidences   
}
  \item{BrCa_lambda2}{
  Breast Cancer Competing Mortality
}
  \item{BrCa_beta}{
  The logistic regression coefficients (beta) derived from the Gail model
}
  \item{BrCa_1_AR}{
  1-Attributable Risk
}
}
\details{
  See "BrCa_lambda1.rda", "BrCa_lambda2.rda", "BrCa_beta.rda", "BrCa_1_AR.rda"
  in package data folder.
}
\value{
   A text file "list_all_constants.txt" exported under user's working 
   directory for reading convenience.
}
