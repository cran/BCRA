% --- Source file: relative.risk.Rd ---
\name{relative.risk}
\alias{relative.risk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate relative risks
}
\description{
A function to estimate relative risks for risk factor combinations
}
\usage{
relative.risk(data, Raw_Ind=1)
}
\arguments{
  \item{data}{
   A data set containing all the required input data needed to perform risk projections, 
   such as initial age, projection age, BrCa relative risk covariates and race. See \code{exampledata} for details.
   }
  \item{Raw_Ind}{
   The raw file indicator with default value 1.
   \code{Raw_Ind}=1 means RR covariates are in raw/original format.
   \code{Raw_Ind}=0 means RR covariates have already been re-coded to 0, 1, 2 or 3.
   }
}
\details{
   The age is dichotomized as "age less than 50 years" and "age 50 years or more".
   The relative risks can be obtained from Gail Model, an unconditional logistic regression that included main effects
\code{NB_Cat}, \code{AM_Cat}, \code{AF_Cat}, \code{NR_Cat} as well as interactions between \code{AF_Cat} and 
\code{NR_Cat} and between the age category and \code{NR_Cat}. 
}
\value{
   \item{RR_Star1}{Relative risk for woman of interest at ages < 50.}
   \item{RR_Star2}{Relative risk for woman of interest at ages >= 50.}
   \item{PatternNumber}{The sequence number of risk patterns. There are 3 levels for \code{NB_Cat}, 
   3 for \code{AM_Cat}, 4 for \code{AF_Cat}, 3 for \code{NR_Cat}, 3*3*4*3 = 108 patterns in total.
   Pattern Number=NB_Cat*3*3*4+AM_Cat*3*4+AF_Cat*3+NR_Cat*1+1.}
}
\seealso{
  \code{\link{recode.check}} 
}

\examples{
data(exampledata)
relative.risk(exampledata)
}
