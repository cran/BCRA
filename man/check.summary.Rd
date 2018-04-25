\name{check.summary}
\alias{check.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Summarize the error indicators, relative risks and absolute risks
}
\description{
    A function to show descriptive statistics by applying function \code{mean} and \code{sd} 
to the quantities Error_Ind, AbsRisk, RR_Star1 and RR_Star2. 
}
\usage{
check.summary(data, Raw_Ind=1, Avg_White=0)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{Avg_White}{Calculation indicator.
  \code{Avg_White}=0, calculate absolute risks; 
  \code{Avg_White}=1, calculate average absolute risks based on the rates for average non-hispanic white women and average other (native american) women. 
   The default value is 0.
   }
}

\details{
    When the mean and standard deviation for the variable \code{Error_Ind} is 0, implies that
no errors have not been found. Otherwise when the mean and std for \code{Error_Ind} is not 0,
implies that errors have been found. When errors are found, the number of records with
errors is the count asscociated with \code{AbsRisk} listed under NMiss (number of missing). 
}
\value{
   A summary table for error indicators, relative risks and absolute risks
}
\seealso{
  \code{\link{recode.check}}, \code{\link{relative.risk}}, \code{\link{absolute.risk}}  
}