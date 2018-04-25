% --- Source file: risk.summary.Rd ---
\name{risk.summary}
\alias{risk.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   List the records with relative risks and absolute risks
}
\description{
   A function to list all the records with relative risks and absolute risks. 
 
}
\usage{
risk.summary(data, Raw_Ind=1)
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
}
\value{
   A data frame that includes age, duration of the projection time interval, covariates and the projected risk.  
   A CSV file is created to save the data frame under user's working directory for reading convenience.
}

\seealso{
    \code{\link{relative.risk}}, \code{\link{absolute.risk}}  
}
\examples{
data(exampledata)
risk.summary(exampledata)
}