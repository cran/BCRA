\name{absolute.risk}
\alias{absolute.risk}
\title{
Estimate absolute risks
}
\description{
A function to estimate absolute risks of developing breast cancer
}
\usage{
absolute.risk(data, Raw_Ind=1, Avg_White=0)
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
  \item{Avg_White}{Calculation indicator.
  \code{Avg_White}=0, calculate absolute risks; 
  \code{Avg_White}=1, calculate average absolute risks based on the rates for average non-hispanic white women and average other (native american) women. 
   The default value is 0.
   }
}
\details{
    For the projection of absolute risks, this function is defined based on Gail Model. 
    Parameters and constants needed in this function include initial and projection age, 
    recoded covariates from function \code{recode.check}, relative risks of BrCa at age
    "<50" and ">=50" from function \code{relative.risk} as well as other known constants
    like BrCa composite incidences, competing hazards, 1-attributable risk using in NCI 
    BrCa Risk Assessment Tool (NCI BCRAT).   
}
\value{
  A vector which returns absolute risk values when Avg_White=0 or average absolute risk values 
  when Avg_White=1.
}
\seealso{
 \code{\link{recode.check}}, \code{\link{relative.risk}}
}
\examples{
data(exampledata)
# calculate absolute risk
absolute.risk(exampledata)
# calculate average absolute risk
Avg_White <- 1
absolute.risk(exampledata, Raw_Ind=1, Avg_White)
}