% --- Source file: error.table.Rd ---
\name{error.table}
\alias{error.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   List the records and errors for IDs with missing absolute risks 
}
\description{
   A function to list the records and errors for IDs with missing absolute risks.   
   For each of the records with error, the record is listed followed by a line which gives
some indication as to where the error occured. Relative risks and risk pattern numbers are also included. 
}
\usage{
error.table(data, Raw_Ind=1)
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
   A data frame listing the raw records, errors, relative risks and pattern numbers for IDs with missing absolute risks. 
If there is nothing wrong with the input data, the function will return "NO ERROR!".
}

\seealso{
  \code{\link{recode.check}}, \code{\link{error.table.all}}
}

