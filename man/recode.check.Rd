% --- Source file: recode.check.Rd ---
\name{recode.check}
\alias{recode.check}
\title{
Recode and check the relative risk covariate values
}
\description{
A function to recode the relative risk covariates and check errors.
}
\usage{
recode.check(data, Raw_Ind=1)
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
   This function is to recode the following relative risk covariates. Recoded RR covariates
are named as \code{NB_Cat}, \code{AM_Cat}, \code{AF_Cat} and \code{NR_Cat} for \code{N_Biop}, 
\code{AgeMen}, \code{Age1st} and \code{N_Rels}, respectively. 
    \tabular{ll}{
    N_Biop: \tab The number of biopsies. \cr
    AgeMen: \tab Age at menarchy. \cr
    Age1st: \tab Age at first live birth. \cr
    N_Rels: \tab The number of first degree relatives with BrCa. \cr
    }
   See the following table for recoding details.
    \tabular{llr}{
    Covariate \tab Raw Value \tab Recoded to \cr
    N_Biop   \tab 0 or 99 (unk or not applicable) \tab 0 \cr
             \tab 1                               \tab 1 \cr
             \tab 2,3,4 ... and not 99            \tab 2 \cr
             \tab                                 \tab   \cr
    AgeMen   \tab 14,15,16 ... or 99 (unk) \tab 0 \cr
             \tab 12,13                    \tab 1 \cr
             \tab 11 and younger           \tab 2 \cr
             \tab                          \tab   \cr
    Age1st   \tab 19 and younger or 99 (unk) \tab 0 \cr
             \tab 20,21,22,23,24             \tab 1 \cr
             \tab 25,26,27,28,29 or 98 (nulliparous) \tab 2 \cr
             \tab 30,31,32 ... and not 98 and not 99  \tab 3 \cr
             \tab                                    \tab   \cr
    N_Rels   \tab 0 or 99 (unk) \tab 0 \cr
             \tab 1             \tab 1 \cr
             \tab 2,3,4 ... and not 99 \tab 2 \cr
    }

    This function is also used to check consistency and errors of input data.
    Let \code{set_T1_missing} and \code{set_T2_missing} be the checking variables for \code{T1}
    and \code{T2}. 
    The constraint on \code{T1} and \code{T2} is 20<=T1<T2<=90. If it is violated, 
    \code{set_T1_missing} and \code{set_T2_missing} and the absolute risk will be set to
    the missing value \code{NA}.
 
    Let \code{RacCat} be the checking variable for \code{Race}. If the \code{Race} value is not
    included in the 11 races defined, the absolute risk will be set to the missing value \code{NA}
    and \code{RacCat} will be set to "U" (undefined). The corresponding character of Race 
    \code{CharRace} will be set to "??". 

    Let \code{set_HyperP_missing} and \code{set_R_Hyp_missing} be the checking variables for \cr
    \code{HypPlas} and \code{R_Hyp}. Consistency patterns for the number of Biopsies and Hyperplasia are: 
        \tabular{ll}{
    Requirment (A)   \tab \code{N_Biops}=0 or 99, then \code{HypPlas} MUST = 99 (not applicable). \cr
    Requirment (B)   \tab \code{N_Biops}>0 and <99, then \code{HypPlas} = 0, 1 or 99. \cr
    }

    If ANY of the above 2 REQUIREMENTS is violated, \code{NB_Cat}, \code{set_HyperP_missing} and 
    \code{set_R_Hyp_missing} will be set to the corresponding character "A" or "B" and the absolute
    risk will be set to the missing value \code{NA}. 
    The consequences to the relative risk (RR) for the above two requirements are:

    (A) \code{N_Biops}=0 or 99, \code{HypPlas}=99 (not applicable) inflates RR by 1.00.
    
    (B) \code{N_Biops}>0 and <99, \code{HypPlas}=0 (no) inflates RR by 0.93; \cr
        \code{N_Biops}>0 and <99, \code{HypPlas}=1 (yes) inflates RR by 1.82; \cr
        \code{N_Biops}>0 and <99, \code{HypPlas}=99 (unk) inflates RR by 1.00. 

    For remaining relative risk covariates, \code{AgeMen}, \code{Age1st} and \code{N_Rels}:
    \tabular{ll}{
    AgeMen   \tab Age at menarchy must be postive integer less than or equal to initial age T1.\cr
             \tab NOTE: (1) For African-American women AgeMen<=11 are grouped with AgeMen=12 \cr 
             \tab or 13. (2) For US Born Hispanic women  AgeMen is not included in the RR model \cr
             \tab and all values for this variable are recoded to 0. \cr            
    Age1st   \tab Age at 1st live birth must be postive integer greater than equal to AgeMen \cr
             \tab and less than or equal to initial age T1. \cr
             \tab NOTE: (1) For African-American women, Age1st is not included in the RR model \cr
             \tab and all values for this variable are recoded to 0. (2) For US Born and Foreign \cr
             \tab Born Hispanic women, the recoding for this variable follows: \cr
    }
    \tabular{llr}{
    Age1st   \tab 19 and younger or 99 (unk) \tab 0 \cr
             \tab 20 - 29                    \tab 1 \cr
             \tab 30+ or 98 (nulliparous) and not 99       \tab 2 \cr
    }
    \tabular{ll}{
    N_Rels   \tab The number of 1st degree relatives with BrCa must be 0,1,2....  \cr
             \tab NOTE: For Asian-Americans Race=6-11 and Hispanic-Americans (US and foreign born), \cr 
             \tab the number of 1st degree relative coded value of 2 gets grouped with 1. \cr
    }

}
\value{
A data frame containing the error indictors, recoded covariates as well as other checking 
variables defined for checking the consistency of the input data. 
}
\seealso{
  \code{\link{error.table.all}}, \code{\link{error.table}}
}
\examples{
data(exampledata)
recode.check(exampledata)
}