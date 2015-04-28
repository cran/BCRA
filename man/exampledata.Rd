\name{exampledata}
\alias{exampledata}
\docType{data}
\title{
   Example data set
}
\description{
   A data set containing all the required input data needed to perform risk projections, 
   such as initial age, projection age, BrCa relative risk covariates and race.
}
\usage{data("exampledata")}
\format{
  A data frame with 26 observations on the following 9 variables.
  \describe{
    \item{\code{ID}}{Woman's ID, positive integer 1, 2, 3,...}
    \item{\code{T1}}{Initial age, all real numbers T1 in [20, 90).}
    \item{\code{T2}}{BrCa projection age, all real numbers T2 in (20,90] such that T1<T2.}
    \item{\code{N_Biop}}{The number of biopsies, 0, 1, 2,..., 99=unk (99 recoded to 0).}
    \item{\code{HypPlas}}{Did biopsy display atypical hyperplasia? 0=no, 1=yes, 99=unk or not applicable.}
    \item{\code{AgeMen}}{Age at menarchy, less than or equal to initial age, 99=unk.}
    \item{\code{Age1st}}{Age at first live birth, greater or equal to age at menarchy and less than or 
                         equal to initial age, 98=nulliparous, 99=unk.}
    \item{\code{N_Rels}}{The number of 1st degree relatives with BrCa, 0, 1, 2,... 99=unk.}
    \item{\code{Race}}{Race, positive integer 1, 2, 3,...,11. See details.}
  }
}
\details{
   \tabular{ll}{
   1=Wh \tab White 1983-87 SEER rates (rates used in NCI BCRAT) \cr
   2=AA \tab African-American \cr
   3=Hi \tab Hispanic-American \cr
   4=NA \tab Other (Native American and unknown race) \cr
   5=Wo \tab White 1995-03 SEER rates (rates under further investigation) \cr
   6=Ch \tab Chinese-American \cr
   7=Ja \tab Japanese-American \cr
   8=Fi \tab Filipino-American \cr
   9=Hw \tab Hawaiian-American \cr
   10=oP \tab Other Pacific Islander \cr
   11=oA \tab Other Asian \cr
   }
   Note that Hispanic and other ethnic women risks are based on white women log relative
risks. Hispanic women risk are based on Hispanic seer rates while other women risk 
are based on white women seer rates. Avgerage other women rates are also based on
average white women seer rates as well.                   
}

