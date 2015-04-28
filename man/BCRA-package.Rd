\name{BCRA-package}
\alias{BCRA-package}
\alias{BCRA}
\docType{package}
\title{
A Package for Breast Cancer Risk Assessment
}
\description{
This package is to project absolute risk of invasive breast cancer according to 
NCI's Breast Cancer Risk Assessment Tool (BCRAT) algorithm 
for specified race/ethnic groups and age intervals. 
}
\details{
This package can be used to estimate the risk of developing breast cancer over a 
predetermined time interval with risk factors. As the same as Breast Cancer Risk Assessment SAS Macro, 
the users can specify the time interval as appropriate, not only limited to the 5 years risk prediction available with BCRAT. 

The main function in this package is \code{absolute.risk}, which is defined based on 
a statistical model known as the "Gail model". Parameters and constants needed in this 
function include initial and projection age, recoded covariates using function
 \code{recode.check}, relative risks of BrCa at age "<50" and ">=50" obtained from 
 function \code{relative.risk} as well as other known constants listed from 
 function \code{list.constants} like BrCa composite incidences, competing hazards, 
 1-attributable risk using in NCI BrCa Risk Assessment Tool (NCI BCRAT). With risk factors
 and projection interval ages for a group of women, the function \code{absolute.risk} will
 return the corresponding absolute risk projections. 
 If the function returns any missing values, the function \code{error.table}
 or \code{error.table.all} is used to find where the errors occured. 
 The function \code{check.summary} can give a quick check
 for errors of input file and missing values of risks. 

For further analysis, a data frame is created from the function \code{risk.summary}, which includes age, duration of the 
projection time interval, covariates and the projected risk. This function can also export a CSV file "risk_summary.csv" to save 
the data frame under user's working directory for reading convenience.

Any changes/modifications to the package would be at the user's own discretion and risk.
}
\author{
Fanni Zhang <zhangf@imsweb.com>
}
\references{
Gail MH, Brinton LA, Byar DP, Corle DK, Green SB, Shairer C, Mulvihill JJ. 
Projecting individualized probabilities of 
developing breast cancer for white females who are being examined annually. 
JNCI 81(24): 1879-86, 1989.

Costantino J, Gail MH, Pee D, Anderson S, Redmond CK, Benichou J, Wieand HS. 
Validation studies for models to project the risk of invasive and total breast
cancer. JNCI 91(18):1541-48, 1999.

Gail MH, Costantino JP, Pee D, Bondy M, Newman L, Selvan M, Anderson GL, Malone KE, 
Marchbanks PA, McCaskill-Stevens W, Norman SA, Simon MS, Spirtas R, Ursin G, Berstein L. 
Projecting individualized absolute invasive breast cancer risk in African American women. 
JNCI 99(23):1782-92, 2007.

Matsuno RK, Costantino JP, Ziegler RG, Anderson GL, Li H, Pee D, Gail MH. 
Projecting individualized absolute invasive breast cancer risk in asian and 
pacific islander american women. JNCI 103(12):951-61, 2011. 
}
