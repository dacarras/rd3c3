% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_grm2_m01_w_strict.r
\name{fit_grm2_m01_w_strict}
\alias{fit_grm2_m01_w_strict}
\title{fit_grm2_m01_w_strict() it fits a graded response model (GRM) using MPLUS and MplusAutomation}
\usage{
fit_grm2_m01_w_strict(data, scale_num, scale_info, grp_var, grp_txt, grp_ref)
}
\arguments{
\item{data}{a data frame, where rows = observations, and columns = variables}

\item{scale_num}{a number, that identifies a unique set of items within the scale_info table}

\item{scale_info}{a data frame where items memmbership to scale is uniquely identify}

\item{grp_var}{group variable (as numeric)}

\item{grp_txt}{group variable (as text)}

\item{grp_ref}{reference group}
}
\value{
mplus_object generated with MplusAutomation
}
\description{
fit_grm2_m01_w_strict() it fits a graded response model (GRM) using MPLUS and MplusAutomation
}
\examples{

inv_1 <- fit_grm2_m01_wj_strict(
         data = data_model, 
         scale_num = scale_id, 
         scale_info = scales_data,
         grp_var = 'id_k',
         grp_txt = 'CNTRY',
         grp_ref = 'CHL'
         )


}
