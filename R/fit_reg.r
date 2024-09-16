#' fit_reg() fits a linear regression model with pseudo maximum likelihood, using MPLUS and MplusAutomation, with and without controling by a covariate.
#'           in particular, it fits a population average model.
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param pv1 vector within the data frame with plausible value 1
#' @param pv2 vector within the data frame with plausible value 2
#' @param pv3 vector within the data frame with plausible value 3
#' @param pv4 vector within the data frame with plausible value 4
#' @param pv5 vector within the data frame with plausible value 5
#' @param ind vector name of the target variable of interest
#' @return table with estimates
#' @export
#'
#' @examples
#'
#' est_table <- fit_mlm(
#'              data = data_response,
#'              pv1  = PV1CIV,
#'              pv2  = PV2CIV,
#'              pv3  = PV3CIV,
#'              pv4  = PV4CIV,
#'              pv5  = PV5CIV,
#'              ind  = opd
#'              )
#'
fit_reg <- function(
    data,
    pv1,pv2,pv3,pv4,pv5,
    ind){

#----------------------------------------------------------
# libraries
#----------------------------------------------------------


require(rlang)
require(dplyr)
require(purrr)
require(MplusAutomation)


#----------------------------------------------------------
# get minimal objects
#----------------------------------------------------------

data_model    <- data
pv1_value     <- rlang::enquo(pv1)
pv2_value     <- rlang::enquo(pv2)
pv3_value     <- rlang::enquo(pv3)
pv4_value     <- rlang::enquo(pv4)
pv5_value     <- rlang::enquo(pv5)

ind_var       <- rlang::enquo(ind)

#----------------------------------------------------------
# center ind scores
#----------------------------------------------------------

data_model <- data_model %>%
## student responses
mutate(w = !!ind_var)  %>%                  # mean score
mutate(w_c = r4sda::c_mean(w, id_j)) %>%    # means by group
mutate(w_g = r4sda::c_mean(w, id_k)) %>%    # grand mean    
mutate(w_w = w - w_c   )    %>%  # centering within cluster
mutate(w_m = w - w_g   )    %>%  # centering to the grand mean
mutate(w_b = w_c - w_g )         # centered cluster means

#----------------------------------------------------------
# create imputed data list
#----------------------------------------------------------

data_1 <- data_model %>% mutate(pv = !!pv1_value)
data_2 <- data_model %>% mutate(pv = !!pv2_value)
data_3 <- data_model %>% mutate(pv = !!pv3_value)
data_4 <- data_model %>% mutate(pv = !!pv4_value)
data_5 <- data_model %>% mutate(pv = !!pv5_value)

data_list <- list(
data_1,
data_2,
data_3,
data_4,
data_5
)

#----------------------------------------------------------
# list of variables
#----------------------------------------------------------

design_variables  <- c('id_j','id_s','wi','wj')
pv_terms          <- c('pv')
independent_terms <- c('w')

list_variables <- c(
                  design_variables,
                  pv_terms,
                  independent_terms
                  )

data_mlm <- data_model %>%
            dplyr::select(one_of(list_variables))

#----------------------------------------------------------
# quiet function
#----------------------------------------------------------

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

#----------------------------------------------------------
# model: y on z;
#----------------------------------------------------------

reg_01 <- quiet(MplusAutomation::mplusObject(
MODEL = '

pv on w;

',
ANALYSIS = '
TYPE = COMPLEX;
ESTIMATOR = MLR;
',
VARIABLE ='
STRATIFICATION = id_s;
CLUSTER  = id_j;
WEIGHT   = ws;

USEVARIABLES =
pv
w
;

',
OUTPUT ='
STANDARDIZED
CINTERVAL
RESIDUAL
;
',
rdata = data_list,
imputed = TRUE)
          )


#----------------------------------------------------------
# fit models
#----------------------------------------------------------

fit_01 <- quiet(
          mplusModeler(reg_01,
          modelout = 'reg_01.inp',
          run = 1L,
          hashfilename = FALSE,
          writeData = 'always')
          )

#----------------------------------------------------------
# get est
#----------------------------------------------------------

ci_01  <- fit_01 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          dplyr::filter(paramHeader == 'PV.ON') %>%
          mutate(model = 1) %>%
          mutate(term = param)  %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(model, '_', term)) %>%
          dplyr::select(index, ll, ul)

est_01 <- fit_01 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('unstandardized') %>%
          tibble::tibble() %>%
          dplyr::filter(paramHeader == 'PV.ON')  %>%
          mutate(model = 1) %>%
          mutate(term = param) %>%
          mutate(e  = est) %>%
          mutate(se = se) %>%
          mutate(p  = pval) %>%
          mutate(missing  = rate_missing) %>%
          mutate(index = paste0(model, '_', term)) %>%
          dplyr::left_join(., ci_01, by = 'index') %>%
          dplyr::select(model, term, e, se, p, missing, ll, ul)
          

est_table <- est_01 %>%
             mutate(term = tolower(term))

#----------------------------------------------------------
# return object
#----------------------------------------------------------

return(est_table)

}
