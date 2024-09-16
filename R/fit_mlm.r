#' fit_mlm() fits a mixed model with pseudo maximum likelihood, using MPLUS and MplusAutomation, with and without controling by a covariate.
#'           in particular, it fits a within-between model.
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param pv1 vector within the data frame with plausible value 1
#' @param pv2 vector within the data frame with plausible value 2
#' @param pv3 vector within the data frame with plausible value 3
#' @param pv4 vector within the data frame with plausible value 4
#' @param pv5 vector within the data frame with plausible value 5
#' @param cov vector name of the covariate variable. Is assumed to be a level 1 variable.
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
#'              cov  = ses,
#'              ind  = opd
#'              )
#'
fit_mlm <- function(
    data,
    pv1,pv2,pv3,pv4,pv5,
    cov,
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

cov_var       <- rlang::enquo(cov)
ind_var       <- rlang::enquo(ind)

#----------------------------------------------------------
# center ses scores
#----------------------------------------------------------

data_model <- data_model %>%
## socio economic status
mutate(z = !!cov_var)  %>%                  # mean score
mutate(z_c = r4sda::c_mean(z, id_j)) %>%    # means by group
mutate(z_g = r4sda::c_mean(z, id_k)) %>%    # grand mean    
mutate(z_w = z - z_c   )    %>%  # centering within cluster
mutate(z_m = z - z_g   )    %>%  # centering to the grand mean
mutate(z_b = z_c - z_g )         # centered cluster means

#----------------------------------------------------------
# center ses scores
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
control_terms     <- c('z_w','z_b')
independent_terms <- c('w_w','w_b')

list_variables <- c(
                  design_variables,
                  pv_terms,
                  control_terms,
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

mlm_01 <- quiet(MplusAutomation::mplusObject(
MODEL = '
%WITHIN%
pv on z_w@0;
pv on w_w (ww);

%BETWEEN%
pv on z_b@0;
pv on w_b (wb);
',
MODELCONSTRAINT = '
new(
w_ce
);
w_ce = wb - ww;
',
ANALYSIS = '
TYPE = COMPLEX TWOLEVEL;
ESTIMATOR = MLR;
',
VARIABLE ='
STRATIFICATION = id_s;
CLUSTER  = id_j;
WEIGHT   = wi;
BWEIGHT  = wj;
WTSCALE  = ECLUSTER;
BWTSCALE = SAMPLE;

USEVARIABLES =
pv
z_w
z_b
w_w
w_b;

WITHIN  = z_w w_w;
BETWEEN = z_b w_b;

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
# model: y on z w;
#----------------------------------------------------------

mlm_02 <- quiet(MplusAutomation::mplusObject(
MODEL = '
%WITHIN%
pv on z_w (zw);
pv on w_w (ww);

%BETWEEN%
pv on z_b (zb);
pv on w_b (wb);
',
MODELCONSTRAINT = '
new(
z_ce
w_ce
);
z_ce = zb - zw;
w_ce = wb - ww;
',
ANALYSIS = '
TYPE = COMPLEX TWOLEVEL;
ESTIMATOR = MLR;
',
VARIABLE ='
STRATIFICATION = id_s;
CLUSTER  = id_j;
WEIGHT   = wi;
BWEIGHT  = wj;
WTSCALE  = ECLUSTER;
BWTSCALE = SAMPLE;

USEVARIABLES =
pv
z_w
z_b
w_w
w_b;

WITHIN  = z_w w_w;
BETWEEN = z_b w_b;

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
          mplusModeler(mlm_01,
          modelout = 'mlm_01.inp',
          run = 1L,
          hashfilename = FALSE,
          writeData = 'always')
          )

fit_02 <- quiet(
          mplusModeler(mlm_02,
          modelout = 'mlm_02.inp',
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
          mutate(level  = BetweenWithin) %>%
          mutate(term = param)  %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(model, '_', level, '_', term)) %>%
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
          mutate(level  = BetweenWithin) %>%
          mutate(index = paste0(model, '_', level, '_', term)) %>%
          dplyr::left_join(., ci_01, by = 'index') %>%
          dplyr::select(model, level, term, e, se, p, missing, ll, ul)
          
ci_02  <- fit_02 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          dplyr::filter(paramHeader == 'PV.ON') %>%
          mutate(model = 2) %>%
          mutate(level  = BetweenWithin) %>%
          mutate(term = param)  %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(model, '_', level, '_', term)) %>%
          dplyr::select(index, ll, ul)
          
est_02 <- fit_02 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('unstandardized') %>%
          tibble::tibble() %>%
          dplyr::filter(paramHeader == 'PV.ON') %>%
          mutate(model = 2) %>%
          mutate(term = param) %>%
          mutate(e  = est) %>%
          mutate(se = se) %>%
          mutate(p  = pval) %>%
          mutate(missing  = rate_missing) %>%
          mutate(level  = BetweenWithin) %>%
          mutate(index = paste0(model, '_', level, '_', term)) %>%
          dplyr::left_join(., ci_02, by = 'index') %>%
          dplyr::select(model, level, term, e, se, p, missing, ll, ul)

est_table <- dplyr::bind_rows(est_01, est_02) %>%
             mutate(term = tolower(term)) %>%
             mutate(level = tolower(level))

#----------------------------------------------------------
# return object
#----------------------------------------------------------

return(est_table)

}
