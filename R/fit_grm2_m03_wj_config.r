#' fit_grm2_m03_wj_config() it fits a graded response model (GRM) using MPLUS and MplusAutomation
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @param grp_var group variable (as numeric)
#' @param grp_txt group variable (as text)
#' @param grp_ref reference group
#' @return mplus_object generated with MplusAutomation
#' @export
#'
#' @examples
#'
#' inv_3 <- fit_grm2_m03_wj_config(
#'          data = data_model, 
#'          scale_num = scale_id, 
#'          scale_info = scales_data,
#'          grp_var = 'id_k',
#'          grp_txt = 'CNTRY',
#'          grp_ref = 'CHL'
#'          )
#'
#'
fit_grm2_m03_wj_config <- function(data, scale_num, scale_info, grp_var, grp_txt, grp_ref) {

# -----------------------------------------------
# main objects
# -----------------------------------------------

selected_scale <- scale_num
responses      <- data
item_table     <- scale_info
scales_data    <- scale_info
mplus_file     <- scale_info %>%
                  dplyr::filter(scale_num == selected_scale) %>%
                  dplyr::select(mplus_file) %>%
                  unique() %>%
                  .$mplus_file

# selected_scale <- 3
# responses      <- response_data
# item_table     <- scales_data
# mplus_file     <- 'scale_003'

grp_var <- grp_var
grp_txt <- grp_txt
grp_ref <- grp_ref

# -----------------------------------------------
# source functions
# -----------------------------------------------

# source('fit_grm2_m00_basic_generators.r')

# -----------------------------------------------
# grouping
# -----------------------------------------------

group_lines <- grouping_lines(
               data    = responses, 
               grp_var = grp_var,
               grp_txt = grp_txt
               )

# -----------------------------------------------
# requires
# -----------------------------------------------

require(dplyr)
require(MplusAutomation)
require(stringr)

# -----------------------------------------------
# item list
# -----------------------------------------------

item_names <- item_table %>%
              dplyr::filter(scale_num == selected_scale) %>%
              dplyr::select(item) %>%
              .$item %>%
              as.character()

# -----------------------------------------------
# get data for modelling
# -----------------------------------------------

pre_names <- scales_data %>%
             dplyr::filter(scale_num == selected_scale) %>%
             dplyr::select(variable) %>%
             .$variable

new_names <- scales_data %>%
             dplyr::filter(scale_num == selected_scale) %>%
             dplyr::select(item) %>%
             .$item

reverse_items <- scales_data %>%
                 dplyr::filter(scale_num == selected_scale) %>%
                 dplyr::filter(recoding == 'reverse') %>%
                 dplyr::select(item) %>%
                 .$item

design_data <- responses %>%
               dplyr::select(id_i, id_j, ws, all_of(grp_var))

items_data <- responses %>%
              rename_at(vars(all_of(pre_names)), ~paste0(new_names)) %>%
              mutate_at(
              	.vars = reverse_items,
              	.funs = ~r4sda::reverse(.)) %>%
              dplyr::select(one_of(item_names))
data_model <- dplyr::bind_cols(
              design_data, items_data)

# -----------------------------------------------
# mplus variable statement
# -----------------------------------------------

categorical_lines <- item_table %>%
                     dplyr::filter(
                     scale_num == selected_scale) %>%
                     mutate(variable_lines = paste0(item,'\n')) %>%
                     dplyr::select(variable_lines)

variable_lines <- item_table %>%
                     dplyr::filter(
                     scale_num == selected_scale) %>%
                     mutate(variable_lines = paste0(item,'\n')) %>%
                     dplyr::select(variable_lines)


categorical_equal_lines <- read.table(
text="
variable_lines
'\n'
'CATEGORICAL =         \n'
",
header=TRUE, stringsAsFactors = FALSE)


usevariable_equal_lines <- read.table(
text="
variable_lines
'\n'
'USEVARIABLES =         \n'
",
header=TRUE, stringsAsFactors = FALSE)


design_lines <- read.table(
text="
variable_lines
'\n'
'!STRATIFICATION = id_s;\n'
'CLUSTER        = id_j;\n'
'WEIGHT         = ws;  \n'
'IDVARIABLE     = id_i;\n'
'                      \n'
",
header=TRUE, stringsAsFactors = FALSE)


closing_lines <- read.table(
text="
variable_lines
';\n'
",
header=TRUE, stringsAsFactors = FALSE)

group_lines <- data.frame(
variable_lines = group_lines
)


variable_lines <- dplyr::bind_rows(
                  design_lines,
                  usevariable_equal_lines,
                  variable_lines,
                  closing_lines,
                  categorical_equal_lines,
                  categorical_lines,
                  closing_lines,
                  group_lines,
                  )

variable_structure <- variable_lines %>%
                      unlist() %>%
                      stringr::str_c(., collapse = '')


variable_statement <- formula(
                   bquote(~.(
                   noquote(
                   variable_structure
                   ))))

# -----------------------------------------------
# mplus savedata statement
# -----------------------------------------------

first_line <- '\n'
file_line <- paste0('FILE = ',mplus_file,'_inv_03_scores.dat;\n')
save_line <- 'SAVE = FSCORES;\n'

save_table <- data.frame(
              save_lines = c(
              first_line,
              file_line,
              save_line
              ))

save_structure <- save_table %>%
                  unlist() %>%
                  stringr::str_c(., collapse = '')

save_statement <- formula(
                  bquote(~.(
                  noquote(
                  save_structure
                  ))))

# -----------------------------------------------
# mplus model statement
# -----------------------------------------------

num_categories <- dplyr::select(items_data, 1) %>%
                  na.omit() %>%
                  unique() %>%
                  nrow()

num_thresholds <- num_categories - 1

var_names  <- new_names
thresholds <- data.frame(items = var_names) %>%
              mutate(thresholds = num_thresholds) %>%
              dplyr::select(thresholds) %>%
              dplyr::pull()

grp_tab <- dplyr::count(data, 
           !!!rlang::syms(grp_var),
           !!!rlang::syms(grp_txt)
           )

grp_lst <- grp_tab[2] %>%
           dplyr::pull()

model_structure <- gen_configural_model(grp_lst, grp_ref, var_names, thresholds, file = paste0(mplus_file,'_inv_03_mod.txt'))

model_statement <- formula(
                   bquote(~.(
                   noquote(
                   model_structure
                   ))))

# -----------------------------------------------
# mplus generic model
# -----------------------------------------------

library(MplusAutomation)
grm_model <- mplusObject(
MODEL = '
eta by i01*;
eta by i02*;
eta by i03*;
eta by i04*;
[eta@0];
eta@1;
', # this is the model statement
ANALYSIS = '
TYPE = COMPLEX;
ESTIMATOR = WLSMV;
',
VARIABLE ='
IDVARIABLE = id_i;

CATEGORICAL =
i01
i02
i03
i04
',
OUTPUT ='
STANDARDIZED
SAMPSTAT
PATTERNS
CINTERVAL
RESIDUAL
SVALUES
;
',
PLOT = '
TYPE = PLOT2;
', # add item characteristic curves
SAVEDATA ='
FILE = eta_scores.dat;
SAVE = FSCORES;
',
usevariables = names(data_model),
rdata = data_model)

# -----------------------------------------------
# mplus fit model
# -----------------------------------------------

mplus_object <- grm_model %>%
                update(.,
                MODEL = model_statement,
                VARIABLE = variable_statement,
                SAVEDATA = save_statement
                ) %>%
                mplusModeler(.,
                modelout = paste0(mplus_file,'_inv_03.inp'),
                run = 1L,
                hashfilename = FALSE,
                writeData = 'always')


# -----------------------------------------------
# fit model
# -----------------------------------------------

return(mplus_object)

}
