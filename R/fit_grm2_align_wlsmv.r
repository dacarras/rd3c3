#' fit_grm2_align() it fits a between country GRM with alignment method using MPLUS and MplusAutomation
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return mplus_object generated with MplusAutomation
#' @export
#'
#' @examples
#'
#' scale_001_lign <- fit_grm2_align(
#' scale_info = scales_data,
#' scale_num  = 1,
#' data = data_model)
#'
#'
fit_grm2_align_wlsmv <- function(data, scale_num, scale_info) {

# -----------------------------------------------
# main objects
# -----------------------------------------------


# -----------------------------------------------
# test objects
# -----------------------------------------------

# scale_info <- readxl::read_xlsx(
#               'report_scales_example.xlsx', 
#               sheet = 'scales_data'
#               )
# scale_num <- 1
# 
# data      <- readRDS('report_dev_sample_k10_g4.rds')


# -----------------------------------------------
# requires
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
               dplyr::select(id_k, id_i, id_j, id_s, ws)

items_data <- responses %>%
              rename_at(vars(pre_names), ~paste0(new_names)) %>%
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
'STRATIFICATION = id_s;\n'
'CLUSTER        = id_j;\n'
'WEIGHT         = ws;  \n'
'IDVARIABLE     = id_i;\n'
'                      \n'
",
header=TRUE, stringsAsFactors = FALSE)

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
'STRATIFICATION = id_s;\n'
'CLUSTER        = id_j;\n'
'WEIGHT         = ws;  \n'
'IDVARIABLE     = id_i;\n'
'                      \n'
",
header=TRUE, stringsAsFactors = FALSE)


grouping_lines_1 <- read.table(
text="
variable_lines
'\n'
'GROUPING = id_k (              \n'
'\n'
",
header=TRUE, stringsAsFactors = FALSE)


responses <- responses %>%
             mutate(ctry = COUNTRY)

grouping_lines_2 <- dplyr::count(responses, id_k, ctry, ctry_name) %>%
dplyr::select(id_k, ctry, ctry_name) %>%
mutate(variable_lines = paste0(id_k, ' = ', ctry, ' !', ctry_name)) %>%
mutate(variable_lines = paste0(variable_lines,'\n')) %>%
dplyr::select(variable_lines)

grouping_lines_3 <- read.table(
text="
variable_lines
'\n'
');\n'
",
header=TRUE, stringsAsFactors = FALSE)

closing_lines <- read.table(
text="
variable_lines
';\n'
",
header=TRUE, stringsAsFactors = FALSE)

variable_lines <- dplyr::bind_rows(
                      design_lines,
                      usevariable_equal_lines,
                      variable_lines,
                      closing_lines,
                      categorical_equal_lines,
                      categorical_lines,
                      closing_lines,
                      grouping_lines_1,
                      grouping_lines_2,
                      grouping_lines_3
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
file_line <- paste0('RANKINGS = ',mplus_file,'_align_rangking.dat;\n')
save_h5_line <- paste0('H5RESULTS = ',mplus_file,'_align_results.H5;\n')

save_table <- data.frame(
              save_lines = c(
              first_line,
              file_line,
              save_h5_line
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


lambda_lines <- item_table %>%
                dplyr::filter(scale_num == selected_scale) %>%
                mutate(i = item) %>%
                mutate(lambda_lines =
                paste0('eta by ',i, ';\n')) %>%
                dplyr::select(lambda_lines)


lambda_table <- dplyr::bind_rows(
                lambda_lines
                )


model_structure <- lambda_table %>%
                   unlist() %>%
                   stringr::str_c(., collapse = '')

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
eta by i01;
eta by i02;
eta by i03;
eta by i04;
', # this is the model statement
ANALYSIS = '
TYPE = COMPLEX;
ESTIMATOR = WLSMV;
ALIGNMENT = FREE;
PROCESSORS = 4;
',
VARIABLE ='
STRATIFICATION = id_s;
CLUSTER        = id_j;
WEIGHT         = ws;

IDVARIABLE = id_i;

CATEGORICAL =
i01
i02
i03
i04
',
OUTPUT ='
STANDARDIZED
CINTERVAL
TECH8
ALIGN
SVALUES
;
',
SAVEDATA ='
RANKINGS = alignment_ranking.csv
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
                modelout = paste0(mplus_file,'_align.inp'),
                run = 1L,
                hashfilename = FALSE,
                writeData = 'always')


# -----------------------------------------------
# fit model
# -----------------------------------------------

return(mplus_object)

}
