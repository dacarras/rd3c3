#' get_realizations() produces realizations given a Mplus code model (i.e., fixed calibration)
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @param model_param Mplus code for the model retrieved by get_response_model()
#' @param file_name Mplus file names
#' @return mplus_object generated with MplusAutomation
#' @export
#'
#' @examples
#'
#' mplus_out_eta  <- rd3c3::silent(rd3c3::get_realizations(
#' data            = data_responses,
#' scale_num       = scale_id,
#' scale_info      = scales_data,
#' model_param     = get_response_model(grm_model),
#' file_name       = paste0(scale_file, '_realizations')
#' ))
#'
get_realizations <- function(data, scale_num, scale_info, model_param, file_name){

#----------------------------------------------------------
# define input object
#----------------------------------------------------------

selected_scale <- scale_num

responses      <- data

scales_data    <- scale_info

mplus_file     <- file_name

#----------------------------------------------------------
# generate data for modelling
#----------------------------------------------------------

item_names <- scales_data %>%
              dplyr::filter(scale_num == selected_scale) %>%
              dplyr::select(item) %>%
              .$item %>%
              as.character()

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
               dplyr::select(id_i)

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

categorical_lines <- scales_data %>%
                     dplyr::filter(
                     scale_num == selected_scale) %>%
                     mutate(variable_lines = paste0(item,'\n')) %>%
                     dplyr::select(variable_lines)

variable_lines <- scales_data %>%
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

variable_lines <- dplyr::bind_rows(
                      design_lines,
                      usevariable_equal_lines,
                      variable_lines,
                      closing_lines,
                      categorical_equal_lines,
                      categorical_lines,
                      closing_lines)

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
file_line <- paste0('FILE = ',mplus_file,'_scores.dat;\n')
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

#----------------------------------------------------------
# generic model
#----------------------------------------------------------

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
TYPE = GENERAL;
ESTIMATOR = WLSMV;
PROCESSORS = 4;
',
VARIABLE ='
USEVARIABLES =
i01-i04
;

CATEGORICAL =
i01-i04
;

IDVARIABLE   = id_i;

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
',
SAVEDATA ='
FILE = eta_scores.dat;
SAVE = FSCORES;
',
usevariables = names(data_model),
rdata = data_model)

#----------------------------------------------------------
# fit
#----------------------------------------------------------

mplus_object <- grm_model %>%
                update(.,
                MODEL = model_param,
                VARIABLE = variable_statement,
                SAVEDATA = save_statement
                ) %>%
                mplusModeler(.,
                modelout = paste0(mplus_file,'.inp'),
                run = 1L,
                hashfilename = FALSE,
                writeData = 'always')

#----------------------------------------------------------
# return object
#----------------------------------------------------------

return(mplus_object)

}