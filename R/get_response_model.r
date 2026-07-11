#' get_response_model() retrieves the mplus code to generate realizations
#'
#' @param out it uses as an input a mplus_object generated with MplusAutomation
#' @return a formula string with the mplus code to produce the scores
#' @export
#'
#' @examples
#'
#' get_response_model(grm_model)
#'
#'
get_response_model <- function(out){


score_output <- out

#----------------------------------------------------------
# lambda
#----------------------------------------------------------

lambda <- score_output$results$gh5$irt_data$loading %>% 
as.data.frame() %>%
tidyr::gather() %>%
mutate(index = seq(1:nrow(.))) %>%
mutate(mplus_line = as.character(NA_real_)) %>%
mutate(mplus_line = case_when(
nchar(as.character(index)) == 1 ~ paste0('eta by ','i0',as.character(index),'@',as.character(value),';\n'),
nchar(as.character(index)) == 2 ~ paste0('eta by ','i0',as.character(index),'@',as.character(value),';\n')
))

#----------------------------------------------------------
# tau
#----------------------------------------------------------

tau <- score_output$results$gh5$irt_data$tau %>%
as.data.frame() %>%
tidyr::gather() %>%
mutate(p = 1) %>%
group_by(key) %>%
mutate(j = seq(1:sum(p))) %>%
ungroup() %>%
mutate(index = as.numeric(stringr::str_replace_all(key, 'V',''))) %>%
mutate(mplus_line = as.character(NA_real_)) %>%
mutate(mplus_line = case_when(
nchar(as.character(index)) == 1 ~ paste0('[ ','i0',as.character(index),'$',j,'@',as.character(value),' ];\n'),
nchar(as.character(index)) == 2 ~ paste0('[ ','i0',as.character(index),'$',j,'@',as.character(value),' ];\n')
))

#----------------------------------------------------------
# mplus lines
#----------------------------------------------------------

model_lines <- dplyr::bind_rows(
               data.frame(
               mplus_line = c('\n')
               ),
               dplyr::select(lambda, mplus_line),
               data.frame(
               mplus_line = c('\n')
               ),
               dplyr::select(tau, mplus_line),
               data.frame(
               mplus_line = c('\n','[eta@0];\n','eta@1;\n','\n')
              )
               )


mplus_response_model <- model_lines %>%
                        unlist() %>%
                        stringr::str_c(., collapse = '')

model_statement <- formula(
                   bquote(~.(
                   noquote(
                   mplus_response_model
                   ))))

return(model_statement)

}