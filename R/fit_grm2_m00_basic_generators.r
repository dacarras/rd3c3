#------------------------------------------------------------------------------
# basic generators
#------------------------------------------------------------------------------

#--------------------------------------------------------------------
# grouping
#--------------------------------------------------------------------

grouping_lines <- function(data, grp_var, grp_txt) {

grp_tab <- dplyr::count(data, 
           !!!rlang::syms(grp_var),
           !!!rlang::syms(grp_txt)
           ) %>%
           mutate(grp_term =
           paste0(!!!rlang::syms(grp_var), " = ", !!!rlang::syms(grp_txt), "")
           )

grp_var_txt <- names(grp_tab[1])

grp_term_txt <- grp_tab[4] %>%
                dplyr::pull()

grouping_block <- ''
grouping_block <- paste0(grouping_block, "GROUPING = ",grp_var_txt, " (\n")
grouping_block <- paste0(grouping_block, paste0(grp_term_txt, collapse = "\n"), "\n")
grouping_block <- paste0(grouping_block, ");\n")

return(grouping_block)

# Note: grp_var is the variable with the values, grp_txt is the variable with the labels.
#       both variables needs to have the same lenght of objects
#       both variables should be included in the data table at uses        

}

#--------------------------------------------------------------------
# lambda
#--------------------------------------------------------------------

generate_lambda <- function(var_names) {
  lambda_block <- ""  
  # lambda
  for (x in var_names) {
    pseudo_item  <- gsub('i', 'u', x)
    lambda_block <- paste0(lambda_block, pseudo_item, " by ",x,"@1;\n")
  }

  for (x in var_names) {
    lambda_term  <- gsub('i', 'l', x)
    pseudo_item  <- gsub('i', 'u', x)
    lambda_block <- paste0(lambda_block, "eta BY ", pseudo_item, "* (",lambda_term, ");\n")
  }

  return(lambda_block)

}

#--------------------------------------------------------------------
# lambda
#--------------------------------------------------------------------

generate_free_lambda <- function(var_names) {
  lambda_block <- ""  
  # lambda
  for (x in var_names) {
    pseudo_item  <- gsub('i', 'u', x)
    lambda_block <- paste0(lambda_block, pseudo_item, " by ",x,"@1;\n")
  }

  for (x in var_names) {
    lambda_term  <- gsub('i', 'l', x)
    pseudo_item  <- gsub('i', 'u', x)
    lambda_block <- paste0(lambda_block, "eta BY ", pseudo_item, "* ;\n")
  }

  return(lambda_block)

}

#--------------------------------------------------------------------
# phantom item residual
#--------------------------------------------------------------------

generate_y_res <- function(var_names) {
  phantom_block <- ""  
  # lambda
  for (x in var_names) {
    pseudo_item  <- gsub('i', 'u', x)
    phantom_block <- paste0(phantom_block, pseudo_item, "@0;\n")
  }

  return(phantom_block)

}

#--------------------------------------------------------------------
# phantom item intercept
#--------------------------------------------------------------------

generate_y_fix <- function(var_names) {
  phantom_block <- ""  
  # lambda
  for (x in var_names) {
    pseudo_item  <- gsub('i', 'u', x)
    phantom_block <- paste0(phantom_block, "[",pseudo_item, "@0];\n")
  }

  return(phantom_block)

}

#--------------------------------------------------------------------
# phantom item intercept
#--------------------------------------------------------------------

generate_y_free <- function(var_names) {
  phantom_block <- ""  
  # lambda
  for (x in var_names) {
    pseudo_item  <- gsub('i', 'u', x)
    phantom_block <- paste0(phantom_block, "[",pseudo_item, "*];\n")
  }

  return(phantom_block)

}

#--------------------------------------------------------------------
# tau
#--------------------------------------------------------------------

generate_tau <- function(var_names, thresholds) {
tau_block <- ""
 # thresholds
  t_counter <- 1
  for (i in 1:length(var_names)) {
    for (j in 1:thresholds[i]) {
      tau_block <- paste0(tau_block, "[ ", var_names[i], "$", j, "*]   (",sprintf("t%02d", t_counter), ");\n")
      t_counter <- t_counter + 1
    }
  }

  return(tau_block)
}

#--------------------------------------------------------------------
# tau
#--------------------------------------------------------------------

generate_free_tau <- function(var_names, thresholds) {
tau_block <- ""
 # thresholds
  t_counter <- 1
  for (i in 1:length(var_names)) {
    for (j in 1:thresholds[i]) {
      tau_block <- paste0(tau_block, "[ ", var_names[i], "$", j, "*];\n")
      t_counter <- t_counter + 1
    }
  }

  return(tau_block)
}

#--------------------------------------------------------------------
# scale
#--------------------------------------------------------------------

generate_scale <- function(var_names) {
  scale_block <- ""
  # scale factor
  scale_block <- paste0(scale_block, paste0("{ ", var_names, "@1 };", collapse = "\n"), "\n")
  return(scale_block)
}

#--------------------------------------------------------------------
# free scale
#--------------------------------------------------------------------

generate_free_scale <- function(var_names) {
  scale_block <- ""
  # scale factor
  scale_block <- paste0(scale_block, paste0("{ ", var_names, "* };", collapse = "\n"), "\n")
  return(scale_block)
}

#--------------------------------------------------------------------
# means and variances
#--------------------------------------------------------------------

generate_ref <- function(){
  ref_block <- ""
  ref_block <- paste0(ref_block, "[eta@0];\neta@1;\n")
  return(ref_block)
}


generate_else <- function(){
  else_block <- ""
  else_block <- paste0(else_block, "[eta@0];\neta*;\n")
  return(else_block)
}


#--------------------------------------------------------------------
# strict model: threshold, lambda, and scale
#--------------------------------------------------------------------

gen_strict_model <- function(grp_lst, grp_ref, var_names, thresholds, file){
grp_lst     <- grp_lst
var_names   <- var_names
thresholds  <- thresholds
output_filename <- file

  model_block <- ''

  model_block <- paste0(model_block, "\n")
  model_block <- paste0(model_block, generate_lambda(var_names))
  model_block <- paste0(model_block, "\n")

  for (i in grp_lst) {
      if (i == grp_ref) {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_lambda(var_names))
      model_block <- paste0(model_block, generate_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_scale(var_names))
      model_block <- paste0(model_block, generate_y_fix(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))
      model_block <- paste0(model_block, generate_ref())
      } else {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_lambda(var_names))
      model_block <- paste0(model_block, generate_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_scale(var_names))
      model_block <- paste0(model_block, generate_y_free(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))
      model_block <- paste0(model_block, generate_else())
      }
  }

writeLines(model_block, con = output_filename)
return(model_block)

}

#--------------------------------------------------------------------
# scalar model: threshold and lambda
#--------------------------------------------------------------------

gen_scalar_model <- function(grp_lst, grp_ref, var_names, thresholds, file){
grp_lst     <- grp_lst
var_names   <- var_names
thresholds  <- thresholds
output_filename <- file

  model_block <- ''

  model_block <- paste0(model_block, "\n")
  model_block <- paste0(model_block, generate_lambda(var_names))
  model_block <- paste0(model_block, "\n")

  for (i in grp_lst) {
      if (i == grp_ref) {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_lambda(var_names))
      model_block <- paste0(model_block, generate_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_scale(var_names))
      model_block <- paste0(model_block, generate_scale(var_names))
      model_block <- paste0(model_block, generate_y_fix(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))      
      model_block <- paste0(model_block, generate_ref())
      } else {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_lambda(var_names))
      model_block <- paste0(model_block, generate_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_free_scale(var_names))
      model_block <- paste0(model_block, generate_y_free(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))
      model_block <- paste0(model_block, generate_else())
      }
  }

writeLines(model_block, con = output_filename)
return(model_block)

}

#--------------------------------------------------------------------
# configural: threshold invariance
#--------------------------------------------------------------------

gen_configural_model <- function(grp_lst, grp_ref, var_names, thresholds, file){
grp_lst     <- grp_lst
var_names   <- var_names
thresholds  <- thresholds
output_filename <- file

  model_block <- ''

  model_block <- paste0(model_block, "\n")
  model_block <- paste0(model_block, generate_lambda(var_names))
  model_block <- paste0(model_block, "\n")

  for (i in grp_lst) {
      if (i == grp_ref) {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_free_lambda(var_names))
      model_block <- paste0(model_block, generate_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_scale(var_names))
      model_block <- paste0(model_block, generate_y_fix(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))      
      model_block <- paste0(model_block, generate_ref())
      } else {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_free_lambda(var_names))
      model_block <- paste0(model_block, generate_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_free_scale(var_names))
      model_block <- paste0(model_block, generate_y_free(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))
      model_block <- paste0(model_block, generate_ref())
      }
  }

writeLines(model_block, con = output_filename)

return(model_block)

}

#--------------------------------------------------------------------
# baseline model
#--------------------------------------------------------------------

gen_baseline_model <- function(grp_lst, grp_ref, var_names, thresholds, file){
grp_lst     <- grp_lst
var_names   <- var_names
thresholds  <- thresholds
output_filename <- file

  model_block <- ''

  model_block <- paste0(model_block, "\n")
  model_block <- paste0(model_block, generate_lambda(var_names))
  model_block <- paste0(model_block, "\n")

  for (i in grp_lst) {
      if (i == grp_ref) {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_free_lambda(var_names))
      model_block <- paste0(model_block, generate_free_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_scale(var_names))
      model_block <- paste0(model_block, generate_y_fix(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))    
      model_block <- paste0(model_block, generate_ref())
      } else {
      model_block <- paste0(model_block, "MODEL ", i, ":\n")
      model_block <- paste0(model_block, generate_free_lambda(var_names))
      model_block <- paste0(model_block, generate_free_tau(var_names, thresholds))
      model_block <- paste0(model_block, generate_scale(var_names))
      model_block <- paste0(model_block, generate_y_fix(var_names))
      model_block <- paste0(model_block, generate_y_res(var_names))    
      model_block <- paste0(model_block, generate_ref())
      }
  }

writeLines(model_block, con = output_filename)

return(model_block)


}