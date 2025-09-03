#' WOE transformation for numeric variable
#'
#' `obtain_int_woe` tranform a given numeric variable in WOE form.
#'
#' This function takes an input dataframe of raw data, a second dataframe  for the lower and upper
#' bound indications.
#'
#' @importFrom magrittr "%>%"
#'
#' @param in_data The initial dataframe containing the raw data.
#' @param int_condition_table The dataframe with necessary lower and upper bound for WOE
#' transformation for numeric variables.
#' @param var_select The variable to transform in WOE forms.
#'
#' @return A dataframe contains the initial input dataframe plus one extract columns.
#'
#' @export

# Function to bin continuous variable for WOE

obtain_int_woe <- function(in_data, int_condition_table, var_select) {
  out_data <- in_data
  data_with_selected_var <- int_condition_table %>%
    filter(variable == var_select)
  
  data_no_missing <- data_with_selected_var %>%
    filter(!is.na(lower_bound) & !is.na(upper_bound))
  
  if (nrow(data_no_missing) == 0) stop("var_select must be a variable in the WOE condition dataframe")
  max_rank <- nrow(data_no_missing)
  woe_col <- paste("woe", var_select, sep = "_")
  
  vec_r <- head(data_no_missing$lower_bound)
  
  data_1 <- out_data |>
    dplyr::mutate(
      rank = findInterval(out_data[[var_select]], vec_r),
      rank = ifelse(is.na(rank), -99, rank)
    )
  
  
  data_var2 <- subset(data_with_selected_var, select = c(rank, WOE))
  colnames(data_var2)[2] <- paste("woe", var_select, sep = "_")
  
  out_data <- data_1 |>
    dplyr::left_join(data_var2, by = "rank") |>
    dplyr::select(-rank)
  
  return(out_data)
}

######################################################################################
###          Function to bin WOE for all numeric variables                 ###########
######################################################################################

#' WOE transformation for all numerics variables
#'
#' `var_list_int_woe` tranforms numerics variables in WOE form.
#'
#' This function takes an input dataframe of raw data, a second dataframe  for the lower and
#' upper bound indications.
#'
#' @importFrom magrittr "%>%"
#'
#' @param in_data The initial dataframe containing the raw data.
#' @param int_condition_table The dataframe with necessary lower and upper bound for WOE
#' transformation for numeric variables.
#'
#' @return A dataframe contains the initial input dataframe and WOE transformation for
#' numeric variables.
#'
#'
#' @export

var_list_int_woe <- function(in_data, int_condition_table) {
  out_data <- in_data
  variables_to_bin <- colnames(out_data)
  cond_var_0 <- unique(int_condition_table$variable)
  cond_var <- intersect(variables_to_bin, cond_var_0)
  
  for (i in c(1:length(cond_var))) {
    var_select <- cond_var[i]
    out_data <- obtain_int_woe(out_data, int_condition_table, var_select)
  }
  return(out_data)
}


###############################################################################
###             bin WOE  for a given nominal variable                  ########
###############################################################################

#' WOE transformation for character variable
#'
#' `obtain_nom_woe` transform a given character variable in WOE form.
#'
#' This function takes an input dataframe of raw data, a second dataframe  for the WOE indications.
#'
#' @importFrom magrittr "%>%"
#'
#' @param in_data A dataframe contains the initial input dataframe and WOE transformation for numeric variables.
#' @param nom_condition_table The dataframe with indications for WOE
#' transformation for character variables.
#' @param var_nom The variable to transform in WOE forms.
#'
#' @return A dataframe contains the initial input dataframe plus one extract columns.
#'
#' @export

obtain_nom_woe <- function(in_data, nom_condition_table, var_nom) {
  out_data <- in_data
  names(out_data) <- tolower(names(out_data))
  data_var <- subset(nom_condition_table, variable == var_nom & !is.na(nom_condition_table$cat))
  data_var$variable <- tolower(data_var$variable)
  
  woe_col <- paste("woe", var_nom, sep = "_")
  
  for (i in 1:nrow(data_var)) {
    cat_val <- data_var$cat[i]
    # Select rows with missing var_nom values
    col_nom <- out_data[, var_nom]
    row_nom <- which(col_nom == cat_val)
    # set WOE for the selected rows
    out_data[row_nom, woe_col] <- data_var$woe[i]
  }
  return(out_data)
}

######################################################################################
###          Function to bin WOE for all character variables                 ###########
######################################################################################

#' WOE transformation for all characters variables
#'
#' `var_list_nom_woe` tranforms all characters variables in WOE forms.
#'
#' This function takes an input dataframe of raw data, a second dataframe  for WOE indications.
#'
#' @importFrom magrittr "%>%"
#'
#' @param in_data The initial dataframe containing the raw data.
#' @param nom_condition_table The dataframe with indication for WOE
#' transformation for character variables.
#'
#' @return A dataframe contains the initial input dataframe and WOE transformation for
#' characters variables.
#'
#' @export
#'
var_list_nom_woe <- function(in_data, nom_condition_table) {
  out_data <- in_data
  names(out_data) <- tolower(names(out_data))
  variables_to_bin <- tolower(colnames(out_data))
  cond_var_0 <- tolower(unique(nom_condition_table$variable))
  cond_var <- intersect(variables_to_bin, cond_var_0)
  
  for (i in c(1:length(cond_var))) {
    var_nom <- cond_var[i]
    out_data <- obtain_nom_woe(out_data, nom_condition_table, var_nom)
  }
  return(out_data)
}

#############################################################################
###
##########################

#' Logistic prediction based on given parameters values
#'
#' `logistic_prediction` perform logistic prediction for a given dataset and variables prediction parameters.
#' This function takes an input dataframe, a second dataframe  of model parameters.
#'
#' @importFrom magrittr "%>%"
#'
#' @param in_data A dataframe contains the initial input dataframe and WOE transformation for characters variables.
#' @param log_reg_params The dataframe with Model parameters.
#' @param predict_var name for the predicted variable
#'
#' @return A dataframe contains the initial input dataframe and a columns for the predicted PD
#'
#'
#' @export

logistic_prediction <- function(log_reg_params, predict_var, in_data) {
  out_data <- in_data
  # exclude unnecessary columns
  mp_var <- log_reg_params %>% dplyr::select(!starts_with("x_"))
  mp_var <- mp_var %>% dplyr::select(!starts_with("_"))
  
  names(mp_var) <- tolower(names(mp_var))
  var_mp <- colnames(mp_var)
  
  out_data$tmp <- 0
  for (i in 1:length(var_mp)) {
    var_s <- var_mp[i]
    if (!startsWith(var_s, "woe")) {
      out_data$tmp <- out_data$tmp + mp_var[[var_s]]
    } else {
      out_data$tmp <- out_data$tmp + mp_var[[var_s]] * out_data[[var_s]]
    }
  }
  out_data$prediction_var <- 1 / (1 + exp(-out_data$tmp))
  out_data <- out_data %>% dplyr::select(!tmp)
  colnames(out_data)[ncol(out_data)] <- predict_var
  
  return(out_data)
}
