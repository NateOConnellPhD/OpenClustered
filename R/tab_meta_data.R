#' Provides a table of descriptive statistics for the meta data of the datasets provided
#'
#' A wrapper function using the 'table1' package that provides summary data characteristics of a given list of data sets
#' @param Task An entry that will specify which task to summarize. Default is both.
#' @param formula A formula specifying which meta data characteristics to summarize. In the form of "~ x + y + z +..."
#' @param df List of data frames or vector of data frame names to summarize
#' @return An HTML table from the 'table1' package summarizing the meta data of the supplied data list
#' @examples 
#' plot_meta_data(allplots=T)
#' @export
#' 
#' 

tab_meta_data <- function(Task = "both", formula, df = data_list) {
  #get names of the data_list to be summarize 

  df_names <- names(df)
  
  # Subset data to summarize to those included
  temp_data <- OpenClustered::meta_data
  temp_data <- temp_data[temp_data$dataset %in% df_names, ]
  
  
  temp_data$imbalance = as.numeric(temp_data$imbalance)

  
  #Label meta data
  Hmisc::label(temp_data$n_obs) = "Number of Observations"
  Hmisc::label(temp_data$n_features) = "Number of Features"
  Hmisc::label(temp_data$missing_percent) = "Rows with Missing Data"
  Hmisc::label(temp_data$n_clusters) = "Number of Clusters"
  Hmisc::label(temp_data$imbalance) = "Imbalance"
  Hmisc::label(temp_data$missing_obs) = "Number of Missing Observations"
  Hmisc::label(temp_data$target_mean) = "Continuous Outcome Means"
  Hmisc::label(temp_data$target_sd) = "Continuous Outcome SD"
  Hmisc::label(temp_data$coeff_var) = "Continuous Outcome Coefficient of Variation"
  Hmisc::label(temp_data$cluster_type) = "Cluster Type"
  Hmisc::label(temp_data$task) = "Task"
  
  #Generate the table using the provided task and formula
  if(Task == "Regression"){
    reg_data <- subset(temp_data, task == "Regression")
    table1::table1(data = reg_data, formula)
  } else if(Task == "Classification"){
    class_data <- subset(temp_data, task == "Classification")
    table1::table1(data = class_data, formula)
  } else{
    table1::table1(data = temp_data, formula)
  }

 
}


