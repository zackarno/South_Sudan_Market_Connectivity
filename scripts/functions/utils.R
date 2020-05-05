
top_n_by_freq <- function(data, column, top_n_threshold){
  freq_col_name<- paste0("freq.", column)
  data %>%
    group_by(!!sym(column)) %>%
    summarise(!!freq_col_name:= n()) %>%
    top_n(wt= !!sym(freq_col_name),n =  top_n_threshold)

}
