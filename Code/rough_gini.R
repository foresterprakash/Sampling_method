# Do Create a function called calculate_gini to calculate gini-coefficient 

calculate_gini <- function(x = NULL, data = NULL, group = NULL) {
  if (is.null(data) && is.null(group)) {
    # If only one argument is provided, interpret it as x
    if (!is.null(x)) {
      x <- as.data.frame(x)
      colnames(x) <- "value"
      x <- arrange(x,value)
      n <- length(x$value)
      x$j <- 1:length(x$value)
      x$numerator <- (2 * x$j - n - 1) * x$value
      x$denominator <- x$value * (n - 1)
      gini_coefficient <- sum(x$numerator) / sum(x$denominator)
      dat_gini <- gini_coefficient
    } else {
      stop("No arguments provided.")
    }
  } else if (is.null(group)) {
    # Calculate Gini coefficient for x vector
    x <- as.data.frame(data[[x]])
    colnames(x) <- "value"
    x <- arrange(x,x$value)
    n <- length(x$value)
    x$j <- 1:length(x$value)
    x$numerator <- (2 * x$j - n - 1) * x$value
    x$denominator <- x$value * (n - 1)
    gini_coefficient <- sum(x$numerator) / sum(x$denominator)
    dat_gini <- gini_coefficient
  } else {
    # Calculate Gini coefficient for each group
    dat_gini <- data %>%
      group_by(!!sym(group)) %>%
      arrange(!!sym(group), !!sym(x)) %>%
      mutate(n = n()) %>%
      mutate(j = 1:n()) %>% 
      mutate(numerator = (2 * j - n - 1) * !!sym(x)) %>%
      mutate(denominator = !!sym(x) * (n - 1)) %>%
      summarise(gini_coefficient = sum(numerator) / sum(denominator))
  }
  
  return(dat_gini)
}

# gini_value <- calculate_gini("BA_sqm",dat_analysed,"plot_id")
# 
# dat_gini_compare <- left_join(dat_gini_sum,compare[,c("plot_id","diff_volume","ratio_tree_out")], by = "plot_id")
# 
# 
# plot(dat_gini_compare$gini_coefficient,dat_gini_compare$ratio_tree_out)
# plot(dat_gini_compare$gini_coefficient,dat_gini_compare$diff_volume)
# plot(dat_gini_compare$median_BA,dat_gini_compare$gini_coefficient)
# plot(dat_gini_compare$mean_BA,dat_gini_compare$gini_coefficient)


