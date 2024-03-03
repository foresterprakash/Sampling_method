

# We can use Gini coefficient to evaluate the size heterogeneity
# example papers: https://link.springer.com/article/10.1007/s11104-023-05874-2#Sec2
# https://iforest.sisef.org/contents/?id=ifor4096-015


# if dat is plot level inventory data

# Function to calculate Gini coefficient
gini_coefficient <- function(data) {
  # Sort data in ascending order
  data <- sort(data)
  
  # Calculate Lorenz curve
  n <- length(data)
  lorenz_y <- cumsum(data) / sum(data)
  lorenz_x <- seq(1, n) / n
  
  # Calculate Gini coefficient using trapezoidal rule
  A <- sum(lorenz_x * (lorenz_y - (1:n) / n))
  G <- 1 - 2 * A
  
  return(G)
}


## In my understanding #### (Prakash Lamichhane)

gini <- function(data) {
  data <- sort(data)
  
  # Calculate required variable
  n <- length(data)
  
}



# Calculate summary statistics by plot using group_by
plot_summary <- dat_analysed %>%
  group_by(plot_id) %>%
  summarise(
    n_trees = n(),
    mean_dbh = mean(dbh),
    total_agb = sum(total_biomass_ton_ha)/1000,
    gini_dbh = gini_coefficient(dbh)
  )

# Print the summary table
#with(plot_summary, plot(gini_dbh, total_agb))
#with(plot_summary, abline(0, 1))




library(ggplot2)
ggplot(plot_summary, aes(gini_dbh, total_agb)) +
  geom_point() +
  #geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.5) +
  geom_smooth() +
  xlab("Gini Index (dbh)") +
  ylab("Total Above-Ground Biomass")


##
