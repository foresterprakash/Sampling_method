# Load Libraries

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# # Load data
# 
# dat1 <- read_excel("Data/Compiled_1st_year.xlsx")
# dat2 <- read_excel("Data/compiled_2nd_year.xlsx")
# # 
# # # Write combined data
# 
# dat_combined <- rbind(dat1,dat2)
# write.csv(dat_combined, "Data/combined_LiDAR_data.csv")
# 
# # Combine both data 
# 
# dat <- rbind(dat1,dat2) %>%
#   filter(crown_class < 9)
# dat$height <- as.numeric(dat$height)

dat <- read_excel("Data/combined_data_updated.xlsx", sheet = 1)

# Columns to convert to numeric
# cols_to_convert <- c("distance", "dbh", "height", "height_crown")
# 
# dat[, cols_to_convert] <- lapply(data[,cols_to_convert], as.numeric)

dat$distance <- as.numeric(dat$distance)
dat <- dat %>%
  drop_na(dbh) %>%
  drop_na(height)

# Load parameters data for further analysis

param <- read.csv("Data/Equations.csv")

# Set a plot id as concatenates col,row and plot_number

# dat_id <- dat %>%
#   mutate(plot_id = paste(col,row,plot_number,sep = "-")) %>%
#   select(ncol(dat)+1,1:ncol(dat))

dat_id <- dat
# Rename the species code as it is in param

dat_id$species_code <- ifelse(dat_id$species_code %in% param$species_code, dat_id$species_code, "OTHERS-1")

# Join the data with respective parameters 

dat_param <- left_join(dat_id,param, by = c("species_code" = "species_code"))

# Now this data is ready for calculation
dat_analysed <- dat_param %>%
  mutate(exp_fac = 10000 / (pi * 20 * 20)) %>%
  mutate(exp_fac_ccsp = ifelse(dbh >= 30, 10000 / (pi * 20 *20),
                               ifelse(dbh >= 20, 10000/(pi * 15 *15),
                                      ifelse(dbh >= 10,10000/(pi * 8 *8),10000/(pi * 4 *4))))) %>%
  mutate(BA_sqm = pi * dbh ^ 2 / 40000) %>%
  mutate(BA_sqm_ha = BA_sqm * exp_fac) %>%
  mutate(stem_vol = (exp(Stem_a + stem_b * log(dbh) + Stem_c * log(height)))/1000) %>%
  mutate(stem_vol_ha = stem_vol * exp_fac) %>%
  mutate(stem_biomass_ton = stem_vol * density / 1000) %>%
  mutate(stem_biomass_ton_ha = stem_biomass_ton * exp_fac) %>%
  mutate(branch_ratio = ifelse(dbh > 70,branch_l,
                               ifelse(dbh > 40,((dbh-40)*branch_l + (70-dbh)*branch_m)/30,
                                      ifelse(dbh > 10,((dbh-10)*branch_m + (40-dbh)*branch_s)/30,branch_s)))) %>%
  mutate(final_branch_ratio = ifelse(crown_class == 8, 0 * branch_ratio, 
                                     ifelse(crown_class == 7, 0.75 *branch_ratio,branch_ratio))) %>%
  mutate(branch_biomass_ton = stem_biomass_ton * final_branch_ratio) %>%
  mutate(foliag_ratio = ifelse(dbh > 70,foliage_l,
                               ifelse(dbh > 40,((dbh-40) * foliage_l + (70-dbh)* foliage_m)/30,
                                      ifelse(dbh > 10,((dbh-10) * foliage_m + (40-dbh) * foliage_s)/30,foliage_s)))) %>%
  mutate(final_foliage_ratio = ifelse(crown_class == 7 | crown_class == 8, 0, foliag_ratio )) %>%
  mutate(foliage_biomass_ton = stem_biomass_ton * final_foliage_ratio) %>%
  mutate(total_biomass_ton = stem_biomass_ton + branch_biomass_ton + foliage_biomass_ton) %>%
  mutate(total_biomass_ton_ha = total_biomass_ton * exp_fac) %>%
  mutate(carbon_ton = total_biomass_ton * 0.47) %>%
  mutate(carbon_ton_ha = carbon_ton * exp_fac)

write.csv(dat_analysed,"Report/tree_data_analysis.csv")

## Plot_wise summary for whole plot data 

plot_sum_whole <- dat_analysed %>%
  group_by(plot_id) %>%
  summarise(Trees_enumerated = n(),
            trees_ha = sum(exp_fac),
            BA_ha = sum(BA_sqm_ha),
            stem_volume_cum_ha = sum(stem_vol_ha),
            total_biomass_ton_ha = sum(total_biomass_ton_ha),
            total_carbon_ton_ha = sum(carbon_ton_ha)) %>%
  ungroup()
write.csv(plot_sum_whole, "Report/plot_summary_total.csv")

# Now we have to break data into CCSP format 

# For CCSP data Please refer the CCSP rule from FRA, Manual 2022 (FRTC)

ccsp <- dat_analysed %>%
  mutate(criteria = if_else(distance > 20, "Out",
                            if_else(distance >= 15,if_else(dbh >= 30,"in","out"),
                                    if_else(distance>= 8,if_else(dbh >=20,"in","out"),
                                            if_else(distance >= 4,if_else(dbh >= 10,"in","out"),"in"))))) %>%
  filter(criteria == "in") %>%
  select(-criteria,)



# Calculate pr hac information from ccsp expansion factor
ccsp_analysis <- ccsp %>%
  select(-BA_sqm_ha,-stem_vol_ha,-stem_biomass_ton_ha,-total_biomass_ton_ha,-carbon_ton_ha) %>%
  mutate(BA_sqm_ha_ccsp = BA_sqm * exp_fac_ccsp) %>%
  mutate(stem_vol_ha_ccsp = stem_vol * exp_fac_ccsp) %>%
  mutate(total_biomass_ha_ccsp = total_biomass_ton * exp_fac_ccsp) %>%
  mutate(total_carbon_ha_ccsp = carbon_ton * exp_fac_ccsp)


## Plot-wise summary for ccsp data analysis

plot_sum_ccsp <- ccsp_analysis %>%
  group_by(plot_id) %>%
  summarise(Trees_enumerated = n(),
            tree_ha = sum(exp_fac_ccsp),
            BA_ha = sum(BA_sqm_ha_ccsp),
            Stem_vol_ha = sum(stem_vol_ha_ccsp),
            biomass_ha = sum(total_biomass_ha_ccsp),
            carbon_ton_ha = sum(total_carbon_ha_ccsp)) %>%
  ungroup()
write.csv(plot_sum_ccsp,"Report/ccsp_summary.csv")


## Lets make a data to compare the summary of plots from both methods

compare <- left_join(plot_sum_whole,plot_sum_ccsp, by = "plot_id")

headers <- c("plot_id", "plot_tree_total","trees_ha_total","BA_ha_total","stem_vol_ha_total","biomass_ha_total",
             "carbon_ton_total","plot_tree_ccsp","trees_ha_ccsp","BA_ha_ccsp","stem_vol_ha_ccsp",
             "biomass_ha_ccsp","carbon_ton_ccsp")

colnames(compare) <- headers
  
source("Code/rough_gini.R", local = TRUE)

compare$diff_volume <- abs(compare$stem_vol_ha_total-compare$stem_vol_ha_ccsp)
compare$ratio_tree_out <- compare$plot_tree_ccsp/compare$plot_tree_total

compare <- left_join(compare, calculate_gini(data = dat_analysed, x = "BA_sqm", group = "plot_id"), by = "plot_id")


## 

plot(compare$ratio_tree_out,compare$diff_volume)


library(ggplot2)
p <- dat_id %>%
  ggplot(mapping = aes(x = distance, y = dbh)) +
  geom_point()
p

with(compare, plot(carbon_ton_total, carbon_ton_ccsp))
abline(0, 1)

which.max(compare$carbon_ton_total - compare$carbon_ton_ccsp)
compare[15,]

problem <- dat_id[ dat_id$plot_id %in%  "135-23-4",]
with(problem, plot(distance, dbh, ylim=c(0, 50)))
abline(0,1)

## One more test with diameter distribution

# Analysis of First research question

# 1. 1. Does currently applied Concentric Circular Sample Plot (CCSP) estimates accurate per ha data?
# Check the distribution of these volumes :

par(mfrow = c(1,2))
hist(compare$stem_vol_ha_total, xlab = "Volume per ha (Total Count)")
hist(compare$stem_vol_ha_ccsp, xlab = "Volume per ha (CCSP)")
# Hypothesis tests for normality test


s1 <- shapiro.test(compare$stem_vol_ha_total)

ifelse(s1$p.value < 0.05, "Not-Normal", "Normal")

s2 <- shapiro.test(compare$stem_vol_ha_ccsp)

ifelse(s2$p.value < 0.05, "Not-Normal", "Normal")

# Both are normally distributed #

#Fit on parametric assumptions

# two groups so two sample test is applied

# Variance test 

data_var <- tidyr::pivot_longer(data = compare,cols = c("stem_vol_ha_total","stem_vol_ha_ccsp"),
                                names_to = "group",values_to = "value")
var.test1 <- with(data_var,car::leveneTest(value,group))

ifelse(var.test1$`Pr(>F)`[1] < 0.05, "Heterogenous", "Equal Variance")
rm(data_var)


# Now run the two sample t test with equal variance

result <- t.test(compare$stem_vol_ha_total, compare$stem_vol_ha_ccsp, var.equal = TRUE)


# Decision

ifelse(result$p.value < 0.05, "Significantly different", "Statistically not difference")


# Plot 

# Load required library
library(ggplot2)

# Assuming 'compare$stem_vol_ha_total' and 'compare$stem_vol_ha_ccsp' are your groups

# Create a box plot
library(ggplot2)
library(dplyr)

# Assuming 'compare$stem_vol_ha_total' and 'compare$stem_vol_ha_ccsp' are your groups

re_dat <- reshape2::melt(compare[,c("stem_vol_ha_total", "stem_vol_ha_ccsp")])

re_dat %>%
  ggplot(mapping = aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Category",
    y = "Volume cum per ha"
  ) +
  scale_x_discrete(labels = c("Variable 1" = "Label 1", "Variable 2" = "Label 2", "Variable 3" = "Label 3")) +
  scale_y_continuous(labels = function(x) paste0(x, " (custom label)"))

ggstatsplot::ggbetweenstats(
  data = re_dat,
  x = "variable",
  y = "value",
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "holm",
  xlab = NULL,
  ylab = NULL,
  k = 2L,
  var.equal = TRUE,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.2,
  centrality.plotting = TRUE,
  centrality.type = "parameteric")

# Calculate RMSE then

rmse <- caret::RMSE(compare$stem_vol_ha_ccsp,compare$stem_vol_ha_total)
rmse
# Calculate in Percentage by mean 
rmse_percent <- rmse /mean(compare$stem_vol_ha_total) *  100
rmse_percent

# The error is below 10 % 

# Can be acceptable in Many cases but needs to find
# get better CCSP for lesser error

Eval <- c(Metrics::mape(compare$stem_vol_ha_ccsp,compare$stem_vol_ha_total),
  Metrics::rmse(compare$stem_vol_ha_ccsp,compare$stem_vol_ha_total),
  Metrics::rae(compare$stem_vol_ha_total,compare$stem_vol_ha_ccsp))

Eval
