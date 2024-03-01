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


dat$distance <- as.numeric(dat$distance)
dat$height_crown <- as.numeric(dat$height_crown)
dat$height

dat <- dat %>%
  drop_na(dbh) %>%
  drop_na(height)

# Load parameters data for further analysis

param <- read.csv("Data/Equations.csv")

# Set a plot id as concatenates col,row and plot_number

dat_id <- dat %>%
  mutate(plot_id = paste(col,row,plot_number,sep = "-")) %>%
  select(ncol(dat)+1,1:ncol(dat))

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
  
# names(compare)[2] <- "plot_tree_total"
# names(compare)[3] <- "trees_ha_total"
# names(compare)[4] <- "BA_ha_total"
# names(compare)[5] <- "stem_vol_ha_total"
# names(compare)[6] <- "biomass_ha_total"
# names(compare)[7] <- "carbon_ton_total"
# names(compare)[8] <- "plot_tree_ccsp"
# names(compare)[9] <- "trees_ha_ccsp"
# names(compare)[10] <- "BA_ha_ccsp"
# names(compare)[11] <- "stem_vol_ha_ccsp"
# names(compare)[12] <- "biomass_ha_ccsp"
# names(compare)[13] <- "carbon_ton_ccsp"
# names(compare)

compare$diff_volume <- abs(compare$stem_vol_ha_total - compare$stem_vol_ha_ccsp)
compare$ratio_tree_out <- 
## 

library(ggplot2)
p <- dat_id %>%
  ggplot(mapping = aes(x = distance, y = dbh)) +
  geom_point()


with(compare, plot(carbon_ton_total, carbon_ton_ccsp))
abline(0, 1)

which.max(compare$carbon_ton_total - compare$carbon_ton_ccsp)
compare[15,]

problem <- dat_id[ dat_id$plot_id %in%  "135-23-4",]
with(problem, plot(distance, dbh, ylim=c(0, 50)))
abline(0, 1)

