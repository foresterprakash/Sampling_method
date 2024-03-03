# first option let's keep the diameter breaks as the same but different distance thresholds

cutoff_values <- c(4, 8, 15, 20)

aa <- rbind(c(3, 6, 10, 15),
           c(3, 5, 10, 15),
           c(4, 8, 12, 16),
           c(5, 10, 15, 20),
           c(3, 6, 12, 15),
           c(3, 6, 15, 20))

## Generate all combinations of distance cutoffs 
# Get the number of elements in each column
num_elements <- sapply(aa, length)

# Create an empty list to store all combinations
all_combos <- list()

# Function to generate combinations for a single column
generate_col_combos <- function(col_elements) {
  return(unique(col_elements))  # Ensure unique elements in each combination
}

# Loop through each column and generate combinations
for (i in 1:ncol(aa)) {
  current_elements <- aa[, i]
  col_combos <- generate_col_combos(current_elements)
  all_combos[[i]] <- col_combos
}

# Combine column combinations using Cartesian product
all_combos <- do.call(expand.grid, all_combos)


##### Now calculate plot-level forest AGB and compare each against total in the plot

agbCalc <- list()
for(i in 1:nrow(all_combos)){

  cutoffs <- all_combos[i,]
  
  ccsp <- dat_analysed %>%
    mutate(criteria = if_else(distance > cutoffs$Var4, "Out",
                              if_else(distance >= cutoffs$Var3,if_else(dbh >= 30,"in","out"),
                                      if_else(distance>=  cutoffs$Var2,if_else(dbh >=20,"in","out"),
                                              if_else(distance >=  cutoffs$Var1,if_else(dbh >= 10,"in","out"),"in"))))) %>%
    filter(criteria == "in") %>%
    select(-criteria,)
  
  # Now this data is ready for calculation
  dat_analysed <- dat_param %>%
    mutate(exp_fac = 10000 / (pi * cutoffs$Var4 * cutoffs$Var4)) %>%
    mutate(exp_fac_ccsp = ifelse(dbh >= 30, 10000 / (pi * cutoffs$Var4 *cutoffs$Var4),
                                 ifelse(dbh >= 20, 10000/(pi * cutoffs$Var3 *cutoffs$Var3),
                                        ifelse(dbh >= 10,10000/(pi * cutoffs$Var2 *cutoffs$Var2),
                                               10000/(pi * cutoffs$Var1 *cutoffs$Var1))))) %>%
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
  
   # Calculate pr hac information from ccsp expansion factor
   ccsp_analysis <- ccsp %>%
    select(-BA_sqm_ha,-stem_vol_ha,-stem_biomass_ton_ha,-total_biomass_ton_ha,-carbon_ton_ha) %>%
    mutate(BA_sqm_ha_ccsp = BA_sqm * exp_fac_ccsp) %>%
    mutate(stem_vol_ha_ccsp = stem_vol * exp_fac_ccsp) %>%
    mutate(total_biomass_ha_ccsp = total_biomass_ton * exp_fac_ccsp) %>%
    mutate(total_carbon_ha_ccsp = carbon_ton * exp_fac_ccsp)
  
  plot_sum_whole <- ccsp_analysis %>%
        group_by(plot_id) %>%
        summarise(Trees_enumerated = n(),
                  trees_ha = sum(exp_fac),
                  BA_ha = sum(BA_sqm_ha),
                  stem_volume_cum_ha = sum(stem_vol_ha),
                  total_biomass_ton_ha = sum(total_biomass_ton_ha),
                  total_carbon_ton_ha = sum(carbon_ton_ha)) %>%
        ungroup()
  agbCalc[[i]] <- plot_sum_whole$total_biomass_ton_ha

}


finaldat <- do.call(cbind, agbCalc)

