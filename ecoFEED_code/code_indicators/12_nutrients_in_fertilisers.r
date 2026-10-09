rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# amount of nutrients applied on the fields as fertilisers
N_P_inputs <- readRDS(file = file.path(filePath2, "N_P_inputs.rds"))

# type (2) land-use (cultivated, on-farm, excl. fallows)
cult_LU_by_sect <- readRDS(file = file.path(filePath2, "cult_LU_by_sect.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nutrients_in_ferti <- cult_LU_by_sect %>%
	filter(sector != "plant_production") %>%
	left_join(N_P_inputs, by = c("region", "sector", c("yield_crop" = "feed"))) %>%
	mutate(
		N_input_in_fert = N_kg_per_ha * area,
		P_input_in_fert = P_kg_per_ha * area
	) %>%
	group_by(sector, region) %>%
  	summarise(
  		N_input_in_fert_region = sum(N_input_in_fert, na.rm = TRUE),
  		P_input_in_fert_region = sum(P_input_in_fert, na.rm = TRUE),
  		cult_area_ha_sum = sum(area, na.rm = TRUE),
  		) %>%
	mutate(
		N_input_in_fert_region_per_ha = N_input_in_fert_region /cult_area_ha_sum,
		P_input_in_fert_region_per_ha = P_input_in_fert_region /cult_area_ha_sum
	)	

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveRDS(nutrients_in_ferti, file = file.path(filePath2, "LU_ferti_all_sectors.rds"))
write.csv(nutrients_in_ferti, file = file.path(filePath2, "LU_ferti_all_sectors.csv"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
