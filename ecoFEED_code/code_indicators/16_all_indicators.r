rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# production and land-use data

cult_LU_by_sect <- readRDS(file = file.path(filePath2, "cult_LU_by_sect.rds"))

cult_LU_sums <- cult_LU_by_sect %>%
	group_by(sector, region) %>%
	summarise(
		cult_area = sum(area, na.rm = TRUE)
	)

food_production <- readRDS(file = file.path(filePath2, "food_production.rds"))

prod_by_sect <- cult_LU_sums %>% 
	left_join(food_production, by = c("region", "sector", "cult_area")) %>% 
	select(sector, region, cult_area, no_of_real_animals, food_prod_kg_dm_per_region, food_prod_kg_dm_per_ha)

prod_by_sect <- prod_by_sect %>% 
	mutate(animal_density = no_of_real_animals / cult_area)

# animal numbers
animal_nos_all <- readRDS(file = file.path(filePath2, "animal_nos_all.rds")) %>% 
  filter(!animal %in% c("dairy_cull_cow", 
  						"dairy_replacement_heifers",
						"beef_replacement_heifers",
  						"beef_cull_suckler_cows",
  						"sheep_lamb"))

animal_nos_all_by_sect <- animal_nos_all %>% 
	group_by(region, sector) %>%
	summarise(
		no_of_all_real_animals = sum(no_of_real_animals)
	)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Nutrient balance:
# N balance: N input in fertilizers + Biological nitrogen fixation + N input in manure - N output in harvest
# P balance: P input in fertilizers + P input in manure - P output in harvest

LU_ferti_all_sectors <- readRDS(file = file.path(filePath2, "LU_ferti_all_sectors.rds"))
BNF_sectors <- readRDS(file = file.path(filePath2, "BNF_sectors.rds"))
P_N_in_manure_per_ha <- readRDS(file = file.path(filePath2, "P_N_in_manure_per_ha.rds"))
N_P_in_harvest <- readRDS(file.path(filePath2, "N_P_in_harvest.rds"))

N_P_in_harvest_tot <- N_P_in_harvest %>%
	group_by(sector, region) %>%
	summarise(
		N_in_harvest_tot = sum(N_in_harvest, na.rm = TRUE),
		P_in_harvest_tot = sum(P_in_harvest, na.rm = TRUE)
	) %>%
	left_join(cult_LU_sums, by = c("region", "sector")) %>%
	mutate(
		N_in_harvest_per_ha = N_in_harvest_tot / cult_area,
		P_in_harvest_per_ha = P_in_harvest_tot / cult_area
	)

nutrient_flows <- LU_ferti_all_sectors %>%
	left_join(BNF_sectors, by = c("region", "sector"))	%>%
	left_join(P_N_in_manure_per_ha, by = c("region", "sector")) %>%
	left_join(N_P_in_harvest_tot[, c("region", "sector", 
									 "N_in_harvest_tot",
									 "P_in_harvest_tot",									   
									 "N_in_harvest_per_ha", 
									 "P_in_harvest_per_ha")], 
			   by = c("sector", "region"))

nutrient_balance <- nutrient_flows %>%
	mutate(
		N_balance_tot = N_input_in_fert_region + BNF_per_region + (N_input_manure_kg_per_ha * cult_area) - N_in_harvest_tot,
		P_balance_tot = P_input_in_fert_region + (P_input_manure_kg_per_ha * cult_area) - P_in_harvest_tot,
		N_balance_per_ha = N_input_in_fert_region_per_ha + BNF_per_ha + N_input_manure_kg_per_ha - N_in_harvest_per_ha,
		P_balance_per_ha = P_input_in_fert_region_per_ha + P_input_manure_kg_per_ha - P_in_harvest_per_ha
	) %>%
	select(region, sector, N_balance_tot, P_balance_tot, N_balance_per_ha, P_balance_per_ha)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# GHG emissions: CO2eqv kg
	# per ha on-farm area
	# per kg food produced
	# sector totals

# CH4 emissions from enteric fermentation: CO2eq per animal per year  

# dairy
EF_dairy <- readRDS(file = file.path(filePath2, "EF_dairy.rds"))
dairy_EF <- EF_dairy %>% 
	summarise(EF_CO2eqv_per_animal_per_year = sum(EF_CO2eqv_per_animal_per_year, na.rm = TRUE))

EF_dairy_CO2eqv <- food_production %>% 
	filter(sector == "dairy") %>% 
	mutate(
		EF_CO2eqv_per_year = no_of_real_animals * dairy_EF$EF_CO2eqv_per_animal_per_year
	) %>% 
	select(sector, region, EF_CO2eqv_per_year)

# beef
EF_beef <- readRDS(file = file.path(filePath2, "EF_beef.rds"))
beef_EF <- EF_beef %>% 
	group_by(beef_subsect) %>% 
	summarise(EF_CO2eqv_per_animal_per_year = sum(EF_CO2eqv_per_animal_per_year, na.rm = TRUE))

EF_beef_CO2eqv <- beef_EF %>% 
	left_join(animal_nos_all, by = c("beef_subsect" = "animal")) %>% 
	mutate(
		EF_CO2eqv_per_year = no_of_real_animals * EF_CO2eqv_per_animal_per_year
	) %>% 
	group_by(sector, region) %>% 
		summarise(EF_CO2eqv_per_year = sum(EF_CO2eqv_per_year, na.rm = TRUE)) %>% 
	select(sector, region, EF_CO2eqv_per_year)

# sheep
EF_sheep <- readRDS(file = file.path(filePath2, "EF_sheep.rds"))
sheep_EF <- EF_sheep %>% 
	summarise(EF_CO2eqv_per_animal_per_year = sum(EF_CO2eqv_per_animal_per_year, na.rm = TRUE))

EF_sheep_CO2eqv <- food_production %>% 
	filter(sector == "sheep") %>% 
	mutate(
		EF_CO2eqv_per_year = no_of_real_animals * sheep_EF$EF_CO2eqv_per_animal_per_year
	) %>% 
	select(sector, region, EF_CO2eqv_per_year)

EF_CO2eqv <- bind_rows(EF_dairy_CO2eqv, EF_beef_CO2eqv, EF_sheep_CO2eqv)

# CH4 emissions from manure management: CO2eqvs per animal per year
manure_manag_ghg <- readRDS(file.path(filePath4, "manure_manag_ghg.rds"))

CH4_manure_manage_CO2eqv <- animal_nos_all_by_sect %>% 
	left_join(manure_manag_ghg, by = c("sector" = "animal")) %>% 
	mutate(
		CH4_manure_manag_CO2eqv_per_year = no_of_all_real_animals * CO2eqv_per_ani_per_year	
	) %>% 
	select(sector, region, CH4_manure_manag_CO2eqv_per_year)

# Emissions from manure management: CO2eq per animal per year
CO2eqv_emissions_fromN2O <- readRDS(file.path(filePath2, "CO2eqv_emissions_fromN2O.rds"))

N2O_emissions_CO2eqv <- CO2eqv_emissions_fromN2O %>%
	left_join(food_production %>% select(sector, region, no_of_real_animals), by = c("region", "sector")) %>%
	mutate(
		N2O_emissions_CO2eqv_per_year = no_of_real_animals * total_emissions	
	) %>%
	select(sector, region, N2O_emissions_CO2eqv_per_year)

CO2eqv_emissions_fromN2O_beef <- readRDS(file.path(filePath2, "CO2eqv_emissions_fromN2O_beef.rds"))

N2O_emissions_CO2eqv_beef <- CO2eqv_emissions_fromN2O_beef %>%
	left_join(animal_nos_all, by = c("region", "sector", "production_per_animal_group" = "animal")) %>%
	mutate(
		N2O_emissions_CO2eqv_per_year = no_of_real_animals * total_emissions	
	) %>%
	group_by(sector, region)  %>%
	summarise(
		N2O_emissions_CO2eqv_per_year = sum(N2O_emissions_CO2eqv_per_year)
	) %>%
	select(sector, region, N2O_emissions_CO2eqv_per_year)

N2O_emissions_CO2eqv <- rbind(N2O_emissions_CO2eqv, N2O_emissions_CO2eqv_beef)

# N2O emissions from nitrogen application: per region (all animals of that sector)
CO2eqv_ferti_N_emissions <- readRDS(file = "ecoFEED_mod_data/data_current/CO2eqv_ferti_N_emissions.rds")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ALL emissions together

all_emissions <- prod_by_sect %>%
	left_join(CH4_manure_manage_CO2eqv, by = c("sector", "region")) %>%
	left_join(N2O_emissions_CO2eqv, by = c("sector", "region")) %>%
	left_join(CO2eqv_ferti_N_emissions, by = c("sector", "region")) %>%
	left_join(EF_CO2eqv, c("region", "sector")) %>%
	mutate(
	tot_emissions_CO2eqv = 
			rowSums(across(c(EF_CO2eqv_per_year, # rumination
							 CH4_manure_manag_CO2eqv_per_year, # manure manag
							 N2O_emissions_CO2eqv_per_year, # manure manag
							 CO2eqv_emissions_from_ferti)), # fertilising
				na.rm = TRUE),
		tot_emissions_CO2eqv_per_ha = tot_emissions_CO2eqv / cult_area,
		tot_CO2eqv_per_kg_dm_produced = tot_emissions_CO2eqv / food_prod_kg_dm_per_region
	)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# calculate for wet matter produced

dairy_sector_coef_dm_milk_meat <- readRDS(file = file.path(filePath2, "dairy_sector_coef_dm_milk_meat.rds"))
dairy_sector_coef_dm_milk_meat$sector <- "dairy"
nutritional_values <- readRDS(file = file.path(filePath2, "nutritional_values.rds"))

# lookup file for the sector products
sector_to_product <- c(
	beef = "beef_meat",
	dairy = "milk",
	dairy = "beef_meat",
	eggs = "eggs",
	pork = "pork_meat",
	sheep = "sheep_meat",
	poultry = "poultry_meat"
)
sector_lookup <- tibble(
	sector = names(sector_to_product),
	product = unname(sector_to_product)
)

# combine production and sector products
all_emissions_wm <- all_emissions %>% 
	select(
		sector,
		region,
		tot_emissions_CO2eqv,
		tot_emissions_CO2eqv_per_ha,
		tot_CO2eqv_per_kg_dm_produced
	) %>%
	left_join(sector_lookup, by = "sector")

# join dm factors based on the products and the coefficients for proportions of meat/milk (dairy)
all_emissions_wm <- all_emissions_wm %>%
  left_join(nutritional_values %>% 
      		select(product, dm_factor), 
      		by = "product") %>%
	left_join(dairy_sector_coef_dm_milk_meat, by = c("sector", "product"))

# wet matter for dairy calculated 
all_emissions_wm <- all_emissions_wm %>%
	mutate(
		wm_per_kg_total_dm = coef_milk_meat / dm_factor
	)

# sum the wet matter for dairy
all_emissions_wm <- all_emissions_wm %>%
	group_by(sector, region) %>%
	mutate(
		total_wm_per_kg_dm = 
			if_else(
				sector == "dairy",
				sum(wm_per_kg_total_dm),
				1 / dm_factor
		)) %>% ungroup()

all_emissions_wm <- all_emissions_wm %>%
	mutate(
		tot_CO2eqv_per_kg_wm_produced =
			tot_CO2eqv_per_kg_dm_produced / total_wm_per_kg_dm
	)

all_emissions_wm <- all_emissions_wm %>%
	group_by(sector, region) %>%
	summarise(
		tot_emissions_CO2eqv = first(tot_emissions_CO2eqv),
		tot_emissions_CO2eqv_per_ha = first(tot_emissions_CO2eqv_per_ha),
		tot_CO2eqv_per_kg_wm_produced = first(tot_CO2eqv_per_kg_wm_produced),
			.groups = "drop"
	)

# Food production: protein produced / land-area on farm; total per sector

food_prod_current <- readRDS(file = file.path(filePath2, "food_production.rds"))

food_prod_current_others <- food_prod_current  %>%
	filter(sector != "dairy") %>%
	left_join(sector_lookup, by = "sector") %>%
	left_join(nutritional_values %>% select(product, protein_g_per_kg_dm), by = "product") %>%
	mutate(
		food_prod_kg_protein_per_region = (food_prod_kg_dm_per_region * protein_g_per_kg_dm) /1000
	) %>%
	select(region, sector, food_prod_kg_protein_per_region)
	
food_prod_current_dairy <- food_prod_current  %>%
	filter(sector == "dairy") %>%
	left_join(dairy_sector_coef_dm_milk_meat, by = "sector") %>%
	left_join(nutritional_values %>% select(product, protein_g_per_kg_dm), by = "product") %>%
	mutate(
		food_prod_kg_protein_per_region = (food_prod_kg_dm_per_region * coef_milk_meat * protein_g_per_kg_dm) /1000
	) %>%
	group_by(region) %>%
	summarise(
		food_prod_kg_protein_per_region = sum(food_prod_kg_protein_per_region, na.rm = TRUE)
	)
food_prod_current_dairy$sector <- "dairy"

food_prod_current_protein <- rbind(food_prod_current_others, food_prod_current_dairy)

food_prod_current <- food_prod_current %>%
	left_join(food_prod_current_protein, by = c("sector", "region"))

food_prod_current_all <- food_prod_current %>%
	mutate(
		food_prod_kg_protein_per_ha = food_prod_kg_protein_per_region / cult_area
	)
	
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

regions_order <- rev(unique(food_prod_current$region))

food_prod_current_all <- food_prod_current_all %>%
	arrange(sector, match(region, regions_order))

nutrient_balance <- nutrient_balance %>%
	arrange(sector, match(region, regions_order))

nutrient_flows <- nutrient_flows %>%
	arrange(sector, match(region, regions_order))

all_emissions <- all_emissions %>%
	arrange(sector, match(region, regions_order))

all_emissions_wm <- all_emissions_wm %>%
	arrange(sector, match(region, regions_order))
		
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write.csv(food_prod_current_all, file.path(filePath2, "food_prod_current_all.csv"))

saveRDS(nutrient_balance, file.path(filePath2, "nutrient_balance.rds"))
write.csv(nutrient_balance, file.path(filePath2, "nutrient_balance.csv"))

saveRDS(nutrient_flows, file.path(filePath2, "nutrient_flows.rds"))
write.csv(nutrient_flows, file.path(filePath2, "nutrient_flows.csv"))

saveRDS(all_emissions, file.path(filePath2, "all_emissions.rds"))
write.csv(all_emissions, file.path(filePath2, "all_emissions.csv"))

saveRDS(all_emissions_wm, file.path(filePath2, "all_emissions_wm.rds"))
write.csv(all_emissions_wm, file.path(filePath2, "all_emissions_wm.csv"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
