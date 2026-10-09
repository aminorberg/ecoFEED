rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Nutrients in manure:
# nutrient input in feed - output in milk/meat = output in manure
# (ignoring what goes to eg metabolism)

# N input in manure:   
# N content in the animal product (protein produced divided by 6.25),
# subtracted from the sum of the N contents by feed type

# P input in manure:   
# P content content in the animal product (dm production * P g/kg DM)
# subtracted from the sum of the P contents by feed type

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Nutrients in harvested feed crops: 
# nutrient consumption in feed divided by the total land use for producing feeds

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

animal_production <- readRDS(file = file.path(filePath2, "animal_production.rds"))
feed_data <- readRDS(file = file.path(filePath2, "feed_data.rds"))

nutritional_values <- readRDS(file = file.path(filePath2, "nutritional_values.rds"))

food_prod_per_animal_per_year_all_values <- 
	readRDS(file = file.path(filePath2, "food_prod_per_animal_per_year_all_values.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

yield_groups_lookup <- readRDS(file = file.path(filePath2, "yield_groups.rds")) %>%
	tibble::enframe(name = "crop", value = "feed") %>%	
	unnest(feed)

cult_yield_rename_lookup <- readRDS(file = file.path(filePath2, "cult_yield_rename.rds")) %>%
	tibble::enframe(name = "yield_from", value = "crop") %>%	
	unnest(crop)
	
dairy_diet <- readRDS(file = file.path(filePath2, "dairy_diet.rds"))
beef_diets <- readRDS(file = file.path(filePath2, "beef_diets.rds"))

dairy_animal_no <- readRDS(file = file.path(filePath2, "dairy_animal_no.rds"))
beef_animal_no <- readRDS(file = file.path(filePath2, "beef_animal_no.rds"))

all_animals <- c("dairy", "beef", "pork", "poultry", "sheep", "eggs")	
noncow_animals <- c("pork", "poultry", "sheep", "eggs")	
	LU_current_noncow <- 
	diets_current_noncow <- setNames(vector("list", length(noncow_animals)), noncow_animals)
	for (ani in noncow_animals) {
		diets_current_noncow[[ani]] <- readRDS(file = paste0(filePath2, "/", ani, "_diet.rds"))
	}

cult_LU_by_sect <- readRDS(file = file.path(filePath2, "cult_LU_by_sect.rds"))
cult_LU_by_sect_areas <- cult_LU_by_sect %>%
	group_by(sector, region) %>%
	summarise(
		cult_area = sum(area)
	)
	
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

N_P_in_diets <- P_N_inputs_manure <- P_N_in_manure_per_ha <- setNames(vector("list", length(all_animals)), all_animals)

# dairy
# inputs in diet: how much nutrients the animals eat in their feed?
		# nutrients in diet are calculated by multiplying the feed consumption (kg/animal/year)
		# by the nutrient factors
		# -> nutrient contents of the diet, same across the country
		# unit: total kg of N/P per animal per year
dairy_N_P_in_diet <- dairy_diet %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%	
	left_join(feed_data, by = c("yield_from" = "feed")) %>%
	rename(feed = yield_from) %>%
	mutate(		
		N_in_diet_kg_per_animal_per_year = N_factor * feed_consumption_kg,
		P_in_diet_kg_per_animal_per_year = P_factor * feed_consumption_kg
	)
# N_P_in_diets[["dairy"]] <- dairy_N_P_in_diet
dairy_N_P_in_diet$sector <- "dairy"
dairy_N_P_in_diet$beef_subsect <- NA
N_P_in_diets <- dairy_N_P_in_diet

dairy_N_P_in_diet_totals <- dairy_N_P_in_diet %>%
	summarise(
		N_in_diet_all_feeds = sum(N_in_diet_kg_per_animal_per_year, na.rm = TRUE),
		P_in_diet_all_feeds = sum(P_in_diet_kg_per_animal_per_year, na.rm = TRUE)
	)

# total inputs in manure
	# calculated as on the difference between the produced milk/meat
	# and the nutrients of the whole animal diet (incl. byproducts)
P_N_inputs_manure_dairy <- food_prod_per_animal_per_year_all_values %>%
	filter(animal %in% c("dairy_cow", "dairy_cull_cow", "dairy_surplus_heifer")) %>%
	left_join(nutritional_values %>% select(-dm_factor), by = "product") %>%
	group_by(region) %>%
	summarise(
		N_input_manure = dairy_N_P_in_diet_totals$N_in_diet_all_feeds -
						 sum((dm_prod_kg_per_herd_animal * protein_g_per_kg_dm) /1000 /6.25,  na.rm = TRUE),
		P_input_manure = dairy_N_P_in_diet_totals$P_in_diet_all_feeds -
						 sum(P_g_per_kg_dm * dm_prod_kg_per_herd_animal /1000, 
							 na.rm = TRUE)
	)
P_N_inputs_manure_dairy$sector <- "dairy"
P_N_inputs_manure <- P_N_inputs_manure_dairy

# inputs in manure per ha, by region
	# inputs in manure per animal are the same, but animal numbers, harvest levels, etc, depend on the region
P_N_in_manure_dairy_per_ha <- cult_LU_by_sect_areas %>%
	filter(sector == "dairy") %>%
	left_join(dairy_animal_no, by = "region") %>%
	transmute(
		region = region,
		cult_area = cult_area,
		N_input_manure_kg_per_ha =
			(P_N_inputs_manure_dairy$N_input_manure * no_of_animals) / cult_area,
		P_input_manure_kg_per_ha =
			(P_N_inputs_manure_dairy$P_input_manure * no_of_animals) / cult_area
	)
P_N_in_manure_per_ha$sector <- "dairy"
P_N_in_manure_per_ha <- P_N_in_manure_dairy_per_ha

# beef

# inputs in diet
beef_N_P_in_diet <- beef_diets %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	left_join(feed_data, by = c("yield_from" = "feed")) %>%
	rename(feed = yield_from) %>%
	mutate(
		N_in_diet_kg_per_animal_per_year = N_factor * feed_consumption_kg,
		P_in_diet_kg_per_animal_per_year = P_factor * feed_consumption_kg
	)
beef_N_P_in_diet$sector <- "beef"
N_P_in_diets <- bind_rows(N_P_in_diets, beef_N_P_in_diet)

beef_N_P_in_diet_totals <- beef_N_P_in_diet %>%
	group_by(beef_subsect) %>%
	summarise(
		N_in_diet_all_feeds = sum(N_in_diet_kg_per_animal_per_year, na.rm = TRUE),
		P_in_diet_all_feeds = sum(P_in_diet_kg_per_animal_per_year, na.rm = TRUE)
	)

# total inputs in manure		
P_N_inputs_manure_beef_shares <- food_prod_per_animal_per_year_all_values %>%
	filter(animal %in% c("heifers_from_dairy", 
						 "bulls_from_dairy", 
						 "beef_suckler_cows", 
						 "beef_cull_suckler_cows",
						 "beef_heifers", 
						 "beef_bulls")) %>%
	left_join(beef_N_P_in_diet_totals, by = c("animal" = "beef_subsect")) %>%
	left_join(nutritional_values %>% select(-dm_factor), by = "product") %>%
	group_by(production_per_animal_group, region) %>%
		summarise(
			N_input_manure = sum(N_in_diet_all_feeds, na.rm = TRUE) -
							 sum((dm_prod_kg_per_animal * protein_g_per_kg_dm) /1000 /6.25, na.rm = TRUE),
			P_input_manure = sum(P_in_diet_all_feeds, na.rm = TRUE) -
							 sum(P_g_per_kg_dm * dm_prod_kg_per_animal /1000, na.rm = TRUE)
		) %>%
	left_join(animal_production[, c("region", "animal", "Share")], by = c("production_per_animal_group" = "animal", "region")) %>%
	group_by(region) %>%
	mutate(
		N_input_manure_kg_share =
			(N_input_manure * Share),
		P_input_manure_kg_share =
			(P_input_manure * Share)
	)

P_N_inputs_manure_beef <- P_N_inputs_manure_beef_shares %>%
	summarise(
		N_input_manure = sum(N_input_manure_kg_share),
		P_input_manure = sum(P_input_manure_kg_share)
	)
P_N_inputs_manure_beef$sector <- "beef"
P_N_inputs_manure <- bind_rows(P_N_inputs_manure, P_N_inputs_manure_beef)

P_N_in_manure_beef_per_ha <- cult_LU_by_sect_areas %>%
	filter(sector == "beef") %>%
	left_join(beef_animal_no, by = "region") %>%
	transmute(
		region = region,
		cult_area = cult_area,
		N_input_manure_kg_per_ha =
			(P_N_inputs_manure_beef$N_input_manure * no_of_animals) / cult_area,
		P_input_manure_kg_per_ha =
			(P_N_inputs_manure_beef$P_input_manure * no_of_animals) / cult_area
	)
P_N_in_manure_beef_per_ha$sector <- "beef"
P_N_in_manure_per_ha <- bind_rows(P_N_in_manure_per_ha, P_N_in_manure_beef_per_ha)

# monogastrics & sheep

for (ani in noncow_animals) {

	# inputs in diet
	nutr_in_diet <- NA
	nutr_in_diet <- diets_current_noncow[[ani]] %>%
		left_join(cult_yield_rename_lookup, by = "crop") %>%
		left_join(feed_data, by = c("yield_from" = "feed")) %>%
		rename(feed = yield_from) %>%
		mutate(
			N_in_diet_kg_per_animal_per_year = N_factor * feed_consumption_kg,
			P_in_diet_kg_per_animal_per_year = P_factor * feed_consumption_kg
		)
	nutr_in_diet$sector <- ani
	N_P_in_diets <- bind_rows(N_P_in_diets, nutr_in_diet)
	
	# total inputs in manure
	P_N_inputs_manure <- food_prod_per_animal_per_year_all_values %>%
		filter(animal %in% ani) %>%
		left_join(nutritional_values %>% select(-dm_factor), by = "product") %>%
		group_by(region) %>%
			summarise(
				N_input_manure = sum(nutr_in_diet$N_in_diet_kg_per_animal_per_year, na.rm = TRUE) -
								 sum((dm_prod_kg_per_animal * protein_g_per_kg_dm) /1000 /6.25, na.rm = TRUE),
				P_input_manure = sum(nutr_in_diet$P_in_diet_kg_per_animal_per_year, na.rm = TRUE) -
								 sum((P_g_per_kg_dm * dm_prod_kg_per_animal) /1000, na.rm = TRUE)
			) %>%
		mutate(sector = ani) %>%
		bind_rows(P_N_inputs_manure)
		
	# inputs in manure per ha, by region
	
	animal_no <- NA
	animal_no <- readRDS(file = paste0(filePath2, "/", ani, "_animal_no.rds"))
	
	P_N_in_manure_per_ha <- cult_LU_by_sect_areas %>%
		filter(sector == ani) %>%
		left_join(animal_no, by = "region") %>%
		left_join(P_N_inputs_manure, by = c("region", "sector")) %>%
		transmute(
			region = region,
			cult_area = cult_area,
			N_input_manure_kg_per_ha =
				(N_input_manure * no_of_animals) / cult_area,
			P_input_manure_kg_per_ha =
				(P_input_manure * no_of_animals) / cult_area
		)  %>%
		bind_rows(P_N_in_manure_per_ha)

}

# nutrients removed as harvest (i.e. output as harvest), for all sectors
N_P_in_harvest <- as_tibble(readRDS(file = file.path(filePath2, "feed_production_by_sect_feeds.rds"))) %>%		
	left_join(yield_groups_lookup, by = "feed") %>%
	left_join(feed_data, by = c("crop" = "feed")) %>%
	mutate(
		N_in_harvest = harvest_kg_dm_sums * N_factor,
		P_in_harvest = harvest_kg_dm_sums * P_factor
	) %>%
	left_join(cult_LU_by_sect, by = c("sector", "region", "feed")) %>%
	mutate(
		N_in_harvest_per_ha = N_in_harvest / area,
		P_in_harvest_per_ha = P_in_harvest / area
	)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveRDS(N_P_in_harvest, file.path(filePath2, "N_P_in_harvest.rds"))
saveRDS(N_P_in_diets, file.path(filePath2, "N_P_in_diets.rds"))
saveRDS(P_N_inputs_manure, file.path(filePath2, "P_N_inputs_manure.rds"))
saveRDS(P_N_inputs_manure_beef_shares, file.path(filePath2, "P_N_inputs_manure_beef_shares.rds"))
saveRDS(P_N_in_manure_per_ha, file.path(filePath2, "P_N_in_manure_per_ha.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

