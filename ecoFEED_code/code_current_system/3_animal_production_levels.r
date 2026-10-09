rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Calculate animal production levels per animal, herd animal and herd,
# combine milk and beef production from dairy sector,
# save the coefficient of the proportions for later
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Animal production levels
# the quantity of protein and calories (in milk/meat) produced per animal 
# was calculated by multiplying the production per animal 
# by the dry matter content, 
# the protein/energy contents of each product, 
# and by the factor representing the animal's (sector-specific) share in the herd

nutritional_values <- readRDS(file = file.path(filePath2, "nutritional_values.rds"))
animal_production <- readRDS(file = file.path(filePath2, "animal_production.rds"))

# Production per animal and production per herd animal (i.e. multiplied by its share in the herd)
food_prod_per_animal_per_year_all_values <- animal_production %>%
	left_join(nutritional_values[,c("product", "dm_factor")], by = "product") %>%
	mutate(
		dm_prod_kg_per_animal = as.numeric(production_kg_per_animal_per_year) * dm_factor,
		dm_prod_kg_per_herd_animal = dm_prod_kg_per_animal * Share
	)
# gives out warning; NAs caused by animals in the herd that do not have production

# Proportions of milk/meat produced by the dairy cow 
# (milking; culling, surplus heifers; replacement heifers for dairy cows have no production)
dairy_sector_coef_dm_milk_meat <- 
	food_prod_per_animal_per_year_all_values %>%
		filter(region == "Koko maa") %>%
		select(-region) %>%
		filter(sector == "dairy") %>%
		drop_na(dm_prod_kg_per_herd_animal) %>%
		group_by(product) %>%
		summarise(
			dm_tot = sum(dm_prod_kg_per_herd_animal)
		) %>%
		mutate(coef_milk_meat = dm_tot / sum(dm_tot))
saveRDS(dairy_sector_coef_dm_milk_meat, file = file.path(filePath2, "dairy_sector_coef_dm_milk_meat.rds"))

# Food production per animal, combining the different stages:
	# dairy: (milking) dairy cow + surplus heifer + cull dairy cow;
		# (replacement heifers have no production)
	# beef: suckler cow + suckler cull cown + beef_replacement_heifers;
		# (culled breeding bulls factored in the suckler cows)
	# for all the others, the different stages are integrated into the one animal
food_prod_per_animal_per_year <- food_prod_per_animal_per_year_all_values %>%
	group_by(production_per_animal_group, sector, region) %>%
	summarise(
		dm_prod_kg_per_animal = sum(dm_prod_kg_per_animal, na.rm = TRUE),
		dm_prod_kg_per_herd_animal = sum(dm_prod_kg_per_herd_animal, na.rm = TRUE)
	) %>%
	rename(animal = production_per_animal_group) %>%
	filter(animal != "NA")

saveRDS(food_prod_per_animal_per_year_all_values, 
		file = file.path(filePath2, "food_prod_per_animal_per_year_all_values.rds"))
saveRDS(food_prod_per_animal_per_year, 
		file = file.path(filePath2, "food_prod_per_animal_per_year.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++