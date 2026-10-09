rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# LAND-USE demand for each livestock sector:
# dividing the feed consumption (for each sector) 
# by the average crop yields, and the land-use factor

# type (3) lu :
# ie, the land use required by the feed consumption,
# aka on-farm lu

# NB!
# animal shares are not used, because we use the actual numbers for the all the animals
# for dairy, the other herd animals are included in the dairy cows;
# for beef, some are included in the suckler cows, but for others we use the real numbers

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

feed_data <- readRDS(file = file.path(filePath2, "feed_data.rds"))
yields_dm <- readRDS(file = file.path(filePath2, "yields_dm.rds"))

cult_yield_rename_lookup <- readRDS(file = file.path(filePath2, "cult_yield_rename.rds")) %>%
	tibble::enframe(name = "yield_from", value = "crop") %>%	
	unnest(crop)

# DAIRY
dairy_feed_use <- readRDS(file = file.path(filePath2, "dairy_feed_use.rds")) %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	mutate(lufac_from = yield_from,
		   lufac_from = recode(lufac_from,
					"rapeseed_rapa_oleifera" = "rapeseed_meal")	
	)

dairy_LU <- dairy_feed_use %>%
	left_join(yields_dm, by = c("yield_from" = "feed", "region"))  %>%
	left_join(feed_data,  by = c("lufac_from" = "feed")) %>%
	mutate(
		land_use_ha = total_feed_cons_kg / (land_use_factor * dm_yield_kg_per_ha),
		land_use_ha_per_animal = feed_consumption_kg / (land_use_factor * dm_yield_kg_per_ha),		
  		land_use_ha_per_animal = if_else(total_feed_cons_kg == 0, 0, land_use_ha_per_animal)
	)
saveRDS(dairy_LU, file = file.path(filePath2, "dairy_LU_on_farm.rds"))

# BEEF
beef_feed_use <- readRDS(file = file.path(filePath2, "beef_feed_use.rds")) %>%
	group_by(region, crop, beef_subsect) %>%
	summarise(
		total_feed_cons_kg = sum(total_feed_cons_kg, na.rm = TRUE),
		feed_cons_kg = sum(feed_consumption_kg, na.rm = TRUE)
	) %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	mutate(lufac_from = yield_from,
		   lufac_from = recode(lufac_from,
					"rapeseed_rapa_oleifera" = "rapeseed_meal"))	

beef_LU <- beef_feed_use %>%
	left_join(yields_dm, by = c("yield_from" = "feed", "region"))  %>%
	left_join(feed_data,  by = c("lufac_from" = "feed")) %>%
	mutate(
		land_use_ha = total_feed_cons_kg / (land_use_factor * dm_yield_kg_per_ha),
		land_use_ha_per_animal = feed_cons_kg / (land_use_factor * dm_yield_kg_per_ha),
  		land_use_ha_per_animal = if_else(total_feed_cons_kg == 0, 0, land_use_ha_per_animal)
	)
saveRDS(beef_LU, file = file.path(filePath2, "beef_LU_on_farm.rds"))

# PORK
pork_feed_use <- readRDS(file = file.path(filePath2, "pork_feed_use.rds")) %>%
# 	filter(!str_detect(crop, regex("industrial", ignore_case = TRUE))) %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	mutate(lufac_from = yield_from,
		   lufac_from = recode(lufac_from,
					"rapeseed_rapa_oleifera" = "rapeseed_meal")	
	)
	
pork_LU <- pork_feed_use %>%
	left_join(yields_dm, by = c("yield_from" = "feed", "region"))  %>%
	left_join(feed_data,  by = c("lufac_from" = "feed")) %>%
	mutate(
		land_use_ha = total_feed_cons_kg / (land_use_factor * dm_yield_kg_per_ha),
		land_use_ha_per_animal = feed_consumption_kg / (land_use_factor * dm_yield_kg_per_ha),			
  		land_use_ha_per_animal = if_else(total_feed_cons_kg == 0, 0, land_use_ha_per_animal)
  	)
saveRDS(pork_LU, file = file.path(filePath2, "pork_LU_on_farm.rds"))

# SHEEP
sheep_feed_use <- readRDS(file = file.path(filePath2, "sheep_feed_use.rds")) %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	mutate(lufac_from = yield_from,
		   lufac_from = recode(lufac_from,
					"rapeseed_rapa_oleifera" = "rapeseed_meal")	
	)

sheep_LU <- sheep_feed_use %>%
	left_join(yields_dm, by = c("yield_from" = "feed", "region"))  %>%
	left_join(feed_data,  by = c("lufac_from" = "feed")) %>%
	mutate(
		land_use_ha = total_feed_cons_kg / (land_use_factor * dm_yield_kg_per_ha),
		land_use_ha_per_animal = feed_consumption_kg / (land_use_factor * dm_yield_kg_per_ha),		
  		land_use_ha_per_animal = if_else(total_feed_cons_kg == 0, 0, land_use_ha_per_animal)
	)
saveRDS(sheep_LU, file = file.path(filePath2, "sheep_LU_on_farm.rds"))

# POULTRY
poultry_feed_use <- readRDS(file = file.path(filePath2, "poultry_feed_use.rds")) %>%
	left_join(cult_yield_rename_lookup, by = "crop")

poultry_feed_use <- poultry_feed_use %>%
	mutate(lufac_from = yield_from,
		   lufac_from = recode(lufac_from,
					"rapeseed_rapa_oleifera" = "rapeseed_meal")	
	)

poultry_LU <- poultry_feed_use %>%
	left_join(yields_dm, by = c("yield_from" = "feed", "region"))  %>%
	left_join(feed_data,  by = c("lufac_from" = "feed")) %>%
	mutate(
		land_use_ha = total_feed_cons_kg / (land_use_factor * dm_yield_kg_per_ha),
		land_use_ha_per_animal = feed_consumption_kg / (land_use_factor * dm_yield_kg_per_ha),		
  		land_use_ha_per_animal = if_else(total_feed_cons_kg == 0, 0, land_use_ha_per_animal)
	)
saveRDS(poultry_LU, file = file.path(filePath2, "poultry_LU_on_farm.rds"))


# EGGS
eggs_feed_use <- readRDS(file = file.path(filePath2, "eggs_feed_use.rds")) %>%
	left_join(cult_yield_rename_lookup, by = "crop")

eggs_feed_use <- eggs_feed_use %>%
	mutate(lufac_from = yield_from,
		   lufac_from = recode(lufac_from,
					"rapeseed_rapa_oleifera" = "rapeseed_meal")	
	)

eggs_LU <- eggs_feed_use %>%
	left_join(yields_dm, by = c("yield_from" = "feed", "region"))  %>%
	left_join(feed_data,  by = c("lufac_from" = "feed")) %>%
	mutate(
		land_use_ha = total_feed_cons_kg / (land_use_factor * dm_yield_kg_per_ha),
		land_use_ha_per_animal = feed_consumption_kg / (land_use_factor * dm_yield_kg_per_ha),		
  		land_use_ha_per_animal = if_else(total_feed_cons_kg == 0, 0, land_use_ha_per_animal)
	)
saveRDS(eggs_LU, file = file.path(filePath2, "eggs_LU_on_farm.rds"))
