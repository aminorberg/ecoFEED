rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Self-sufficiency on-farm

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# calculating the land and protein self-sufficiency of the farms, regarding animal feeds
# ie, to what extent the farms could produce the feed they need for their animals,
# in terms of land-are and protein (with their current crop production structure)

sectors <- c("dairy", "beef", "pork", "sheep", "poultry", "eggs")

feed_data <- readRDS(file = file.path(filePath2, "feed_data.rds"))

cult_groups_lookup <- readRDS(file = file.path(filePath2, "cult_groups.rds")) %>%	
	tibble::enframe(name = "cult_group", value = "feed") %>%	
	unnest(feed)

cult_diet_groups_lookup <- readRDS(file = file.path(filePath2, "cult_diet_groups.rds")) %>%	
	tibble::enframe(name = "cult_group", value = "crop") %>%	
	unnest(crop)

# calculating feed production levels for groups of cultivated feeds (matching the diets)
# using the food production data from the livestock sectors
feed_production_by_sect <- readRDS(file = file.path(filePath2, "feed_production_by_sect_feeds.rds")) %>%
	left_join(cult_groups_lookup, by = "feed")

feed_prod_by_sect_crop_groups <- feed_production_by_sect %>%
	group_by(region, sector, cult_group) %>%
  	summarise(
  		crop_group_area_sums = sum(area_sums, na.rm = TRUE),
  		crop_group_harvest_kg_dm_sums = sum(harvest_kg_dm_sums, na.rm = TRUE),
  		crop_group_harvest_prot_kg_sums = sum(harvest_prot_kg_sums, na.rm = TRUE),
  		crop_group_harvest_E_MJ_sums = sum(harvest_E_MJ_sums, na.rm = TRUE)
  	)
saveRDS(feed_prod_by_sect_crop_groups, file = file.path(filePath2, "feed_prod_by_sect_crop_groups.rds"))

feed_prod_by_sect_region_sums <- feed_production_by_sect %>%
	group_by(region, sector) %>%
  	summarise(
  		area_sums = sum(area_sums, na.rm = TRUE),
  		harvest_kg_dm_sums = sum(harvest_kg_dm_sums, na.rm = TRUE),
  		harvest_prot_kg_sums = sum(harvest_prot_kg_sums, na.rm = TRUE),
  		harvest_E_MJ_sums = sum(harvest_E_MJ_sums, na.rm = TRUE)
  	)
saveRDS(feed_prod_by_sect_region_sums, file = file.path(filePath2, "feed_prod_by_sect_region_sums.rds"))

# LAND: feed self-sufficiency: 
# current feed (grass, cereals, annual legumes and oilseed crops, ie cult groups) 
# production area on livestock farm sectors 
# divided by the total livestock-sector specific land area required for feed production
# calculating the land-area required for feed production

for (sect in sectors) {
	sect_LU <- NA
	sect_LU <- readRDS(file = paste0(filePath2, "/", sect, "_LU_on_farm.rds")) %>%
		left_join(cult_diet_groups_lookup, by = "crop")

	sect_LU_by_cult <- sect_LU %>%
		group_by(region, cult_group)  %>%
		summarise(crop_group_area_sums_required = sum(land_use_ha, na.rm = TRUE))

	sect_LU_by_reg <- sect_LU %>%
		group_by(region)  %>%
		summarise(area_sums_required = sum(land_use_ha, na.rm = TRUE))
	
	land_self_sufficiency <- NA
	land_self_sufficiency <- feed_prod_by_sect_crop_groups %>%
		filter(sector == sect) %>%
		left_join(sect_LU_by_cult, by = c("cult_group", "region")) %>%
		mutate(
			area_difference = crop_group_area_sums - crop_group_area_sums_required,
			area_selfsuff = (crop_group_area_sums / crop_group_area_sums_required) *100
		)  	

	land_self_sufficiency_sums <- NA
	land_self_sufficiency_sums <- feed_prod_by_sect_region_sums %>%
		filter(sector == sect) %>%
		left_join(sect_LU_by_reg, by = "region") %>%
		mutate(
			area_difference = area_sums - area_sums_required,
			area_selfsuff = (area_sums / area_sums_required) *100
		)  	

	if (sect == "dairy") {
		self_suff <- land_self_sufficiency_sums	
	} else {
		self_suff <- self_suff %>%
			bind_rows(land_self_sufficiency_sums)	
	}
}

saveRDS(self_suff, file = file.path(filePath2, "land_self_sufficiency.rds"))
write.csv(self_suff, file.path(filePath2, "land_self_sufficiency.csv"))


# NOTE: if the self sufficiency is 
# 	Inf ==> there is no area required for production, ie the diets do not need it
#	0 ==> there is no area currently for producing the required feed
#	NA ==> either the requirement (most likely) or the current area used is NA
#	NaN ==> there is no requirement nor the current area used, ie both == 0

# PROTEIN: feed self-sufficiency:
# Protein production (as feeds) divided by the protein consumption in animal diets;
# = sector-specific feed-protein production / feed-protein use

current_diet_feed_data <- readRDS(file = file.path(filePath2, "current_diet_feed_data.rds")) %>%
	left_join(feed_data, by = c("feed_data_key" = "feed"))

for (sect in sectors) {

	sect_feed_use <- readRDS(file = paste0(filePath2, "/", sect, "_feed_use.rds")) %>%
		left_join(current_diet_feed_data, by = c("crop" = "feed")) %>%
		left_join(cult_diet_groups_lookup, by = "crop")

	sect_feed_use_by_crop_group <- sect_feed_use %>%
		group_by(region, cult_group)  %>%
		summarise(
			feed_use_required_dm_kg = sum(total_feed_cons_kg, na.rm = TRUE),
			feed_use_required_protein_kg = sum(total_feed_cons_kg * CP_g_per_kg_dm, na.rm = TRUE) /1000
		)

	sect_feed_use_sums <- sect_feed_use %>%
		group_by(region)  %>%
		summarise(
			feed_use_required_dm_kg = sum(total_feed_cons_kg, na.rm = TRUE),
			feed_use_required_protein_kg = sum(total_feed_cons_kg * CP_g_per_kg_dm, na.rm = TRUE) /1000
		)
	
	protein_self_sufficiency <- NA
	protein_self_sufficiency <- feed_prod_by_sect_crop_groups %>%
		filter(sector == sect) %>%
		left_join(sect_feed_use_by_crop_group, by = c("cult_group", "region")) %>%
		mutate(
			dm_selfsuff = (crop_group_harvest_kg_dm_sums / feed_use_required_dm_kg) *100,
			dm_difference = crop_group_harvest_kg_dm_sums - feed_use_required_dm_kg,
			protein_selfsuff = (crop_group_harvest_prot_kg_sums / feed_use_required_protein_kg) *100,
			protein_difference = crop_group_harvest_prot_kg_sums - feed_use_required_protein_kg
		)  	

	protein_self_sufficiency_sums <- NA
	protein_self_sufficiency_sums <- feed_prod_by_sect_region_sums %>%
		filter(sector == sect) %>%
		left_join(sect_feed_use_sums, by = "region") %>%
		mutate(
			dm_selfsuff = (harvest_kg_dm_sums / feed_use_required_dm_kg) *100,
			dm_difference = harvest_kg_dm_sums - feed_use_required_dm_kg,
			protein_selfsuff = (harvest_prot_kg_sums / feed_use_required_protein_kg) *100,
			protein_difference = harvest_prot_kg_sums - feed_use_required_protein_kg
		)  	
	
	if (sect == "dairy") {
		prot_self_suff <- protein_self_sufficiency_sums	
	} else {
		prot_self_suff <- prot_self_suff %>%
			bind_rows(protein_self_sufficiency_sums)	
	}
}

saveRDS(prot_self_suff, file = file.path(filePath2, "protein_self_sufficiency.rds"))
write.csv(prot_self_suff, file.path(filePath2, "protein_self_sufficiency.csv"))

