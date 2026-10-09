rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Feed production per sector:
# crop production areas multiplied by the crop-specific attributes, for each livestock sector

# lookup for matching diets, feeds, and yields; land-use factors
current_diet_feed_data <- read_xlsx(file.path(filePath1, "Current_feed_data.xlsx"))
saveRDS(current_diet_feed_data, file = file.path(filePath2, "current_diet_feed_data.rds"))

# type (1) land-area data
LU_by_sect <- readRDS(file = file.path(filePath2, "LU_by_sect.rds"))

# type (2) land-area data
# cult_LU_by_sect <- readRDS(file = file.path(filePath2, "cult_LU_by_sect.rds"))

# nutritional/nutrient data for the feeds
feed_data <- readRDS(file = file.path(filePath2, "feed_data.rds"))

# yield data
yields_dm <- readRDS(file = file.path(filePath2, "yields_dm.rds"))

# total feed production in Finland
prod_totals <- read_xlsx(file.path(filePath1, "Feed_production_FI.xlsx"))

# calculating harvest (production) levels for all crops, across sectors and regions
feed_production_by_sect <- LU_by_sect %>%
	left_join(yields_dm, by = c("yield_crop" = "feed", "region")) %>%
	left_join(feed_data, by = c("yield_crop" = "feed")) %>%
	mutate(
		harvest_dm_kg = area * dm_yield_kg_per_ha,
		harvest_prot_kg = (harvest_dm_kg * CP_g_per_kg_dm) /1000,
		harvest_E_MJ = harvest_dm_kg * ME_MJ_per_kg_dm
	)

# summarising harvest (production) for each sector-region-feed-combination;
# this sums together different products, according to the yield_groups
feed_production_by_sect <- feed_production_by_sect %>%
	group_by(region, sector, feed) %>%
  	summarise(
		area_sums = sum(area, na.rm = TRUE),
		harvest_kg_dm_sums = sum(harvest_dm_kg, na.rm = TRUE),
		harvest_prot_kg_sums = sum(harvest_prot_kg, na.rm = TRUE),
		harvest_E_MJ_sums = sum(harvest_E_MJ, na.rm = TRUE)
  	)
# fallow yield is NA, so does not contribute to feed harvest

saveRDS(feed_production_by_sect, file = file.path(filePath2, "feed_production_by_sect_feeds.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++