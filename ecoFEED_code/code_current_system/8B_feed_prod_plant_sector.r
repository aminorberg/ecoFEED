rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# How much feed is produced outside the livestock sector lands?
# (needed for the scenarios)

# mapping feeds to the broader crop groups, 
# calculating how much of each crop group is produced outside livestock production sector, 
# (by comparing how much is produced on livestock sectors' land and tot amounts produced),
# and allocating that production to the plant production sector 
# ie, feeds produced on land belonging to the plant production sector

# this regional allocation of crop groups done based on 
# how the different cult groups are distributed on plant production sector lands

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# feed production by the different livestock sectors
feed_production_by_sect <- readRDS(file = file.path(filePath2, "feed_production_by_sect_feeds.rds"))

cult_LU_by_sect <- readRDS(file = file.path(filePath2, "cult_LU_by_sect.rds"))

# yield data
yields_dm <- readRDS(file = file.path(filePath2, "yields_dm.rds"))

# total feed production in Finland
prod_totals <- read_xlsx(file.path(filePath1, "Feed_production_FI.xlsx"))

cult_groups_lookup <- readRDS(file = file.path(filePath2, "cult_groups.rds")) %>%
	tibble::enframe(name = "cult_group", value = "feed") %>%	
	unnest(feed)

feed_production_cult_groups <- feed_production_by_sect %>%
	left_join(cult_groups_lookup, by = "feed")

feed_production_sums_livestc <- feed_production_cult_groups %>%
	filter(sector != "plant_production") %>%
	filter(region == "Koko maa") %>%
	group_by(cult_group) %>%
  	summarise(harvest_sums = sum(harvest_kg_dm_sums, na.rm = TRUE)) %>%
	left_join(prod_totals, by = "cult_group") %>%
	rename("country_total" = "kg") %>%
	mutate(left_for_plant_prod_kg = country_total - harvest_sums)

feed_production_sums_plants <- feed_production_cult_groups %>%
	filter(region != "Koko maa") %>%
	filter(sector == "plant_production")  %>%
	group_by(cult_group, region) %>%
	summarise(harvest_sums_cultr_group_region = sum(harvest_kg_dm_sums, na.rm = TRUE)) %>%
	mutate(cult_group_share_region = harvest_sums_cultr_group_region / sum(harvest_sums_cultr_group_region))

# allocation of feed production land to the plant production sector, by region
# ie, how much (kg) feed is produced per region

feed_production_alloc_plant_prod_sect <- feed_production_sums_plants %>%
	left_join(feed_production_sums_livestc[, c("cult_group", "left_for_plant_prod_kg")], by = "cult_group") %>%
	mutate(plant_prod_sect_share_feed_prod_kg = cult_group_share_region * left_for_plant_prod_kg) %>%
	select(-left_for_plant_prod_kg)

# the produced amounts are divided by the yields, to het the land-areas (ha)
yields_dm_sub <- yields_dm  %>%
	filter(feed %in% c("barley", "peas", "rapeseed_rapa_oleifera"))  %>%
	mutate(cult_group = fct_recode(
		feed,
      	cereals  = "barley",
      	legumes = "peas",
      	oil  = "rapeseed_rapa_oleifera"
    )) %>%
  select(-crop_dm)

feed_production_alloc_plant_prod_sect <- feed_production_alloc_plant_prod_sect %>%
	left_join(yields_dm_sub, by = c("region", "cult_group")) %>%
	mutate(plant_prod_sect_share_feed_prod_ha = plant_prod_sect_share_feed_prod_kg / dm_yield_kg_per_ha)

saveRDS(feed_production_alloc_plant_prod_sect, file = file.path(filePath2, "feed_production_alloc_plant_prod_sect.rds"))
# = regional land area distribution in the plant production sector, per feed crop type

# this area, located on plant production sector lands, 
# needs to be added to the different livestock sectors, as their land-use
feed_production_by_sect <- feed_production_by_sect %>%
	filter(region != "Koko maa") %>%
	filter(sector != "plant_production") %>%
	group_by(sector, region) %>%
	summarise(area_sector_sum_per_region = sum(area_sums, na.rm = TRUE)) %>%
	group_by(region) %>%
	mutate(
		area_sum_per_region = sum(area_sector_sum_per_region, na.rm = TRUE),
		sector_area_share_per_region = area_sector_sum_per_region / area_sum_per_region,
	)

saveRDS(feed_production_by_sect, file = file.path(filePath2, "feed_production_by_sect.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
