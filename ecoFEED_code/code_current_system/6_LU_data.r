rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# mixed production has been allocated to the beef sector, so that
# the following land-use categories are allocated directly to the beef sectors:
	# 210_laidun, 220_siemenheinä, 230_rehunurmet, 338_vihantavilja, 511_nurmet, 512_nurmet
# and the rest is added to the beef and pork sectors according to coefficients,
# and the coefficients have been calculated based on the proportion of total land-use occupied by grass fodder (230_rehunurmet)
# for each region, respectively

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# land-use types
# type (1) lu : area used by the sectors (farms); utilised agricultural area [for describing the sectors' land-use]
# type (2) lu : area used for cultivation (same as type (1) but excl. fallows) [used in almost all calculations, eg nutrients, ghgs]
# type (3) lu : are needed to produce the required feeds [self-sufficiency calculations]

# here, we extract and calculate type (1) and type (2) land-use data
# land self sufficiency is calculated with the other self-sufficiency measure

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# land-use by sector in Finland, for different ag sectors, different cultivated products
LU_by_sector <- read_xlsx(file.path(filePath1, "Land_use_by_sector.xlsx"))

# (FI-EN) dictionary for the land-use sectors, for grouping to plant prod, beef, dairy, etc.
sectors <- readRDS(file = file.path(filePath2, "sectors.rds"))
sectors_lookup <- tibble::enframe(sectors, name = "sector", value = "Tuotantohaara_fi") %>%
	unnest(Tuotantohaara_fi)

LU_by_sect_mod <- LU_by_sector %>%
  filter(VUOSI == 2024,
         Source == "pinta-ala (ha) primaarisuojattu") %>%
  filter(Tuotantohaara_fi %in% unlist(sectors)) %>%
  left_join(sectors_lookup, by = "Tuotantohaara_fi") %>%
	mutate(
    	across(`111_Syysvehnä`:`599_Käytössä oleva maatalousmaa`,
      		   ~ tidyr::replace_na(.x, 0)),
		across(where(is.numeric), ~ replace(.x, .x < 0, NA))
		   )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# adding mixed-sector farms: calculating proportions to be added to beef and pork sectors
mixed_sect_farms_coefs <- LU_by_sect_mod %>%
	filter(sector == "mixed") %>%
	rename(feed_grass = "230_Rehunurmet") %>%
	rename(land_total = "599_Käytössä oleva maatalousmaa") %>%
	rename(region = "aluenimi") %>%
  	mutate(
  		coef_beef = feed_grass / land_total,
  		coef_pork = 1-coef_beef
  	) %>%
	select("region", "coef_beef", "coef_pork")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LU_by_sect_mod <- LU_by_sect_mod %>%
	mutate(
		wheat = rowSums(pick("111_Syysvehnä", "112_Kevätvehnä"), na.rm = TRUE),
		rye = rowSums(pick("121_Syysruis", "122_Kevätruis"), na.rm = TRUE),
		potato = rowSums(pick("311_Ruokaperuna", 
							  "312_Varhaisperuna",
							  "313_Ruokateollisuusperuna",
							  "314_Tärkkelysperuna",
							  "315_Muu peruna"), 
						 na.rm = TRUE),
		fallow = rowSums(pick("421_Kesanto", 
							  "422_Luonnonhoitopelto", 
							  "423_Viherlannoitusnurmi"), 
						 na.rm = TRUE),
		permanent_grass = rowSums(pick("511_Nurmet vähintään 5 vuotta (e",
									   "512_Nurmet vähintään 5 vuotta (l"), 
								  na.rm = TRUE),
		silage = rowSums(pick("230_Rehunurmet",
							  "220_Siemenheinä",
							  "338_Vihantavilja"), 
						 na.rm = TRUE),
		others = rowSums(pick("321_Sokerijuurikas",
						  	  "337_Ruokohelpi",
							  "336_Kumina",
							  "340_Puutarhakasvit", 
							  "351_Muut kasvit",
							  "541_Kotitarvepuutarha", 
							  "521_Monivuotiset puutarhakasvit"), 
						 na.rm = TRUE)
	)

LU_by_sect_mod <- LU_by_sect_mod %>%
  select(-VUOSI,
  		 -Source,
  		 -"111_Syysvehnä", 
  		 -"112_Kevätvehnä",
  		 -"121_Syysruis", 
  		 -"122_Kevätruis",
		 -"220_Siemenheinä",
  		 -"230_Rehunurmet",
		 -"311_Ruokaperuna", 
		 -"312_Varhaisperuna",
		 -"313_Ruokateollisuusperuna",
		 -"314_Tärkkelysperuna",
		 -"315_Muu peruna",
  		 -"338_Vihantavilja",
		 -"421_Kesanto", 
		 -"422_Luonnonhoitopelto", 
		 -"423_Viherlannoitusnurmi",
		 -"511_Nurmet vähintään 5 vuotta (e",
		 -"512_Nurmet vähintään 5 vuotta (l",
		 -"321_Sokerijuurikas",
		 -"337_Ruokohelpi",
		 -"336_Kumina",
		 -"340_Puutarhakasvit", 
		 -"351_Muut kasvit",
		 -"541_Kotitarvepuutarha", 
		 -"521_Monivuotiset puutarhakasvit",
		 -"599_Käytössä oleva maatalousmaa",
		 -"potato",
		 -"others")  

LU_by_sect_mod <- LU_by_sect_mod %>%
  rename(
		region = "aluenimi",
		barley_malt = "127_Mallasohra",
		barley_feed = "128_Rehuohra",
		oats = "141_Kaura",
		cereal_mix = "151_Seosvilja",
		other_cereals = "161_Muut viljat",
		pasture = "210_Laidun",
		peas = "331_Herne",
		fababean = "332_Härkäpapu",
		rapeseed_oleifera = "333_Rypsi",
		rapeseed_napus = "334_Rapsi",
		flaxseed = "335_Öljy- ja kuitupellava"
	) %>%
	select(-c(Tuotantosuunta_fi, Tuotantohaara_fi))

colnames(LU_by_sect_mod) # ALWAYS CHECK

crop_cols <- c("barley_malt", "barley_feed", "oats", "cereal_mix",
  			   "other_cereals", "peas", "fababean",
			   "rapeseed_oleifera", "rapeseed_napus",
			   "wheat", "flaxseed", "rye", "fallow")

mixed_base <- LU_by_sect_mod %>%
	filter(sector == "mixed") %>%
	left_join(mixed_sect_farms_coefs, by = "region")

mixed_to_beef <- mixed_base %>%
	mutate(
		sector = "beef",
		across(all_of(crop_cols), ~ .x * coef_beef)
	) %>%
	select(-coef_beef, -coef_pork)

mixed_to_pork <- mixed_base %>%
	mutate(
		sector = "pork",
		across(all_of(crop_cols), ~ .x * coef_pork),
		pasture = 0,
		permanent_grass = 0,
		silage = 0
	) %>%
	select(-coef_beef, -coef_pork)
  
LU_by_sect_mod2 <- LU_by_sect_mod %>%
	bind_rows(mixed_to_beef,
			  mixed_to_pork) %>%
	filter(sector != "mixed")
							 
# pivot to long format
LU_long <- LU_by_sect_mod2 %>%
	pivot_longer(
    	cols = -c(region, sector),
    	names_to = "feed",
    	values_to = "area"			
	)

LU_by_sect_long <- LU_long %>%
  	group_by(region, sector, feed) %>%
  	summarise(
#     	across(where(is.numeric), sum, na.rm = TRUE)
		area = sum(area, na.rm = TRUE)
 	)
	
# add predefined grouping, to match the animal diet groups
cult_groups <- readRDS(file = file.path(filePath2, "cult_groups.rds"))
cult_lookup <- tibble::enframe(cult_groups, name = "cult_group", value = "feed") %>%
	unnest(feed)
LU_by_sect_long <- LU_by_sect_long %>%
	left_join(cult_lookup, by = "feed")

# predefined grouping, to get yield data to match the land-use-based production
yield_groups <- readRDS(file = file.path(filePath2, "yield_groups.rds"))
yield_lookup <- tibble::enframe(yield_groups, name = "yield_crop", value = "feed") %>%
  unnest(feed)

LU_by_sect_long <- LU_by_sect_long %>%
  left_join(yield_lookup, by = "feed")

# FINAL data table:
# land-use by sector, for all regions and crop products;
# including the grouping to match with yield data ('feed') and diet data ('cult_group')
# = type (1) land use, long format with different crops
saveRDS(LU_by_sect_long, file = file.path(filePath2, "LU_by_sect.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# type (1) land-use, with all land pf the sector, incl. nonproductive fallow
# short format, with total areas per region and sector

LU_by_sect_sums <- LU_by_sect_long %>%
	group_by(region, sector) %>%
	summarise(
		area_sect_sum_by_region = sum(area)
	)

LU_by_sect_cult_sums <- LU_by_sect_long %>%
	group_by(sector, region, cult_group) %>%
	summarise(
		area_grouped = sum(area)
	)

LU_by_sect_cult_sums <- LU_by_sect_cult_sums %>%
  pivot_wider(
    names_from = cult_group,
    values_from = area_grouped
  )

# LU_by_sect_sums %>%
# 	filter(region == "Koko maa")

saveRDS(LU_by_sect_cult_sums, file = file.path(filePath2, "LU_by_sect_cult_sums.rds"))
write.csv(LU_by_sect_cult_sums, file.path(filePath2, "LU_by_sect_cult_sums.csv"))

saveRDS(LU_by_sect_sums, file = file.path(filePath2, "LU_by_sect_sums.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# type (2) land-use, without fallow
# long format, with the different crops

cult_LU_by_sect <- LU_by_sect_long %>%
	filter(feed != "fallow")

saveRDS(cult_LU_by_sect, file = file.path(filePath2, "cult_LU_by_sect.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# across all feedcrop-region-combinations, how much land is there categorised NA;
# to check that this land share is negligible
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LU_by_sect_long_kokomaa <- LU_by_sect_long %>%
	filter(region == "Koko maa")  %>%
  	group_by(sector) %>%
  	summarise(
    	across(where(is.numeric), sum, na.rm = TRUE)
 	) %>%
	rename(area_koko_maa = area)
 
LU_by_sect_long_sums <- LU_by_sect_long %>%
	filter(region != "Koko maa")  %>%
	group_by(sector) %>%
	summarise(
		across(where(is.numeric), sum, na.rm = TRUE)
	) %>%
	rename(area_sum = area)

LU_by_sect_long_sums <- LU_by_sect_long_sums  %>%
	left_join(LU_by_sect_long_kokomaa, by = ("sector"))

LU_by_sect_long_sums <- LU_by_sect_long_sums  %>%
	mutate(
		missing_area_ha = area_koko_maa - area_sum,
		missing_area_perc = (missing_area_ha /area_koko_maa) *100,		
		missing_area_perc_of_total_area = (missing_area_ha / (sum(area_koko_maa))) *100		
		)
# less than 1% is missing of sector totals

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LU_by_sect_area_sums <- LU_by_sect_mod %>%
  filter(region != "Koko maa") %>%
  group_by(sector) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE)
  )	

LU_by_sect_area_kokomaa <- LU_by_sect_mod %>%
  filter(region == "Koko maa") %>%
  group_by(sector) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE)
  )

data.frame(sector = LU_by_sect_area_sums$sector, 
		   round(((LU_by_sect_area_kokomaa[, -1] - LU_by_sect_area_sums[, -1]) / LU_by_sect_area_kokomaa[, -1]) * 100))

data.frame(sector = LU_by_sect_area_sums$sector, 
		   round(LU_by_sect_area_kokomaa[, -1] - LU_by_sect_area_sums[, -1]))

round(((LU_by_sect_area_kokomaa[, -1] - LU_by_sect_area_sums[, -1])/rowSums(LU_by_sect_area_kokomaa[, -1])) *100)
# max 2% is not allocated (sector-feed combinations)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

