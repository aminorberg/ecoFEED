rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Normalize shared yield, feed and nutritional inputs,
# standardize feed and product identifiers, 
# and save prepared RDS-files for later workflow stages
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Transform crop yield data, feed composition data, and product nutritional value data

# ELY centres
ELY_centres <- read_xlsx(file.path(filePath1, "ELY_centres.xlsx"))
saveRDS(ELY_centres, file = file.path(filePath2, "ely_centres.rds"))

# yields
yields_dm <- read_xlsx(file.path(filePath1, "Yields_DM.xlsx")) %>%
	pivot_longer(
		cols = -Region,
		names_to = c("crop_dm"),
		values_to = "dm_yield_kg_per_ha") %>%
	mutate(feed = recode(crop_dm,
			   Wheat_DM = "wheat",
			   Rye_DM = "rye",
			   Barlea_matl_DM = "barley_malt",
			   Barley_feed_DM = "barley",
			   Oats_DM = "oats",
			   Mix_crop_DM = "cereal_mix",
			   Silage_DM = "silage",
			   Clover_grass = "clover_grass",
			   Potatoes_food_DM = "potatoes_food",
			   "Potatoes total_DM" = "potatoes_total",
			   Peas_feed_DM = "peas",
			   Braod_bean_DM = "fababean",
			   Turnip_rape_DM = "rapeseed_rapa_oleifera",
			   Rapeseed_DM = "rapeseed_napus",
			   Pasture = "pasture",
			   Flaxseed_DM = "flaxseed",
			   Oilhemp_DM = "hempseed",
			   Caraway_DM = "caraway",
			   "Semi.natural.grasslands" = "seminat_grassl"  
    	))

yields_dm <- yields_dm %>%
	rename(region = Region) %>%
	mutate(region = recode(region, "Finland" = "Koko maa"))

# using the rapeseed_napus and hempseed yield data for rapeseed meal and hempseed meal
yields_dm <- yields_dm %>%
  bind_rows(
    yields_dm %>%
      filter(feed == "rapeseed_napus") %>% 
      mutate(
        feed = "rapeseed_meal"
      )
  ) %>%
  bind_rows(
    yields_dm %>%
      filter(feed == "hempseed") %>% 
      mutate(
        feed = "hempseed_meal"
      )
  )

# Normalize feed composition table columns and map raw feed names to the
# standard feed identifiers (used throughout the analysis)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

feed_data <- read_xlsx(file.path(filePath1, "Feed_data.xlsx"))

feed_data <- feed_data %>%
	rename(
		dm_g_per_kg = "DM  g/kg",
		GE_MJ_per_kg_dm = "GE MJ/kg DM",
		ME_MJ_per_kg_dm = "ME MJ/kg DM",
		CP_g_per_kg_dm = "CP g/kg DM",
		N_g_per_kg_dm = "N g/kg DM",
		P_g_per_kg_dm = "P g/kg DM",
		P_factor = "P factor",
		N_factor = "N factor",
		land_use_factor = "Land use"
	) %>%	
	mutate(
		feed = case_when(
			str_detect(Feed, "clover") ~ "clover_grass",
			str_detect(Feed, "silage") ~ "silage",
			str_detect(Feed, "Barley protein")  ~ "OVR",
			str_detect(Feed, "Barley")  ~ "barley",
			str_detect(Feed, "Oats") ~ "oats",
			str_detect(Feed, "Wheat") ~ "wheat",
			str_detect(Feed, "Rye") ~ "rye",
			str_detect(Feed, "Pea") ~ "peas",
			str_detect(Feed, "Faba") ~ "fababean",
			str_detect(Feed, "Rapeseed meal") ~ "rapeseed_meal",
			str_detect(Feed, "napus") ~ "rapeseed_napus",
			str_detect(Feed, "rapa") ~ "rapeseed_rapa_oleifera",
			str_detect(Feed, "Flax meal") ~ "flaxseed_meal",
			str_detect(Feed, "Flax") ~ "flaxseed",
			str_detect(Feed, "Hemp seed expeller") ~ "hempseed_meal",
			str_detect(Feed, "Hemp") ~ "hempseed",
			str_detect(Feed, "Whey") ~ "whey_prot_meal",
			str_detect(Feed, "BSG") ~ "BSG",
			str_detect(Feed, "Vegetable oil") ~ "vegetable_oil",
			str_detect(Feed, "Industrial byproducts") ~ "industrial_byproducts",
			str_detect(Feed, "Liquid feed") ~ "liquid_feed",
			TRUE ~ Feed
		)
	)
 
# using barley feed data for cereal mix
feed_data <- feed_data %>%
  bind_rows(
    feed_data %>%
      filter(feed == "barley") %>% 
      mutate(
        feed = "cereal_mix"
      )
  )

# Nutrient densities for final animal products; standardising product names
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nutritional_values <- read_xlsx(file.path(filePath1, "Nutritional_values.xlsx"))
colnames(nutritional_values)[1] <- "product"

nutritional_values <- nutritional_values %>%
	mutate(product = fct_recode(product,
    	"milk" = "Milk",
    	"beef_meat" = "Beef",
    	"pork_meat" = "Pork",
    	"eggs" = "Eggs",
    	"poultry_meat" = "Poultry",
    	"sheep_meat" = "Sheep"
  	))

nutritional_values <- nutritional_values %>%
	rename(
    	dm_factor = "DM factor",
    	E_kcal_per_kg_dm = "Energy  kcal/kg DM ",
    	protein_g_per_kg_dm = "Protein g/kg DM",
    	P_g_per_kg_dm = "P g/ kg DM"
	)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveRDS(nutritional_values, file = file.path(filePath2, "nutritional_values.rds"))
saveRDS(yields_dm, file = file.path(filePath2, "yields_dm.rds"))
saveRDS(feed_data, file = file.path(filePath2, "feed_data.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
