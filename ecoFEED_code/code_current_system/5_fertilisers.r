rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Matching fertiliser inputs data identifiers with yield data identifiers
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Fertilisation data; i.e. nutrient inputs in the form of fertilisers applied on fields

# renaming
N_P_input_fert <- read_xlsx(file.path(filePath1, "N_P_input_fert.xlsx"))
N_P_inputs <- N_P_input_fert %>%
	mutate(feed = recode(Crop,
		Wheat = "wheat",
		Rye = "rye",
		Peas = "peas",
		"Faba beans" = "fababean",
		Oats = "oats",
		"Mixed cereals" = "cereal_mix",
		"Silage" = "silage",
		"Clover grass" = "clover_grass",
		"Potatoes" = "potatoes_total",
		"Pasture" = "pasture",
		Flax = "flaxseed",
		Hemp = "hempseed",
		"Barley feed" = "barley",
		"Barley malt" = "barley_malt",
		Rapeseed = "rapeseed",
		Caraway = "caraway",
		.default = "NA"
	)) %>%
	rename(
		region = "Region",
		N_kg_per_ha = "N kg/ha",
		P_kg_per_ha = "P kg/ha"
	)

# duplicating some fertilisation levels for missing plants
N_P_inputs <- N_P_inputs %>%
  bind_rows(
    N_P_inputs %>%
      filter(feed == "rapeseed") %>%   # pick row to duplicate
      mutate(
        feed = "rapeseed_rapa_oleifera"
      )
  )	 %>%
  bind_rows(
    N_P_inputs %>%
      filter(feed == "rapeseed") %>% 
      mutate(
        feed = "rapeseed_napus"
      )
  )	 %>%
  bind_rows(
    N_P_inputs %>%
      filter(feed == "rapeseed") %>% 
      mutate(
        feed = "rapeseed_meal"
      )
  )	 %>%
  bind_rows(
    N_P_inputs %>%
      filter(feed == "hempseed") %>%
      mutate(
      feed = "hempseed_meal"
      )
  )

saveRDS(N_P_inputs, file = file.path(filePath2, "N_P_inputs.rds"))
