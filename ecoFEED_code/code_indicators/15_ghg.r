rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

feed_data <- readRDS(file = file.path(filePath2, "feed_data.rds"))

GHG_factors <- read_xlsx(file.path(filePath3, "GHG_factors.xlsx"))
GHG_factors <- as.data.frame(GHG_factors)

# CH4 emissions (CO2eq) from manure management:  
# Animal specific CH4 Manure factor [GHG_factors: CH4 manure]
# multiplied by the GWP methane factor [GHG_factors: GWP methane] 
# result: CO2eqvs per animal per year

manure_manag_ghg <- GHG_factors %>%
	filter(str_detect(factor_type, "manure")) %>%
	mutate(
		CO2eqv_per_ani_per_year = 
			factor_value * 
			GHG_factors[which(GHG_factors$factor_type == "GWP methane"), "factor_value"]
	) 
manure_manag_ghg <- as.data.frame(manure_manag_ghg)
saveRDS(manure_manag_ghg, file.path(filePath2, "manure_manag_ghg.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

all_animals <- c("dairy", "beef", "pork", "poultry", "sheep", "eggs")	

cows <- c("dairy", "beef")
noncow_animals <- c("pork", "poultry", "sheep", "eggs")	
CO2eqv_emissions_fromN2O <- setNames(vector("list", length(all_animals)), all_animals)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Methane emission factor (EF) for methane from enteric fermentation in livestock
# GE: Gross energy intake per day
# Ym: The methane conversion factor, 
# representing the fraction of gross energy intake converted to methane 
# (e.g., as a percentage, or a value between 0 and 1).  
# 365: The number of days in a year
# 55.65 The energy in one kilogram of methane (\(MJ/kg\ CH_{4}\)
# EF = (GE * (Ym/100) * 365)/55.65

Ym_dairy <- GHG_factors %>%
	filter(
		factor_type == "CH4 conversion factor",
		animal == "dairy"
	) %>%
	pull(factor_value)

Ym_beef <- GHG_factors %>%
	filter(
		factor_type == "CH4 conversion factor",
		animal == "beef"
	) %>%
	pull(factor_value)

Ym_sheep <- GHG_factors %>%
	filter(
		factor_type == "CH4 conversion factor",
		animal == "sheep"
	) %>%
	pull(factor_value)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

diet_dairy <- readRDS(file.path(filePath2, "dairy_diet.rds"))
diets_beef <- readRDS(file.path(filePath2,"beef_diets.rds"))
diet_sheep <- readRDS(file.path(filePath2,"sheep_diet.rds"))

cult_yield_rename <- readRDS(file = file.path(filePath2, "cult_yield_rename.rds"))
cult_yield_rename_lookup <- 
	tibble::enframe(cult_yield_rename, name = "yield_from", value = "crop") %>%	
	unnest(crop)

diet_dairy <- diet_dairy %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	left_join(feed_data, by = c("yield_from" = "feed")) %>%
	rename(
		feed = yield_from,
		kg_dm_per_animal_per_year = feed_consumption_kg
	)	

diets_beef <- diets_beef %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	left_join(feed_data, by = c("yield_from" = "feed")) %>%
	rename(
		feed = yield_from,
		kg_dm_per_animal_per_year = feed_consumption_kg
	)	

diet_sheep <- diet_sheep %>%
	left_join(cult_yield_rename_lookup, by = "crop") %>%
	left_join(feed_data, by = c("yield_from" = "feed")) %>%
	rename(
		feed = yield_from,
		kg_dm_per_animal_per_year = feed_consumption_kg
	)	

# Enteric fermentation:
	# CH4 per animal per year in CO2eqvs

EF_dairy <- diet_dairy %>%
	drop_na(feed)  %>%
	mutate(
		GE_per_animal_per_year = GE_MJ_per_kg_dm * kg_dm_per_animal_per_year,
		EF_CH4_per_animal_per_year = (GE_per_animal_per_year * (Ym_dairy/100)) / 55.65,
		EF_CO2eqv_per_animal_per_year = 
			EF_CH4_per_animal_per_year * GHG_factors[which(GHG_factors$factor_type == "GWP methane"), "factor_value"]
	)

EF_beef <- diets_beef %>%
	drop_na(feed)  %>%
	mutate(
		GE_per_animal_per_year = GE_MJ_per_kg_dm * kg_dm_per_animal_per_year,
		EF_CH4_per_animal_per_year = (GE_per_animal_per_year * (Ym_beef/100)) / 55.65,
		EF_CO2eqv_per_animal_per_year = 
			EF_CH4_per_animal_per_year * GHG_factors[which(GHG_factors$factor_type == "GWP methane"), "factor_value"]
	)

EF_sheep <- diet_sheep %>%
	drop_na(feed)  %>%
	mutate(
		GE_per_animal_per_year = GE_MJ_per_kg_dm * kg_dm_per_animal_per_year,
		EF_CH4_per_animal_per_year = (GE_per_animal_per_year * (Ym_sheep/100)) / 55.65,
		EF_CO2eqv_per_animal_per_year = 
			EF_CH4_per_animal_per_year * GHG_factors[which(GHG_factors$factor_type == "GWP methane"), "factor_value"]
	)

saveRDS(EF_dairy, file.path(filePath2, "EF_dairy.rds"))
saveRDS(EF_beef, file.path(filePath2, "EF_beef.rds"))
saveRDS(EF_sheep, file.path(filePath2, "EF_sheep.rds"))

# N2O emissions (CO2eq) from manure management

# Direct N2O emissions (CO2eq) from manure management:  
# Nitrogen input in manure (with animal share included *already*)
# multiplied by the animal specific EF3 factor GHG_factors:EF3 factor
# &
# Indirect N2O emissions (CO2eq) from manure management:  
# Nitrogen input in manure N input in manure multiplied 
# by the animal specific volatilization factor GHG_factors:volatilisation factor 
# and by the EF4 factor GHG_factors:EF4 factor and multiplied by 44/28. 
# 
# Multiply by the GWP factor GHG_factors:GWP N2O

# CO2eq emissions from manure management =  
# 	CH4 emissions (CO2eq) from manure management + 
# 	Direct N2O emissions (CO2eq) from manure management + 
# 	Indirect N2O emissions (CO2eq) from manure management

P_N_inputs_manure <- readRDS(file.path(filePath2, "P_N_inputs_manure.rds"))
P_N_inputs_manure_beef_shares <- readRDS(file.path(filePath2, "P_N_inputs_manure_beef_shares.rds"))
P_N_inputs_manure_beef_shares$sector <- "beef"

CO2eqv_emissions_fromN2O <- NA

for (ani in all_animals) {
	
	EF3 <- GHG_factors %>%
		filter(animal %in% c(ani, "all")) %>%
		filter(factor_type == "EF3 factor") %>%
		pull(factor_value)
	
	GWP_N2O <- GHG_factors %>%
		filter(animal %in% c(ani, "all")) %>%
		filter(factor_type == "GWP N2O") %>%
		pull(factor_value)
	
	VOL <- GHG_factors %>%
		filter(animal %in% c(ani, "all")) %>%
		filter(factor_type == "Volatilisation factor") %>%
		pull(factor_value)
	
	EF4 <- GHG_factors %>%
		filter(animal %in% c(ani, "all")) %>%
		filter(factor_type == "EF4 factor") %>%
		pull(factor_value)
	 
	if (ani == "beef") {

		CO2eqv_emissions_fromN2O_beef <- P_N_inputs_manure_beef_shares %>%
			mutate(
				direct_emissions = 
					N_input_manure_kg_share * 
					EF3 *
					GWP_N2O,
				indirect_emissions = 
					N_input_manure_kg_share *
					VOL *			 
					EF4 * (44/28) *
					GWP_N2O,
				total_emissions = 
					direct_emissions + 
					indirect_emissions
			)
				
	} else {

	CO2eqv_emissions_fromN2O <- P_N_inputs_manure %>%
		filter(sector == ani) %>%
		mutate(
			direct_emissions = 
				N_input_manure * 
				EF3 *
				GWP_N2O,
			indirect_emissions = 
				N_input_manure *
				VOL *			 
				EF4 * (44/28) *
				GWP_N2O,
			total_emissions = 
				direct_emissions + 
				indirect_emissions
		)  %>%
		rbind(CO2eqv_emissions_fromN2O)
	}
}
saveRDS(CO2eqv_emissions_fromN2O_beef, file.path(filePath2, "CO2eqv_emissions_fromN2O_beef.rds"))
CO2eqv_emissions_fromN2O <- CO2eqv_emissions_fromN2O[-nrow(CO2eqv_emissions_fromN2O),]
saveRDS(CO2eqv_emissions_fromN2O, file.path(filePath2, "CO2eqv_emissions_fromN2O.rds"))

# N2O emissions (CO2eq) from nitrogen application: 
# N input in fertilizer (N (kg/ha)) 
# multiplied by N2O application factor GHG_factors:EF4 factor

ferti <- readRDS(file.path(filePath2, "LU_ferti_all_sectors.rds"))

EF4 <- GHG_factors %>%
	filter(factor_type == "EF4 factor") %>%
	pull(factor_value)

GWP <- GHG_factors %>%
		filter(factor_type == "GWP N2O") %>%
		pull(factor_value)

ferti_N_emissions <- ferti %>%
	group_by(sector, region) %>%
	mutate(
		CO2eqv_emissions_from_ferti = N_input_in_fert_region * EF4 * GWP,
		CO2eqv_emissions_from_ferti_per_ha = N_input_in_fert_region_per_ha * EF4 * GWP
	)

saveRDS(ferti_N_emissions, file.path(filePath2, "CO2eqv_ferti_N_emissions.rds"))
