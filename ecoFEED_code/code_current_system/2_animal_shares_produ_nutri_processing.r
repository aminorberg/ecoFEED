rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Standardize herd structure and animal production identifiers, 
# and save prepared RDS-files for later workflow stages
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Herd structure (animal shares) Animal production

animal_shares <- read_xlsx(file.path(filePath1, "Animal_shares.xlsx"))

animal_shares <- animal_shares %>%
	mutate(animal = fct_recode(Animal,
    	dairy_cow = "Dairy_cow",
    	dairy_cull_cow = "Dairy_cull_cow",
    	dairy_surplus_heifer = "Dairy_surplus_heifers",
    	heifers_from_dairy = "Beef_dairy_finishing_heifers",
    	dairy_replacement_heifers = "Dairy_replacement_heifers",
    	bulls_from_dairy = "Beef_dairy_finishing_bulls",
    	beef_suckler_cows = "Beef_suckler_cows",
    	beef_cull_suckler_cows = "Beef_beef_cull_suckler_cows",
    	beef_heifers = "Beef_beef_finishing_heifers",
    	beef_bulls = "Beef_beef_finishing_bulls",
    	beef_replacement_heifers = "Beef_beef_replacement_heifers",
    	beef_breeding_bulls = "Beef_beef_breeding_bulls",
    	pork = "Pork",
    	eggs = "Eggs",
    	poultry = "Broilers",
    	sheep = "Sheep_ewe_incl_replacement_ewes",
    	sheep_lamb = "Sheep_lamb"
	))

animal_production <- read_xlsx(file.path(filePath1, "Animal_production.xlsx"))

animal_production <- animal_production %>%
	select(where(~ !all(is.na(.))))
  
animal_production <- animal_production %>%
	mutate(animal = fct_recode(Animal,
    	dairy_cow = "Dairy_cow",
    	dairy_cull_cow = "Dairy_cull_cow",
    	dairy_surplus_heifer = "Dairy_surplus_heifers",
    	heifers_from_dairy = "Beef_dairy_finishing_heifers",
    	dairy_replacement_heifers = "Dairy_replacement_heifers",
    	bulls_from_dairy = "Beef_dairy_finishing_bulls",
    	beef_suckler_cows = "Beef_suckler_cows",
    	beef_cull_suckler_cows = "Beef_beef_cull_suckler_cows",
    	beef_heifers = "Beef_beef_finishing_heifers",
    	beef_bulls = "Beef_beef_finishing_bulls",
    	beef_replacement_heifers = "Beef_beef_replacement_heifers",
    	beef_breeding_bulls = "Beef_beef_breeding_bulls",
    	pork = "Pork",
    	eggs = "Eggs",
    	poultry = "Broilers",
    	sheep = "Sheep_ewe_incl_replacement_lambs",
    	sheep_lamb = "Sheep_lamb"
	))

animal_production <- animal_production %>%
	mutate(sector = fct_recode(animal,
    	dairy = "dairy_cow",
    	dairy = "dairy_cull_cow",
    	dairy = "dairy_replacement_heifers",
    	dairy = "dairy_surplus_heifer",
    	beef = "heifers_from_dairy",
    	beef = "bulls_from_dairy",
    	beef = "beef_replacement_heifers",
    	beef = "beef_breeding_bulls",
    	beef = "beef_suckler_cows",
    	beef = "beef_cull_suckler_cows",
    	beef = "beef_heifers",
    	beef = "beef_bulls",
    	pork = "pork",
    	eggs = "eggs",
    	poultry = "poultry",
    	sheep = "sheep",
    	sheep = "sheep_lamb"
  	)) %>%
  	select(-Animal)

animal_production <- animal_production %>%
	mutate(Product = fct_collapse(animal,
    		milk = "dairy_cow",
			"NA" = c("dairy_replacement_heifers",
					 "beef_suckler_cows",
					 "beef_replacement_heifers",
					 "beef_breeding_bulls"),
			beef_meat = c("dairy_cull_cow", # dairy sektori
						  "dairy_surplus_heifer", # dairy sektori
						  "heifers_from_dairy", # beef sektori
						  "bulls_from_dairy", # beef sektori
						  "beef_cull_suckler_cows", # beef sektori
						  "beef_heifers", # beef sektori
						  "beef_bulls"), # beef sektori      		
        	pork_meat = "pork",
      		eggs = "eggs",
      		poultry_meat = "poultry",
			sheep_meat = c("sheep",
						   "sheep_lamb")
  	))

animal_production <- animal_production %>%
	rename(
    	product = Product,
    	production_kg_per_animal_per_year = Production
    )

animal_production <- animal_production %>%
	left_join(animal_shares, by = "animal") %>%
	rename(region = "Region") %>%
	select(-Animal)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveRDS(animal_production, file = file.path(filePath2, "animal_production.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
