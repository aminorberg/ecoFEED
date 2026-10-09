rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Food production
# the quantity of protein and calories (in ASF) produced per animal
# calculated by multiplying the sector sum production levels
# with the no. of animals, for each sector

# note that with dairy, only the no. of dairy cows is used,
# and with beef, only the no. of suckler cows is used
# (others are already factored into the sector sum)

# per ha values are calculated using type (2) land-area, 
# i.e. productive land, excl. fallows

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

food_prod_per_animal_per_year <- 
	readRDS(file = file.path(filePath2, "food_prod_per_animal_per_year.rds"))

food_prod_per_herd_per_year <- food_prod_per_animal_per_year %>%
	group_by(region, sector) %>%
	summarise(
		dm_prod_kg_per_herd = sum(dm_prod_kg_per_herd_animal, na.rm = TRUE)
	)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dairy_animal_no <- read_xlsx(file.path(filePath1, "01lypsy/animals01.xlsx"))
colnames(dairy_animal_no) <- c("region", "no_of_animals")
dairy_animal_no$sector <- "dairy"

suckler_cow_nos <- read_xlsx(file.path(filePath1, "02lihanauta/animals02.2.1.xlsx"))
colnames(suckler_cow_nos)  <- c("region", "no_of_animals")
suckler_cow_nos$sector <- "beef"

pork_animal_no <- read_xlsx(file.path(filePath1, "05lihasika/animals05.xlsx"))
colnames(pork_animal_no) <- c("region", "no_of_animals")
pork_animal_no$sector <- "pork"

poultry_animal_no <- read_xlsx(file.path(filePath1, "08siipikarjanliha/animals08.xlsx"))
colnames(poultry_animal_no) <- c("region", "no_of_animals")
poultry_animal_no$sector <- "poultry"

sheep_animal_no <- read_xlsx(file.path(filePath1, "10lammas/animals10.xlsx"))
colnames(sheep_animal_no) <- c("region", "no_of_animals")
sheep_animal_no$sector <- "sheep"

eggs_animal_no <- read_xlsx(file.path(filePath1, "07kananmuna/animals07.xlsx"))
colnames(eggs_animal_no) <- c("region", "no_of_animals")
eggs_animal_no$sector <- "eggs"

animal_nos <- bind_rows(dairy_animal_no,
						suckler_cow_nos,
						pork_animal_no, 
						poultry_animal_no, 
						sheep_animal_no, 
						eggs_animal_no)

animal_production <- readRDS(file = file.path(filePath2, "animal_production.rds"))

animal_production_mod <- animal_production %>%
	mutate(
    	sector = case_when(
      		animal == "heifers_from_dairy" ~ "dairy",
      		animal == "bulls_from_dairy" ~ "dairy",
      	TRUE ~ as.character(sector)
    	)
  	)

animal_nos_all <- animal_nos %>% 
	left_join(animal_production_mod[, c("region", "sector", "animal", "Share")], by = c("region", "sector")) %>%
	mutate(
		no_of_real_animals = no_of_animals * Share
	) %>% 
	mutate(
    	sector = case_when(
      		animal == "heifers_from_dairy" ~ "beef",
      		animal == "bulls_from_dairy" ~ "beef",
      	TRUE ~ as.character(sector)
    	)
  	)
saveRDS(animal_nos_all, file = file.path(filePath2, "animal_nos_all.rds"))

animal_nos_real_ani <- animal_nos_all %>%
	filter(animal %in% c("dairy_cow", 
						 "heifers_from_dairy", 
						 "bulls_from_dairy", 
						 "beef_suckler_cows", 
						 "pork", 
						 "poultry", 
						 "sheep", 
						 "eggs"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
food_prod <- animal_nos_real_ani %>%
	left_join(food_prod_per_animal_per_year, by = c("region", "sector", "animal")) %>%
	mutate(
		food_prod_kg_dm_per_region = 
			dm_prod_kg_per_animal * no_of_real_animals
	) 

saveRDS(food_prod, file = file.path(filePath2, "food_prod_all_sectors.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cult_LU_by_sect <- readRDS(file = file.path(filePath2, "cult_LU_by_sect.rds"))
cult_LU_by_sect_sums <- cult_LU_by_sect %>%
	group_by(sector, region) %>%
	summarise(
		cult_area = sum(area)
	)

food_prod <- food_prod %>%
	group_by(sector, region) %>%
	summarise(
		no_of_real_animals = sum(no_of_real_animals),
		food_prod_kg_dm_per_region = sum(food_prod_kg_dm_per_region)
	)

food_production <- food_prod %>%
	left_join(cult_LU_by_sect_sums, by = c("sector", "region")) %>%
	mutate(
		food_prod_kg_dm_per_ha = food_prod_kg_dm_per_region / cult_area
	)

saveRDS(food_production, file = file.path(filePath2, "food_production.rds"))
write.csv(food_production, file = file.path(filePath2, "food_production.csv"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

