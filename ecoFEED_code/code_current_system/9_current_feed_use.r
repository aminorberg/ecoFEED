rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# feed use: feed consumption in on-farm and industrial feeds =
# grass, cereal, annual legumes and oilseed crop consumption by animals,
# multiplied by the number of the animals

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# DAIRY
dairy_animal_no <- read_xlsx(file.path(filePath1, "01lypsy/animals01.xlsx"))
colnames(dairy_animal_no) <- c("region", "no_of_animals")
saveRDS(dairy_animal_no, file = file.path(filePath2, "dairy_animal_no.rds"))

dairy_diets <- read_xlsx(file.path(filePath1, "01lypsy/diets01.xlsx"))
dairy_diet <- dairy_diets %>%
	slice(1:9) %>%
	select(
    	crop = "Crop",
    	feed_consumption_kg = "Koko maa"
	) %>%
	filter(crop != "Industrial_total") %>%
	filter(crop != "Industrial_other")
saveRDS(dairy_diet, file = file.path(filePath2, "dairy_diet.rds"))

dairy_feed_use <- merge(dairy_diet, dairy_animal_no)
dairy_feed_use <-  dairy_feed_use %>%
	mutate(
		total_feed_cons_kg = feed_consumption_kg * no_of_animals
	)
saveRDS(dairy_feed_use, file = file.path(filePath2, "dairy_feed_use.rds"))

# BEEF
file_nos <- c("1.1", "1.2", "2.1", "2.2", "2.3", "3.1", "3.2")
beef_subsectors <- c("heifers_from_dairy", 
					 "bulls_from_dairy", 
					 "beef_suckler_cows", 
					 "beef_breeding_bulls", 
					 "beef_replacement_heifers", 
					 "beef_heifers", 
					 "beef_bulls")
beef_feed_use <- tibble(
	beef_subsect = character(),
	crop = character(),
	feed_consumption_kg = numeric(),
	region = character(),
	no_of_animals = numeric()
)

for (i in 1:length(file_nos)) {
	beef_animal_no <- NA
	beef_diet <- NA
	feed_use <- NA
	beef_animal_no <- read_xlsx(paste0(filePath1,
										"/02lihanauta/animals02.",
										file_nos[i], 
										".xlsx"))[, 1:2]	
	beef_diet <- read_xlsx(paste0(filePath1,
							"/02lihanauta/diets02.",
						   file_nos[i],
						   ".xlsx"))[, 1:2]
	colnames(beef_animal_no)  <- c("region", "no_of_animals")
	colnames(beef_diet) <- c("crop", "feed_consumption_kg")
	feed_use <- merge(beef_diet, beef_animal_no)
	feed_use$beef_subsect <- beef_subsectors[i]
	feed_use <-  feed_use %>%
		mutate(
			total_feed_cons_kg = feed_consumption_kg * no_of_animals
		)
	beef_feed_use <- bind_rows(beef_feed_use, feed_use)
}
saveRDS(beef_animal_no, file = file.path(filePath2, "beef_animal_no.rds"))

beef_feed_use <- beef_feed_use %>%
	filter(crop != "Industrial_total") %>%
	filter(crop != "Industrial_other")
saveRDS(beef_feed_use, file = file.path(filePath2, "beef_feed_use.rds"))

beef_diets <- beef_feed_use %>%
	select(beef_subsect, crop, feed_consumption_kg, region) %>%
	filter(region == "Koko maa") %>%
	select(-region)
saveRDS(beef_diets, file = file.path(filePath2, "beef_diets.rds"))

# PORK
pork_animal_no <- read_xlsx(file.path(filePath1, "05lihasika/animals05.xlsx"))
colnames(pork_animal_no) <- c("region", "no_of_animals")
saveRDS(pork_animal_no, file = file.path(filePath4, "pork_animal_no.rds"))

pork_diet <- read_xlsx(file.path(filePath1, "05lihasika/diets05.xlsx"))
colnames(pork_diet) <- c("crop", "feed_consumption_kg")
pork_diet <- pork_diet %>% 
	filter(crop != "Industrial_total") %>%
	filter(crop != "Industrial_other")
saveRDS(pork_diet, file = file.path(filePath2, "pork_diet.rds"))

pork_feed_use <- merge(pork_diet, pork_animal_no)
pork_feed_use <-  pork_feed_use %>%
	mutate(
		total_feed_cons_kg = feed_consumption_kg * no_of_animals
	)
saveRDS(pork_feed_use, file = file.path(filePath2, "pork_feed_use.rds"))

# POULTRY
poultry_animal_no <- read_xlsx(file.path(filePath1, "08siipikarjanliha/animals08.xlsx"))
colnames(poultry_animal_no) <- c("region", "no_of_animals")
saveRDS(poultry_animal_no, file = file.path(filePath2, "poultry_animal_no.rds"))

poultry_diet <- read_xlsx(file.path(filePath1, "08siipikarjanliha/diets08.xlsx"))
colnames(poultry_diet) <- c("crop", "feed_consumption_kg")
poultry_diet <- poultry_diet %>% 
	filter(crop != "Industrial_total") %>%
	filter(crop != "Industrial_other")
saveRDS(poultry_diet, file = file.path(filePath2, "poultry_diet.rds"))

poultry_feed_use <- merge(poultry_diet, poultry_animal_no)
poultry_feed_use <-  poultry_feed_use %>%
	mutate(
		total_feed_cons_kg = feed_consumption_kg * no_of_animals
	)
saveRDS(poultry_feed_use, file = file.path(filePath2, "poultry_feed_use.rds"))

# SHEEP
sheep_animal_no <- read_xlsx(file.path(filePath1, "10lammas/animals10.xlsx"))
sheep_diet <- read_xlsx(file.path(filePath1, "10lammas/diets10.xlsx"))
colnames(sheep_animal_no) <- c("region", "no_of_animals")
saveRDS(sheep_animal_no, file = file.path(filePath2, "sheep_animal_no.rds"))

colnames(sheep_diet) <- c("crop", "feed_consumption_kg")
sheep_diet <- sheep_diet %>% 
	filter(crop != "Industrial_total") %>%
	filter(crop != "Industrial_other")
saveRDS(sheep_diet, file = file.path(filePath2, "sheep_diet.rds"))

sheep_feed_use <- merge(sheep_diet, sheep_animal_no)
sheep_feed_use <-  sheep_feed_use %>%
	mutate(
		total_feed_cons_kg = feed_consumption_kg * no_of_animals
	)
saveRDS(sheep_feed_use, file = file.path(filePath2, "sheep_feed_use.rds"))

# EGGS
eggs_animal_no <- read_xlsx(file.path(filePath1, "07kananmuna/animals07.xlsx"))
eggs_diet <- read_xlsx(file.path(filePath1, "07kananmuna/diets07.xlsx"))
colnames(eggs_animal_no) <- c("region", "no_of_animals")
saveRDS(eggs_animal_no, file = file.path(filePath2, "eggs_animal_no.rds"))

colnames(eggs_diet) <- c("crop", "feed_consumption_kg")
eggs_diet <- eggs_diet %>% 
	filter(crop != "Industrial_total") %>%
	filter(crop != "Industrial_other")
saveRDS(eggs_diet, file = file.path(filePath2, "eggs_diet.rds"))

eggs_feed_use <- merge(eggs_diet, eggs_animal_no)
eggs_feed_use <-  eggs_feed_use %>%
	mutate(
		total_feed_cons_kg = feed_consumption_kg * no_of_animals
	)
saveRDS(eggs_feed_use, file = file.path(filePath2, "eggs_feed_use.rds"))
