rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Biological N fixation: for clover silage, faba beans and peas 
# ((BNF:alphaCult *  N (kg/ha) output in harvest /  BNF:NHI) + BNF:betaCult) * BNF:BGN. 

BNF <- read_xlsx(file.path(filePath3, "BNF.xlsx"))
colnames(BNF) <- c("BNF", "clover_grass", "fababean", "peas")

BNF <- BNF %>%
	pivot_longer(
		cols = -BNF,
		names_to = "feed",
		values_to = "value"
		)  %>%
	pivot_wider(
		names_from = BNF,
	values_from = value
		)

N_P_in_harvest <- readRDS(file.path(filePath2, "N_P_in_harvest.rds"))

BNF_sectors <- N_P_in_harvest %>%
	filter(feed %in% BNF$feed)  %>%
	left_join(BNF, by = "feed") %>%
	mutate(
		N_fixed = (alphaCult * N_in_harvest)/(NHI+betaCult) / BGN,
		N_fixed_per_ha = (alphaCult * N_in_harvest_per_ha)/(NHI+betaCult) / BGN
	) %>%
	group_by(sector, region) %>%
  	summarise(
  		BNF_per_region = sum(N_fixed, na.rm = TRUE),
  		BNF_per_ha = sum(N_fixed_per_ha, na.rm = TRUE)
  	)

saveRDS(BNF_sectors, file.path(filePath2, "BNF_sectors.rds"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++