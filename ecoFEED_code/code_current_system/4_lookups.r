rm(list = ls(all = TRUE)) ; gc()

# specify here the path to the pipeline location
ecofeed_path <- "/Users/annanorb/Documents/RURALIA/sustAnimal"

# run the script with subpaths and packages specified
source(file.path(ecofeed_path, "ecoFEED/ecoFEED_code/0_paths_pkgs.r"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Creating lookups, standardising identifiers
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# translating, renaming and indexing, to connect the data sets to each other

# translating the sectors FI-EN
sectors <- list(
  plant_production = c("Viljanviljely", "Juurikasvien viljely", "Muu kasvinviljely"),
  beef = c("Emolehmätuotanto", 
  		   "Lihanautojen kasvatus", 
  		   "Yhdistetty emolehmätuotanto ja lihanautojen kasvatus"),
  dairy = c("Lypsykarjatalous", "Yhdistetty lypsykarja ja naudanlihan tuotanto"),
  pork = c("Lihasikojen kasvatus", "Porsastuotanto", "Yhdistelmätuotanto"),
  eggs = c("Kananmunien tuotanto", "Yhdistettu kananmunien ja siipikarjanlihan tuotanto"),
  poultry = c("Siipikarjanlihan tuotanto"),
  sheep = c("Lammastalous"),
  mixed = c("Sekamuotoinen tuotanto")
)
saveRDS(sectors, file = file.path(filePath2, "sectors.rds"))

# grouping for the different cultivated plants
cult_groups <- list(
	cereals = c("barley", 
				"wheat", 
				"rye", 
				"BSG", 
				"barley_feed", 
				"barley_malt", 
				"oats", 
				"cereal_mix", 
				"other_cereals"),
	silage = c("silage", "pasture", "permanent_grass", "clover_grass"),
	fallow = "fallow",
	potato = c("potato", "potatoes_food"),
	legumes = c("peas", "fababean"),
	oil = c("rapeseed_oleifera", "rapeseed_napus", "flaxseed", "rapeseed_rapa_oleifera")
)
saveRDS(cult_groups, file = file.path(filePath2, "cult_groups.rds"))

# connecting feed identifiers to the plant yield data identifiers
# (barley is used for unknown/mixed cereals)
# (silage is used also for pasture, permanent grass and clover grass)
yield_groups <- list(
	wheat = "wheat",
	rye = "rye",
	BSG = "BSG",
	barley = c("barley_feed", "barley_malt", "other_cereals"),
	oats = "oats",
	cereal_mix = "cereal_mix",
	silage = c("silage", "pasture", "permanent_grass", "clover_grass"),
	potatoes_food = "potato",
	peas = "peas",
	fababean = "fababean",
	rapeseed_rapa_oleifera = "rapeseed_oleifera",
	rapeseed_napus = "rapeseed_napus",
	flaxseed = "flaxseed",
	fallow = "fallow",
	industrial_byproducts = "industrial_byproducts"
)
saveRDS(yield_groups, file = file.path(filePath2, "yield_groups.rds"))

# connecting current animal diet components to the yield data identifiers
cult_yield_rename <- list(
	silage = "Grass",
	barley = c("Cereals", "Industrial_cereals"),
	peas = c("Industrial_legumes", "Legumes"),
  	industrial_byproducts = "Industrial_byproducts",
	rapeseed_rapa_oleifera = "Industrial_oils"
)
saveRDS(cult_yield_rename, file = file.path(filePath2, "cult_yield_rename.rds"))

# grouping the components of the animal diets to the cultivated plant group identifiers
cult_diet_groups <- list(
  cereals = c("Cereals", "Industrial_cereals"),
  silage = "Grass",
  legumes = c("Legumes", "Industrial_legumes"),
  industrial_byproducts = "Industrial_byproducts",
  oil = "Industrial_oils"
)
saveRDS(cult_diet_groups, file = file.path(filePath2, "cult_diet_groups.rds"))

# renaming and grouping the ELY-centre identifiers, for consistency 
ely_regions <- list(
	"Koko maa" = "Koko maa",
	"Uudenmaan ELY-keskus" = "Uusimaa",
	"Varsinais-Suomen ELY-keskus" = "Varsinais-Suomi",
	"Satakunnan ELY-keskus" = "Satakunta",
	"Hämeen ELY-keskus" = c("Kanta-Häme", "Päijät-Häme"), 
	"Pirkanmaan ELY-keskus" = "Pirkanmaa",
	"Kaakkois-Suomen ELY-keskus" = c("Kymenlaakso", "Etelä-Karjala"),
	"Etelä-Savon ELY-keskus" = "Etelä-Savo",
	"Pohjois-Savon ELY-keskus" = "Pohjois-Savo",
	"Pohjois-Karjalan ELY-keskus" = "Pohjois-Karjala",
	"Keski-Suomen ELY-keskus" = "Keski-Suomi",
	"Etelä-Pohjanmaan ELY-keskus" = "Etelä-Pohjanmaa",
	"Pohjanmaan ELY-keskus" = c("Pohjanmaa", "Keski-Pohjanmaa"),
	"Pohjois-Pohjanmaan ELY-keskus" = "Pohjois-Pohjanmaa",
	"Kainuun ELY-keskus" = "Kainuu",
	"Lapin ELY-keskus" = "Lappi",
	"Ahvenanmaan valtionvirasto" = "Ahvenanmaa")
saveRDS(ely_regions, file = file.path(filePath2, "ely_regions.rds"))
