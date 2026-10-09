How to run the ecoFEED analytical pipeline:

The main folder ecoFEED contains this readme, code folders, and data folders. Code folders contain the scripts used for the analysis. Data folders contain the data used and produced by the scripts.

After downloading the 'ecoFEED'-folder, start by configuring the script '0_paths_pkgs.r', located in the 'ecoFEED_code'-folder: change the working directory so that It corresponds to yours, to ensure the functioning of the following scripts. Install all the packages listed, so that they can be accessed later.

The actual analytical scripts (inside the folders with the prefix 'code_') are numbered according to the order in which they should be run. Run the scripts in numerical order, starting from the first script ('1_') in the code_current_system-folder. After running these scripts, move to the code_indicators-folder, and run those scripts in order as well. At the end of the final script, all the results are saved as both .rds and .csv.

All the raw data are in the 'ecoFEED_data'-folder, and all the processed data, based on the raw data, is saved into the 'ecoFEED_mod_data'-folder.
