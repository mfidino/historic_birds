# historical_birds_analysis

1. To run the alpha diversity analysis, open the R script: `species_richness_analysis.R`

2. To run the beta diversity analysis, open the R script: `beta_diversity_analysis.R`

3. To run the multi-species GLMM, open the R script: `species_glmm_analysis.R`


## The data sub-folder

1. `arrival.csv`: This contains the arrival dates of birds in julian day per year across all survey years. While not used in the analysis, we have provided it.

2. `BirdFuncDat.csv`: The bird functional data used to calculate a species diet and foraging breadth from Wilman et al. 2014.

3. `days_seen_per_year.csv`: This is the number of days species were seen per survey year.

4. `effort.csv`: This is the number of days surveys were conducted per survey year. When combined with `days_seen_per_year.csv`, you can determine the proportion of days a species was observed.

5. `raw_ward_data.csv`: We pulled the statewide occupancy trends from an image of a table in Ward et al. (2018) and it needed some correcting. This is the raw output from pulling text from the image of that table.

6. `species_codes.csv`: Four letter species codes for birds.

7. `species_life_history.csv`: A number of species life history information (e.g., whether a species is migratory in Chicago) collected programmatically from www.allaboutbirds.org.

8. `species_seen_per_day.csv`: The Walter's considered May 7 to May 20 the height of migration. We tabulated species richness per day during this time across all survey years. Can be plotted out with R script `height_migration_exploration.R`.

9. `temperatures.csv`: Temperature data per survey year collected from NOAA.

10.  `ward_changes.csv`: This contains the statewide occupancy trends for Illinois Breeding birds estimated by Ward et al. (2018) that have since been cleaned after `raw_ward_data.csv`. The cleaning script is `pull_ward_data.csv`.




