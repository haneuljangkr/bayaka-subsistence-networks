Data, code and manuscript for "Women’s subsistence networks scaffold cultural transmission among BaYaka foragers in the Congo Basin"
----------------------------

## The manuscript can be found at:

- Science Advances: https://www.sciencedirect.com/science/article/abs/pii/S1090513822000484

## Requirements for analyses:

* R: [https://cran.r-project.org](https://cran.r-project.org/)
* STRAND: [https://github.com/ctross/STRAND](https://github.com/ctross/STRAND)
* cmdstanr: [https://mc-stan.org/cmdstanr/](https://mc-stan.org/cmdstanr/)

## Packages used for data processing and visualisation:

* Rethinking: [https://xcelab.net/rm/statistical-rethinking/](https://xcelab.net/rm/statistical-rethinking/)
* tidyverse: [https://www.tidyverse.org](https://www.tidyverse.org/)
* iGraph: https://igraph.org/r/
* Rcolorbrewer: https://cran.r-project.org/web/packages/RColorBrewer/index.html

## Details

To reproduce the results presented in the manuscript, please first go into the 'code/' folder of the repository. It would probably be best to review the many different scripts for data processing, analysis and visualisation. Then, if you would like to reproduce the results reported in the publication, call the run all file that can be found in the repository:

```
source("./run_all.R")
```

Alternatively, you can go into each processing/analysis/visualisation script and run them separately.

## [Data file](https://github.com/haneuljangkr/bayaka-subsistence-networks#details)

The strand object data (PublicVersion.RData) contains all data used to run models, including dayID, focalID, ind1, ind2, observed together (1: observed being together in a focal woman's foraging group at the day of day ID, 0: not observed in a focal woman's foraging group), camp\_together (1: being at camp together at the day of dayID (cohabitants), 0: not being at camp together), gender difference (female-female, different gender, male-male), co-residence (0: not living together in the same household or 1: co-residing in the same household) and relatedness of each dyad.

The project is maintained by Haneul Jang (haneul_jang@eva.mpg.de), Daniel Redhead (daniel_redhead@eva.mpg.de), and Cody Ross (cody_ross@eva.mpg.de) and is hosted at https://github.com/haneuljangkr

# Data Description 

- **observational_network.txt**: This dataset has 3 columns. The first column, "v1", is the observation ID. Observations here mean a focal follow of 5 focal individual. In total we have 44251 unique observations. The other two columns "ind1" and "ind2" are structured as an edgelist and therefore indicate the presence of ties between the two individuals. Note that these individuals are not always the focal, they may also be individuals who have been observed together during the focal follow. 

- **weighted_network.txt**: The same data as "observational_network.txt", however it has been restructured to be a weighted network. The weights given to each dyad are the frequency of them being observed together during the focal follows. This dataset has 3 columns: ind1, ind2 and weight.

- **people.txt**: This datset contains all individual-level information about the sample. 

- **camp_composition.txt**: These data have 5 columns, the first is the ID for each row, the second is the year, the third is the date, the fourth id the individual ID, and the fifth and final column is a binary indicator of an individual being present in the camp (0=not present). The data are long format, and capture information for every individual at every time point. 

- **observational_network_camp.txt**: This dataset has 4 columns: V1, which refers to date, ind1, ind2 and dayID. The columns ind1 and ind2 indicate the presence of ties between the two individuals. Note that these individuals are not always the focal, they may also be individuals who have been observed together during the focal follow. 

- **weighted_network_camp.txt**: Based on data "camp_composition.txt" and "observational_network_camp.txt", it has been restructured to be a weighted network. The weights given to each dyad are the frequency of them being observed together at camp during the entire observational period of 230 days. This dataset has 3 columns: ind1, ind2 and weight.

- **dyadic_covariates.txt**: This data is an edgelist that caputres relatedness for every possible dyad in the camp. This dataset has 4 columns: ind1, ind2, relation and r.

- **daily_observed_dyads.txt**: This dataset has 3 columns: V1, which refers to dayID in other data sets, ind1 and ind2. In total we have 230 unique observation days. The two columns "ind1" and "ind2" are structured as an edgelist and therefore indicate the presence of ties between the two individuals. Note that these individuals are not always the focal, they may also be individuals who have been observed together during the focal follow.

- **daily_possible_dyads.txt**: This dataset has 4 columns: dayid, focalid, ind1 and ind2. all possible comninatino of dyads among 92 individuals = 92 x 91 /2 = 4186 dyads --> 4186 dyads x 230 days = 962780 rows.

- **data_for_daily_matrices.txt**: This dataset has 6 columns: dayid, focalid, ind1, ind2, observed together, camp_together. 


# Adjacency matrices

In **adjacency_matrix** folder, there are five adjacency matrices (upper triangle) created for statistical analyses.
- **weight_matrix.txt**: an adjacency matrix with the frequency of each dyad being observed together during the focal follows.  
- **camp_composition_matrix.txt**: an adjacency matrix with the frequency of each dyad being observed together at camp. 
- **sex_diff_matrix.txt**: an adjacency matrix with 0 (same sex) or 1 (opposite sex) for each dyad. 
- **age_diff_matrix.txt**: an adjacency matrix with 0 (same age class) or 1 (different age classes) for each dyad. 
- **age_dist_matrix.txt**: an adjacency matrix with distance between age classes for each dyad. 
- **relatedness_matrix.txt**: an adjacency matrix with relatedness of each dyad. 


# Daily matrices
Based on the data "data_for_daily_matrices.txt", daily adjacency matirces were created. 

In **camp** folder, there are 230 adjacency matrices (upper triangle) with file names of "focal woman_dayID". Each cell in a matrix has filled with either 1 or 0. 1: being at camp together at the day of dayID (cohabitants), 0: not being at camp together

In **foraging** folder, there are 230 adjacency matrices (upper triangle) with file names of "focal woman_dayID". Each cell in a matrix has filled with either 1 or 0. 1: observed being together in a focal woman's foraging group at the day of day ID, 0: not observed in a focal woman's foraging group 
