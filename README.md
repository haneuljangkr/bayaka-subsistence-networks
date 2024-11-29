Data, code and manuscript for "Women’s subsistence networks scaffold cultural transmission among BaYaka foragers in the Congo Basin"
----------------------------

## The manuscript can be found at:

- Science Advances: https://www.science.org/doi/10.1126/sciadv.adj2543

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

## Data file

The strand object data (PublicVersion.RData) contains all data used to run models, including dayID, focalID, ind1, ind2, observed together (1: observed being together in a focal woman's foraging group at the day of day ID, 0: not observed in a focal woman's foraging group), camp\_together (1: being at camp together at the day of dayID (cohabitants), 0: not being at camp together), gender difference (female-female, different gender, male-male), co-residence (0: not living together in the same household or 1: co-residing in the same household) and relatedness of each dyad.

The project is maintained by Haneul Jang (haneul_jang@eva.mpg.de), Daniel Redhead (daniel_redhead@eva.mpg.de), and Cody Ross (cody_ross@eva.mpg.de) and is hosted at https://github.com/haneuljangkr
