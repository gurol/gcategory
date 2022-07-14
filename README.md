## A Research Compedium of
# G-Category: A novel method to quantifying and categorizing data sets

[![Last-changedate](https://img.shields.io/badge/last%20change-2022--07--13-brightgreen.svg)](https://github.com/gurol/gcategory) [![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)  [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--9337--097X-green.svg)](https://orcid.org/0000-0002-9337-097X)

This platform is an interactive research compedium of my academic publication below.

> [Gürol Canbek](http:gurol.canbek.com) (2022). G-Category: A novel method to quantifying and categorizing data sets. *Journal of Machine Learning Research* (To be submitted).

The platform provides ready-to-run open-source R scripts for the new method called G-Category (Greatness Category). The method is proposed in the article above to categorize the sizes of a group of data sets in two dimensions: sample space and feature space. The G-Categories are small, medium, shallow, skinny, and large. An experimenter is prepared to test the G-Category method in example synthetic (linear and random size distributions) and the real data sets found in the literature.

The results are given for two approaches: pure geometric (correct) approach and pure arithmetic (erroneous) approach to see the difference. Refer to the article for more information.

**Note**: Please, cite my article if you would like to use and/or adapt the code, datasets, methodology, and other materials provided and [let us know](mailto:gurol@canbek.com?subject=G-category). Thank you for your interest.

Skip to Quick Start section below to learn how to use this platform.

## How Can I Categorize My Data Sets?
You can calculate the G-Categories of your own group of data sets in R using our scripts. Just do the following six steps:
- First, copy our two R scripts (gcategory.R and powerstat.R) in your folder.
- Second, include our main script (i.e. `source('gcategory.R')`) in your script file or in R interactive console
- Third, store the sample sizes of your data sets in a vector (e.g. `n <- c(100, 200, 300)`)
- Fourth, store the corresponding feature space sizes of your data sets in another vector (e.g. `m <- c(10, 12, 13)`)
- Fifth, name the corresponding data sets (e.g. `DSs <- c('DS1', 'DS2', 'DS3')`)
- Finally, use the provided functions (such as `greatnessCategories`, `plotTableGCsDetailed`, `plotGraphGCs`)

A minimal example:

```R
# Put the gcategory.R and powerstat.R script fiiles in your current directory
source('gcategory.R')
# Sample space sizes
n <- c(100, 200, 300)
# Feature space sizes
m <- c(10, 12, 13)
# Data set names
DSs <- c('DS1', 'DS2', 'DS3')
# Using default (correct) approach (pure geometric) (power=0 and theta=1)
greatnessCategories(m, n)
tabulateGCs(m, n, DSs)
```

The outputs are
```
[1] "Small"  "Medium" "Large"

    10          12           13           
300                          DS3 (Large)
200             DS2 (Medium)            
100 DS1 (Small)               
```

## Quick Start
This platform provides **Data** on the bottom-left, **Code** on the top-left, and **Results** on the right pane.

You can explore any file by clicking. The results of a pre-runned experimentation elaborated in the article is already provided in **Results** pane.

If you would like to experiment on your own, you can;

- Click **Run** botton ![](https://raw.githubusercontent.com/gurol/dsanalysis/master/temp/run_button.png) on the right of the top toolbar to launch experimentation. After the run is finished, the files (tabular data and graphics) are populated in the **Results** pane at the right for your review.

The original code repository and future updates can be found at [https://github.com/gurol/gcategory](https://github.com/gurol/gcategory)

### File Contents

```
├── code
│   ├── Experimenter.R : Experiment G-Category method in synthetic and real data sets
│   │                    (total five data sets).
│   ├── gcategory.R : The module for calculating G-Categories
│   ├── LICENSE : License file
│   ├── main.R : Starter R script (internal file for this platform)
│   ├── powerstat.R : Script for calculating several statistics such as mean, standard
│   │                 deviation, z-scored based on the power coefficient.
│   ├── README.md : This help file
│   └── run.sh : Shell script (internal file for this platform)
│
├── data
│   └── (No Data)
│
└── results
    ├── output : Output log of the experimentation (showing the steps)
    ├── 1_SyntheticDSs_Linear : The folder holding the results for the synthetic
    │                           data sets having linear space size distributions.
    ├── 2_SyntheticDSs_Random : The folder holding the results for the synthetic
    │                           data sets having random space size distributions.
    ├── 3_BenignDSs : The folder holding the results for the real data sets in the
    │                 literature (Android benign application samples).
    ├── 4_MalignDSs : The folder holding the results for the real data sets in the
    │                 literature (Android malign application (malware) samples).
    ├── 5_MalwareFamilyDSs : The folder holding the results for the real data sets in
    │                        the literature (Android malign application (malware) samples
    │                        having malware family information or the recent samples).
    │
    └── [in each folder above ("n" is the configuration number)]
        ├── n(ari/geo)_a(DataSetsName).png : G-Categories calculated via arithmetic/
        │                                    geometric approach. It shows detailed
        │                                    information per data set such as Z-scores.
        ├── n(ari/geo)_b(DataSetsName)Graph.png : G-Categories calculated via arithmetic/
        │                                    geometric approach are shown in space graph
        ├── n(ari/geo)_c(DataSetsName)Combination.png : G-Categories calculated for the
        │                                    data sets having all the combination of the
        │                                    space sizes (via arithmetic/
        │                                    geometric approach)
        ├── n(ari/geo)_d(DataSetsName).csv : Tabulated G-Categories calculated via
        │                                    arithmetic/geometric approach
        └── n(ari/geo)_e(DataSetsName)Combination.csv : Tabulated G-Categories calculated 
                                             for the data sets having all the combination
                                             of the space sizes (via arithmetic/geometric
                                             approach)
```

Copyright (C) 2017-2022 Gürol Canbek
