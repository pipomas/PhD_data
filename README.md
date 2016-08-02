Data and scripts
================

This repository contains data and scripts you need to reproduce the results reported in repository [PhD\_thesis](https://github.com/pipomas/PhD_thesis).

### Prerequisites

-   You need [R](https://cran.r-project.org) installed on your system.
-   I recommend using [RStudio](https://www.rstudio.com/products/rstudio/download/) as editor, as it includes a console, syntax-highlighting, tools for plotting, history, and workspace management.
-   Download this repository by clicking on the green «*Clone or download*» button in the top right corner, and [set the downloaded folder as your R working directory](http://rfunction.com/archives/1001).

Repository content
------------------

### `data`

-   `base`: This folder includes all raw files of the tasks used
-   `processed`: This folder stores the `dat.csv` file used for the analyses

### `scripts` &lt;- not uploaded yet

-   `A-Q`: Scripts to reproduce the reported results

-   `read_raw_files`: Folder contains scripts to generate `data/processed/dat.csv`
    -   `BIS`: Run the script `1.read_in_BIS.R` to read the excel file. To drop the selected subjects from the sample, run `2.drop_subjects_BIS.R`.
    -   `Fragebogen`: `1.read_in_questionnaire.R` reads the csv file produced by [EFS](http://www.unipark.de/www/front.php). `2.drop_subjects_questionnaire.R` drops selected subjects.
    -   `Hick`: `1.Hick_analysis_3SD.R` reads in raw data
    -   `Supp`: `1.Supp2_analysis.R` reads in raw data, `2.drop_subjects_Supp2.R` drops selected subjects.
    -   `Merge`: `merge_objects_to_dat.R` merges all objects into one data frame, and writes `data/processed/dat.csv`. In order for it to work, make sure you **run all other scripts in this folder first**.
-   `tikzDevice`: This folder contains scripts to transform plotting commands issued by R functions into LaTeX code blocks. Used to draw a majority of the figures.

Note
----

-   If you wonder what a R function does, place your cursor onto the function, for example `r.test()`, and press F1. Alternatively, you can type `?function` in the console. These commands open the help file for the respective function.

last updated on August 01, 2016 :tada:
