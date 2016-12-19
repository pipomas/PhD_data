Basal mechanisms of information processing and mental ability: The contributions of mental speed, and spatial suppression to individual differences in psychometric intelligence
================

last updated on December 19, 2016 :balloon:

This repository, PhD\_data, is a digital directory you can use to reproduce the results reported in [PhD\_thesis](https://github.com/pipomas/PhD_thesis). To do so, you need [R](https://cran.r-project.org) installed on your system. Click on **Download ZIP File** to download this repository. Then, [set the downloaded folder as your R working directory](http://rfunction.com/archives/1001).

Repository content
------------------

-   `LICENCE`: `data/processed/dat.csv` is is licensed under a [*Creative Commons Attribution - NonCommercial - NoDerivatives 2.5 Switzerland License*](http://creativecommons.org/licenses/by-nc-nd/2.5/ch). Read the licence carefully to see what you are allowed to do.

For the reproduction of the results, you need to run the files in this folder

-   `scripts`: Folder containing R scripts
    -   `1_Read_in_data.R`: Code for loading the required packages and reading in the csv file.
    -   `2_Analyses.R`: Code for the reproduction of statistics reported in text and tables.

Other files in this repository

-   `data/processed/dat.csv`: Csv file used for the analyses.
-   `scripts/source_scripts`: The functions in this folder get sourced from the code which reproduce the results. Do not change any of these.
-   `scripts/tikzDevice`: This folder contains scripts to transform plotting commands issued by R functions into LaTeX code blocks. Used to draw a majority of the figures. You need to run `1_Read_in_data.R` before using the code. The files in this folder are not well commented, but readable with basic knowledge of base graph functions.

<!-- ### Files to ignore -->
<!-- Some files in this repository are only used for the displaying of this website or to tell [git](https://en.wikipedia.org/wiki/Git) which files not to track. You can ignore these files and folders: -->
<!-- * `javascripts`  -->
<!-- * `stylesheets`  -->
<!-- * `.gitignore`  -->
<!-- * `index.html`  -->
<!-- * `params.json` -->
<!-- ### `read_raw_files` <- not uploaded yet -->
<!-- * `A-Q`: Scripts to reproduce the reported results -->
<!-- * `read_raw_files`: Folder contains scripts to generate `data/processed/dat.csv` -->
<!--     + `BIS`: Run the script `1.read_in_BIS.R` to read the excel file. To drop the selected subjects from the sample, run `2.drop_subjects_BIS.R`. -->
<!--     + `Fragebogen`: `1.read_in_questionnaire.R` reads the csv file produced by [EFS](http://www.unipark.de/www/front.php). `2.drop_subjects_questionnaire.R` drops selected subjects. -->
<!--     + `Hick`: `1.Hick_analysis_3SD.R` reads in raw data -->
<!--     + `Supp`: `1.Supp2_analysis.R` reads in raw data, `2.drop_subjects_Supp2.R` drops selected subjects. -->
<!--     + `Merge`: `merge_objects_to_dat.R` merges all objects into one data frame, and writes `data/processed/dat.csv`. In order for it to work, make sure you **run all other scripts in this folder first**. -->
