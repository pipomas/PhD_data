Der Zusammenhang zwischen Spatial-Suppression, Mental-Speed und psychometrischer Intelligenz
================

English title: **The relationship between spatial suppression, mental speed and psychometric intelligence**

Last updated on: April 01, 2017 :balloon:

This repository, PhD\_data, is a digital directory you can use to reproduce the results reported in <a href="https://github.com/pipomas/PhD_thesis" target="_blank">PhD\_thesis</a>. To do so, you need <a href="https://cran.r-project.org" target="_blank">R</a> installed on your system. Click on **Download ZIP File** to download this repository. Then, <a href="http://rfunction.com/archives/1001" target="_blank">set the downloaded folder as your R working directory</a>

Repository content
------------------

-   `LICENCE`: `data/processed/dat.csv` is is licensed under a <a href="http://creativecommons.org/licenses/by-nc-nd/2.5/ch" target="_blank">*Creative Commons Attribution - NonCommercial - NoDerivatives 2.5 Switzerland License*</a>.

For the reproduction of the results, you need to run the files in this folder

-   `scripts`: Folder containing R scripts
    -   `1_Read_in_data.R`: Code for loading the required packages and reading in the csv file.
    -   `2_Analyses.R`: Code for the reproduction of statistics reported in text and tables.

Other files in this repository

-   `data/processed/dat.csv`: Csv file used for the analyses.
-   `scripts/source_scripts`: The functions in this folder get sourced from the code which reproduce the results. Do not change any of these.
-   `scripts/tikzDevice`: This folder contains scripts to transform plotting commands issued by R functions into LaTeX code blocks. Used to draw a majority of the figures. You need to run `1_Read_in_data.R` before using the code. The files in this folder are not well commented, but readable with basic knowledge of base graph functions.
