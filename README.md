Data and scripts
================

This repository contains data and scripts you need to reproduce the results reported in repository [PhD\_thesis](https://github.com/pipomas/PhD_thesis).

### Prerequisites

-   You need [R](https://cran.r-project.org) installed on your system.
-   I recommend using [RStudio](https://www.rstudio.com/products/rstudio/download/) as editor, as it includes a console, syntax-highlighting, tools for plotting, history, and workspace management.
-   Download this repository by clicking on the green "*Clone or download*" button in the top right corner, and [set the downloaded folder as your R working directory](http://rfunction.com/archives/1001).
-   ...

### TODO: Adjust path to your directory

This will get updated, as soon as possible.

    Give examples

    ## [1] "2016-07-13 12:49:14 CEST"

Repository content
------------------

### *data*

-   `base` folder includes raw files of the tasks used
-   `processed` folder includes the csv used for the analyses

### *scripts* folder

-   Files `A_... - Q_...` contain analyses for the reported results
-   `read_raw_files` folder contains scripts to create the csv in the `data/processed` folder
-   `tikzDevice` folder contains scripts to generate .tex pictures
-   `===` sections in scripts correspond to sections or subsections in the pdf

A quick word of advice
----------------------

If you wonder what a R function does, place your cursor onto the function, for example `r.t|est()`, and press F1. Alternatively, you can type `?function` in the console. These commands open the help file for the respective function.

last updated on July 13, 2016.
