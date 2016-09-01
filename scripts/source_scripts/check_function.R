# Define 'ipak' function
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Use 'ipak' function to install 'pacman' package. 'pacman' is needed because
# it provides the 'p_loaded' function used in the 'check' function (below)
ipak("pacman")

# Define 'check' function
check <- function(){
  cat("\014")
  if (all(is.element(mypackages, p_loaded(character.only = TRUE)) == TRUE) &
      exists("dat")){
    message("\n\nData and packages were successfully loaded :)\nYou can now run the file `2_Analyses.R`.\n\n")
    }
  else {
    message("\n\nOooops, something went wrong :(\nEither not all of the packages, or the csv file were not loaded correctly. Try to run the code line by line and see were the error occurs.\n\n")
  }
  rm(list=setdiff(ls(), c("dat")))
}