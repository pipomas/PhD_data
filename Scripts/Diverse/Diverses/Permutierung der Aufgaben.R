################################################################################
################################################################################
###                                                                             
###   D O   P E R M U T A T I O N S    &   C R E A T E   D A T A   F R A M E
###   
###

library(gtools)      # <- needed to do permutations

#0. example(permutations) ------------------------------------------------------
permutations(5,5,letters[1:5])

#1. allow for reproducible results ---------------------------------------------
set.seed(123)

#2. create data.frame 'dat' and do the permutations ----------------------------
dat <- as.data.frame(permutations(n = 5,                    #size of source vector
                     r = 5,                                 #size of the target vectors
                     v = c("DDF","DDE","RP","TG","Hick"),   #source vector
                     set = TRUE,                            #duplicates should be removed?
                     repeats.allowed = FALSE))              #include duplicate values?

#3. check the structure of 'dat' -----------------------------------------------
str(dat)

#4. randomly shuffle the rows in object 'dat' ----------------------------------
dat <- dat[sample(x = nrow(dat),
                  size = 120,
                  replace = FALSE),]

#5. check resulting object -----------------------------------------------------
show(dat)

#6. save the data.frame as an xlsx file ----------------------------------------
library(xlsx) # <- needed to create xlsx files
write.xlsx(dat, "/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Diverses/Permutation.xlsx")
