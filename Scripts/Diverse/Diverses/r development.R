
# Visualize stabilization of r as a function of subjects tested --------
# =========================================================================

# create empty vector ---------------------------------
yvalues <- c()

# get r and paste in vector 'yvalues' -----------------
for (i in 4:nrow(res)) {
  r <- corr.test(res[1:i, c("SI2","zTotal")])$r[1,2]
  yvalues[i] <- r
  }

# use generic plot function ---------------------------
plot(yvalues, type = "o")

# ggplot version of plot ------------------------------
# create data frame
dat <- data.frame(x=as.vector(1:176), y=yvalues, edu=res$edu)

# create plot
ggplot(data=dat, aes(x=x, y=y)) +
    geom_line() +
    geom_point() +
    geom_abline(slope=0, intercept= 0) + 
    xlab("Subject") + ylab("r") +
    ggtitle("Development")

# TEST: split factor by levels --------------------------
dat$edu <- recode(dat$edu, "c('Anderes','obligatorische Schulzeit', 'Berufslehre')='tief'; c('Berufsmatura', 'Matura','Bachelor','Master')='hoch'")
