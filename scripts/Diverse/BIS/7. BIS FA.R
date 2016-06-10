dat <- resultsBIS[9:27]

lol <- fa(dat, fm = "pa",
          nfactors = 18,
          rotate = "none",
          SMC = F)
lol 
