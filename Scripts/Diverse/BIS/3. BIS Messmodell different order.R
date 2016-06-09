################################################################################
################################################################################
###                                                                             
###   BIS CFA's 
###
###   Philipp Thomas, 18. Januar 2016

library(lavaan)
library(semPlot)

# 0. check 'resultsBIS'
ls()

# corr.test(resultsBIS[9:27])
################################################################################
################################################################################

# 1. model 1        ------------------------------------------------------------
# .... 1.1 model specification -------------------------------------------------
model1 <- 'gBIS =~ zAN + zBD + zCH + zKW + zOG + zOE + zRZ + zSC + zST +
                   zTG + zTM + zWA + zWE + zWM + zXG + zZN + zZP + zZZ'

# .... 1.2 fit -----------------------------------------------------------------
model1Fit <- sem(model1,
                     data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                     estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                     mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 1.3 Anzeigen des Modelltests --------------------------------------------
model1Fit

# .... 1.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model1Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model1Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 1.5 Plotten -------------------------------------------------------------
semPaths(model1Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 3,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1)               #rotation of the whole model (1,2,3,4)

# text(0,-1.5, labels = "Chi-Square (135) = 439, p < .001\n CFI = .72, RMSEA = .10 - .12, SRMR = .09")


################################################################################
################################################################################


# 2. model 2 -------------------------------------------------------------------
# .... 2.1 model specification -------------------------------------------------
model2 <- 'speed    =~ zRZ + s2*zOE + s3*zTG + s4*zKW + s5*zXG + s6*zBD
           memory   =~ zZZ + t2*zOG + t3*zWE + t4*zST + t5*zWM + t6*zZP
           capacity =~ zCH + u2*zAN + u3*zSC + u4*zWA + u5*zTM + u6*zZN

           memory ~~ t7*speed    ; t7 > .01
           memory ~~ t8*capacity ; t8 > .01

           s2 > .01; s3 > 0; s4 > .01; s5 > 0; s6 > .01
           t2 > 0; t3 > 0; t4 > 0; t5 > 0; t6 > 0
           u2 > 0; u3 > 0; u4 > 0; u5 > 0; u6 > 0

           figural  =~ zOG + v2*zOE + v3*zBD + v4*zWE + v5*zAN + v6*zCH 
           verbal   =~ zTM + w2*zTG + w3*zKW + w4*zST + w5*zWM + w6*zWA
           numeric  =~ zSC + x2*zXG + x3*zRZ + x4*zZP + x5*zZZ + x6*zZN

           v2 > 0; v3 > 0; v4 > 0.1; v5 > 0; v6 > 0.0
           w2 > 0; w3 > 0; w4 > 0; w5 > 0; w6 > 0
           x2 > 0; x3 > 0; x4 > 0; x5 > 0; x6 > 0

           speed    ~~ 0*figural
           speed    ~~ 0*verbal
           speed    ~~ 0*numeric

           memory   ~~ 0*figural
           memory   ~~ 0*verbal
           memory   ~~ 0*numeric

           capacity ~~ 0*figural
           capacity ~~ 0*verbal
           capacity ~~ 0*numeric

           # figural ~~ 0*verbal
           # figural ~~ 0*numeric
           # verbal  ~~ 0*numeric
           # 
           speed ~~ s7*speed; s7 > .001
           # 
           # zAN ~~ a1*zAN; a1 > 0
           # zBD ~~ b1*zBD; b1 > 0
           # zCH ~~ c1*zCH; c1 > 0
           # zKW ~~ d1*zKW; d1 > 0
           # zOG ~~ e1*zOG; e1 > 0
           # zOE ~~ f1*zOE; f1 > .0
           # zRZ ~~ g1*zRZ; g1 > 0
           # zSC ~~ h1*zSC; h1 > 0
           # zST ~~ i1*zST; i1 > 0
           # zTG ~~ j1*zTG; j1 > 0
           # zTM ~~ k1*zTM; k1 > 0
           # zWA ~~ l1*zWA; l1 > 0
           # zWE ~~ m1*zWE; m1 > 0
           # zWM ~~ n1*zWM; n1 > 0
           # zXG ~~ o1*zXG; o1 > 0
           # zZN ~~ p1*zZN; p1 > 0
           # zZP ~~ q1*zZP; q1 > 0
           # zZZ ~~ r1*zZZ; r1 > 0
           '


# .... 2.2 fit -----------------------------------------------------------------
model2Fit <- sem(model2,
                 data      = resultsBIS,        
                 estimator = "MLM",      
                 mimic     = "Mplus")   

# .... 2.3 Anzeigen des Modelltests --------------------------------------------
model2Fit

# .... 2.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model2Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model2Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 2.5 Plotten -------------------------------------------------------------
semPaths(model2Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = 1e-100,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 3,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)               #rotation of the whole model (1,2,3,4)

# text(0,-1.5, labels = "Chi-Square (111) = 99, p = .80\n CFI = 1, RMSEA = 0, SRMR = .06\n covariance not + definite")

################################################################################
################################################################################

# 3. model 3 -------------------------------------------------------------------
# .... 3.1 model specification -------------------------------------------------
model3 <- 'g =~ speed + memory + capacity
  
            g ~~ 0*figural
            g ~~ 0*verbal
            g ~~ 0*numeric

           speed    =~ zRZ + s2*zOE + s3*zTG + s4*zKW + s5*zXG + s6*zBD
           memory   =~ zZZ + t2*zOG + t3*zWE + t4*zST + t5*zWM + t6*zZP
           capacity =~ zCH + u2*zAN + u3*zSC + u4*zWA + u5*zTM + u6*zZN

           # memory ~~ t7*speed    ; t7 > .01
           # memory ~~ t8*capacity ; t8 > .01
           # 
           # s2 > .01; s3 > 0; s4 > .01; s5 > 0; s6 > .01
           # t2 > 0; t3 > 0; t4 > 0; t5 > 0; t6 > 0
           # u2 > 0; u3 > 0; u4 > 0; u5 > 0; u6 > 0

           figural  =~ zOG + v2*zOE + v3*zBD + v4*zWE + v5*zAN + v6*zCH 
           verbal   =~ zTM + w2*zTG + w3*zKW + w4*zST + w5*zWM + w6*zWA
           numeric  =~ zSC + x2*zXG + x3*zRZ + x4*zZP + x5*zZZ + x6*zZN

           v2 > 0; v3 > 0; v4 > 0.1; v5 > 0; v6 > 0.0
           w2 > 0; w3 > 0; w4 > 0; w5 > 0; w6 > 0
           x2 > 0; x3 > 0; x4 > 0; x5 > 0; x6 > 0

           speed    ~~ 0*figural
           speed    ~~ 0*verbal
           speed    ~~ 0*numeric

           memory   ~~ 0*figural
           memory   ~~ 0*verbal
           memory   ~~ 0*numeric

           capacity ~~ 0*figural
           capacity ~~ 0*verbal
           capacity ~~ 0*numeric

           # figural ~~ 0*verbal
           # figural ~~ 0*numeric
           # verbal  ~~ 0*numeric
           # 
           # speed ~~ s7*speed; s7 > .001
           # 
           # zAN ~~ a1*zAN; a1 > 0
           # zBD ~~ b1*zBD; b1 > 0
           # zCH ~~ c1*zCH; c1 > 0
           # zKW ~~ d1*zKW; d1 > 0
           # zOG ~~ e1*zOG; e1 > 0
           # zOE ~~ f1*zOE; f1 > .0
           # zRZ ~~ g1*zRZ; g1 > 0
           # zSC ~~ h1*zSC; h1 > 0
           # zST ~~ i1*zST; i1 > 0
           # zTG ~~ j1*zTG; j1 > 0
           # zTM ~~ k1*zTM; k1 > 0
           # zWA ~~ l1*zWA; l1 > 0
           # zWE ~~ m1*zWE; m1 > 0
           # zWM ~~ n1*zWM; n1 > 0
           # zXG ~~ o1*zXG; o1 > 0
           # zZN ~~ p1*zZN; p1 > 0
           # zZP ~~ q1*zZP; q1 > 0
           # zZZ ~~ r1*zZZ; r1 > 0
           '


# .... 3.2 fit -----------------------------------------------------------------
model3Fit <- sem(model3,
                 data      = resultsBIS,        
                 estimator = "MLM",      
                 mimic     = "Mplus")   

# .... 3.3 Anzeigen des Modelltests --------------------------------------------
model3Fit

# .... 3.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model3Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model3Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 3.5 Plotten -------------------------------------------------------------
semPaths(model3Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = 1e-100,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 3,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)               #rotation of the whole model (1,2,3,4)

# text(0,-1.5, labels = "Chi-Square (111) = 99, p = .80\n CFI = 1, RMSEA = 0, SRMR = .06\n covariance not + definite")

################################################################################
################################################################################


# model 4 ----------------------------------------------------------------------
# .... 4.1 model specification -------------------------------------------------
model4 <- 'g    =~ zSpeedNumeric + zSpeedVerbal +  zSpeedFigural +
                   zMemoryFigural + zMemoryVerbal + zMemoryNumeric +
                   zCapacityFigural + zCapacityVerbal + zCapacityNumeric 
                   '
          

# .... 4.2 fit -----------------------------------------------------------------
model4Fit <- sem(model4,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 4.3 Anzeigen des Modelltests --------------------------------------------
model4Fit

# .... 4.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model4Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model4Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 4.5 Plotten -------------------------------------------------------------
semPaths(model4Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = 1e-100,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (12) = 9, p = .70\n CFI = 1, RMSEA = 0, SRMR = .02")










# model 5 ----------------------------------------------------------------------
# .... 5.1 model specification -------------------------------------------------
model5 <- 'speed    =~ zSpeedVerbal    + j1*zSpeedFigural    + j2*zSpeedNumeric
          memory   =~ zMemoryFigural   + k1*zMemoryVerbal   + k2*zMemoryNumeric
          capacity =~ zCapacityFigural + l1*zCapacityVerbal + l2*zCapacityNumeric
          
          figural =~ zCapacityFigural + m1*zMemoryFigural + m2*zSpeedFigural
          verbal  =~ zSpeedVerbal + n1*zMemoryVerbal + n2*zCapacityVerbal
          numeric =~ zSpeedNumeric + o1*zMemoryNumeric + o2*zCapacityNumeric

          j1 > 0; j2 > 0
          k1 > 0; k2 > 0
          l1 > 0; l2 > 0

          j1 > 0; j2 > 0
          k1 > 0; k2 > 0
          l1 > 0; l2 > 0

          speed ~~ p1*memory     ; p1 > 0
          speed ~~ q1*capacity   ; q1 > 0
          memory ~~ r1*capacity  ; r1 > 0


          speed ~~ 0*figural
          speed ~~ 0*verbal
          speed ~~ 0*numeric

          memory ~~ 0*figural
          memory ~~ 0*verbal
          memory ~~ 0*numeric

          capacity ~~ 0*figural
          capacity ~~ 0*verbal
          capacity ~~ 0*numeric

          # figural ~~ 0*verbal
          # figural ~~ 0*numeric
          # verbal ~~ 0*numeric
          
          
zSpeedFigural ~~ a1* zSpeedFigural; a1 > 0
zSpeedVerbal  ~~ b1* zSpeedVerbal; b1 > 0
zSpeedNumeric ~~ c1* zSpeedNumeric; c1 > 0

zMemoryFigural ~~ d1* zMemoryFigural; d1 > 0
zMemoryVerbal  ~~ e1* zMemoryVerbal; e1 > 0
zMemoryNumeric ~~ f1* zMemoryNumeric; f1 > 0

zCapacityFigural ~~ g1* zCapacityFigural; g1 > .001
zCapacityVerbal  ~~ h1* zCapacityVerbal; h1 > 0
zCapacityNumeric ~~ i1* zCapacityNumeric; i1 > 0
'


# .... 5.2 fit -----------------------------------------------------------------
model5Fit <- sem(model5,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 5.3 Anzeigen des Modelltests --------------------------------------------
model5Fit

# .... 5.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model5Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model5Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 5.5 Plotten -------------------------------------------------------------
semPaths(model5Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = 1e-100,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (12) = 9, p = .70\n CFI = 1, RMSEA = 0, SRMR = .02")






# model 6 ----------------------------------------------------------------------
# .... 6.1 model specification -------------------------------------------------
model6 <- '
          g =~ speed + memory + capacity
          
          g ~~ 0*figural
          g ~~ 0*verbal
          g ~~ 0*numeric
          
          speed    =~ zSpeedVerbal    + j1*zSpeedFigural    + j2*zSpeedNumeric
          memory   =~ zMemoryFigural   + k1*zMemoryVerbal   + k2*zMemoryNumeric
          capacity =~ zCapacityFigural + l1*zCapacityVerbal + l2*zCapacityNumeric
          
          figural =~ zCapacityFigural + m1*zMemoryFigural + m2*zSpeedFigural
          verbal  =~ zSpeedVerbal + n1*zMemoryVerbal + n2*zCapacityVerbal
          numeric =~ zSpeedNumeric + o1*zMemoryNumeric + o2*zCapacityNumeric

          j1 > 0; j2 > .01
          k1 > 0; k2 > 0
          l1 > 0; l2 > 0

          j1 > 0; j2 > 0
          k1 > 0; k2 > 0
          l1 > 0; l2 > 0

          speed ~~ p1*memory     ; p1 > 0
          speed ~~ q1*capacity   ; q1 > 0
          memory ~~ r1*capacity  ; r1 > 0


          speed ~~ 0*figural
          speed ~~ 0*verbal
          speed ~~ 0*numeric

          memory ~~ 0*figural
          memory ~~ 0*verbal
          memory ~~ 0*numeric

          capacity ~~ 0*figural
          capacity ~~ 0*verbal
          capacity ~~ 0*numeric

          # figural ~~ 0*verbal
          # figural ~~ 0*numeric
          # verbal ~~ 0*numeric
          
          
zSpeedFigural ~~ a1* zSpeedFigural; a1 > 0
zSpeedVerbal  ~~ b1* zSpeedVerbal; b1 > 0
zSpeedNumeric ~~ c1* zSpeedNumeric; c1 > 0

zMemoryFigural ~~ d1* zMemoryFigural; d1 > 0
zMemoryVerbal  ~~ e1* zMemoryVerbal; e1 > 0
zMemoryNumeric ~~ f1* zMemoryNumeric; f1 > 0

zCapacityFigural ~~ g1* zCapacityFigural; g1 > .001
zCapacityVerbal  ~~ h1* zCapacityVerbal; h1 > 0
zCapacityNumeric ~~ i1* zCapacityNumeric; i1 > 0'


# .... 6.2 fit -----------------------------------------------------------------
model6Fit <- sem(model6,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 6.3 Anzeigen des Modelltests --------------------------------------------
model6Fit

# .... 6.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model6Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model6Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 6.5 Plotten -------------------------------------------------------------
semPaths(model6Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = 1e-100,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (12) = 9, p = .70\n CFI = 1, RMSEA = 0, SRMR = .02")





# model 7 ----------------------------------------------------------------------
# .... 7.1 model specification -------------------------------------------------
model7 <- '
          g =~ figural + verbal + numeric
          
          g ~~ 0*speed
          g ~~ 0*memory
          g ~~ 0*capacity
          
          speed    =~ zSpeedVerbal    + j1*zSpeedFigural    + j2*zSpeedNumeric
          memory   =~ zMemoryFigural   + k1*zMemoryVerbal   + k2*zMemoryNumeric
          capacity =~ zCapacityFigural + l1*zCapacityVerbal + l2*zCapacityNumeric
          
          figural =~ zCapacityFigural + m1*zMemoryFigural + m2*zSpeedFigural
          verbal  =~ zSpeedVerbal + n1*zMemoryVerbal + n2*zCapacityVerbal
          numeric =~ zSpeedNumeric + o1*zMemoryNumeric + o2*zCapacityNumeric

          j1 > 0; j2 > .01
          k1 > 0; k2 > 0
          l1 > 0; l2 > 0

          j1 > 0; j2 > 0
          k1 > 0; k2 > 0
          l1 > 0; l2 > 0

          speed ~~ p1*memory    ; p1 > 0
          speed ~~ q1*capacity  ; q1 > 0
          memory ~~ r1*capacity ; r1 > 0


          speed ~~ 0*figural
          speed ~~ 0*verbal
          speed ~~ 0*numeric

          memory ~~ 0*figural
          memory ~~ 0*verbal
          memory ~~ 0*numeric

          capacity ~~ 0*figural
          capacity ~~ 0*verbal
          capacity ~~ 0*numeric

          # figural ~~ 0*verbal
          # figural ~~ 0*numeric
          # verbal ~~ 0*numeric
          
          
zSpeedFigural ~~ a1* zSpeedFigural; a1 > 0
zSpeedVerbal  ~~ b1* zSpeedVerbal; b1 > 0
zSpeedNumeric ~~ c1* zSpeedNumeric; c1 > 0

zMemoryFigural ~~ d1* zMemoryFigural; d1 > 0
zMemoryVerbal  ~~ e1* zMemoryVerbal; e1 > 0
zMemoryNumeric ~~ f1* zMemoryNumeric; f1 > 0

zCapacityFigural ~~ g1* zCapacityFigural; g1 > .001
zCapacityVerbal  ~~ h1* zCapacityVerbal; h1 > 0
zCapacityNumeric ~~ i1* zCapacityNumeric; i1 > 0'


# .... 7.2 fit -----------------------------------------------------------------
model7Fit <- sem(model7,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 7.3 Anzeigen des Modelltests --------------------------------------------
model7Fit

# .... 7.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model7Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model7Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 7.5 Plotten -------------------------------------------------------------
semPaths(model7Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = 1e-100,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (12) = 9, p = .70\n CFI = 1, RMSEA = 0, SRMR = .02")












# 8. model 8 -------------------------------------------------------------------
# .... 8.1 model specification -------------------------------------------------
model8 <- 'speed    =~ zSpeedNumeric    + zSpeedVerbal    + zSpeedFigural
           memory   =~ zMemoryFigural   + zMemoryVerbal   + zMemoryNumeric
           capacity =~ zCapacityFigural + zCapacityVerbal + zCapacityNumeric
          
           zSpeedNumeric  ~~ zMemoryNumeric
           zSpeedNumeric  ~~ zCapacityNumeric
           zMemoryNumeric ~~ zCapacityNumeric

           zSpeedFigural  ~~ zMemoryFigural
           zSpeedFigural  ~~ zCapacityFigural
           zMemoryFigural ~~ zCapacityFigural

           zSpeedVerbal  ~~ zMemoryVerbal
           zSpeedVerbal  ~~ zCapacityVerbal
           zMemoryVerbal ~~ zCapacityVerbal
           '


# .... 8.2 fit -----------------------------------------------------------------
model8Fit <- sem(model8,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 8.3 Anzeigen des Modelltests --------------------------------------------
model8Fit

# .... 8.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model8Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model8Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 8.5 Plotten -------------------------------------------------------------
semPaths(model8Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (15) = 99, p < .01\n CFI = .96, RMSEA = .6 - .13, SRMR = .04")


################################################################################
################################################################################


# 9. model 9 -------------------------------------------------------------------
# .... 9.1 model specification -------------------------------------------------
model9 <- 'g        =~ speed + memory + capacity

           speed    =~ zSpeedVerbal    + zSpeedNumeric    + zSpeedFigural
           memory   =~ zMemoryFigural   + zMemoryVerbal   + zMemoryNumeric
           capacity =~ zCapacityFigural + zCapacityVerbal + zCapacityNumeric

           speed ~~ a1*speed; a1 > .0000001
          
           zSpeedNumeric  ~~ zMemoryNumeric
           zSpeedNumeric  ~~ zCapacityNumeric
           zMemoryNumeric ~~ zCapacityNumeric

           zSpeedFigural  ~~ zMemoryFigural
           zSpeedFigural  ~~ zCapacityFigural
           zMemoryFigural ~~ zCapacityFigural

           zSpeedVerbal  ~~ zMemoryVerbal
           zSpeedVerbal  ~~ zCapacityVerbal
           zMemoryVerbal ~~ zCapacityVerbal
           '


# .... 9.2 fit -----------------------------------------------------------------
model9Fit <- sem(model9,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 9.3 Anzeigen des Modelltests --------------------------------------------
model9Fit

# .... 9.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model9Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model9Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 9.5 Plotten -------------------------------------------------------------
semPaths(model9Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (15) = 99, p < .01\n CFI = .96, RMSEA = .6 - .13, SRMR = .04")


################################################################################
################################################################################


# 10. model 10 -------------------------------------------------------------------
# .... 10.1 model specification -------------------------------------------------
model10 <- '
           speed    =~ zSpeedVerbal    + zSpeedNumeric + zSpeedFigural
           memory   =~ zMemoryFigural   + zMemoryVerbal   + zMemoryNumeric
           capacity =~ zCapacityFigural + zCapacityVerbal + zCapacityNumeric

           
           # zSpeedNumeric  ~~ zMemoryNumeric   # n.s.
           zSpeedNumeric  ~~ zCapacityNumeric
           # zMemoryNumeric ~~ zCapacityNumeric # n.s.

           # zSpeedFigural  ~~ zMemoryFigural   # n.s.
           # zSpeedFigural  ~~ zCapacityFigural # n.s.
           zMemoryFigural ~~ zCapacityFigural

           zSpeedVerbal  ~~ zMemoryVerbal
           zSpeedVerbal  ~~ zCapacityVerbal
           zMemoryVerbal ~~ zCapacityVerbal
            
            #zusätzliche Covarianzen
            zSpeedVerbal ~~ zCapacityFigural   # <- ist das plausibel?
            zSpeedNumeric ~~  zCapacityVerbal  # <- ist das plausibel?
            # zSpeedFigural ~~  zCapacityVerbal

           '


# .... 10.2 fit -----------------------------------------------------------------
model10Fit <- sem(model10,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 10.3 Anzeigen des Modelltests --------------------------------------------
model10Fit

# .... 10.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model10Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model10Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 10.5 Plotten -------------------------------------------------------------
semPaths(model10Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (15) = 99, p < .01\n CFI = .96, RMSEA = .6 - .13, SRMR = .04")

################################################################################
################################################################################


# 11. model 11 -------------------------------------------------------------------
# .... 11.1 model specification -------------------------------------------------
model11 <- 'g =~ speed + memory + capacity

           speed    =~ zSpeedVerbal    + zSpeedNumeric + zSpeedFigural
           memory   =~ zMemoryFigural   + zMemoryVerbal   + zMemoryNumeric
           capacity =~ zCapacityFigural + zCapacityVerbal + zCapacityNumeric

          speed ~~ a1*speed; a1 > 0

           
           # zSpeedNumeric  ~~ zMemoryNumeric   # n.s.
           zSpeedNumeric  ~~ zCapacityNumeric
           # zMemoryNumeric ~~ zCapacityNumeric # n.s.

           # zSpeedFigural  ~~ zMemoryFigural   # n.s.
           # zSpeedFigural  ~~ zCapacityFigural # n.s.
           zMemoryFigural ~~ zCapacityFigural

           zSpeedVerbal  ~~ zMemoryVerbal
           zSpeedVerbal  ~~ zCapacityVerbal
           zMemoryVerbal ~~ zCapacityVerbal
            
            #zusätzliche Covarianzen
            zSpeedVerbal ~~ zCapacityFigural   # <- ist das plausibel?
            zSpeedNumeric ~~  zCapacityVerbal  # <- ist das plausibel?
            # zSpeedFigural ~~  zCapacityVerbal
'


# .... 11.2 fit -----------------------------------------------------------------
model11Fit <- sem(model11,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 11.3 Anzeigen des Modelltests --------------------------------------------
model11Fit

# .... 11.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model11Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model11Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 11.5 Plotten -------------------------------------------------------------
semPaths(model11Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (15) = 99, p < .01\n CFI = .96, RMSEA = .6 - .13, SRMR = .04")



# 14. model 14 -------------------------------------------------------------------
# .... 14.1 model specification -------------------------------------------------
model14 <- 'g =~ zSpeedNumeric + zSpeedFigural    + zSpeedVerbal    +
                zMemoryFigural   + zMemoryVerbal   + zMemoryNumeric +
                zCapacityFigural + zCapacityVerbal + zCapacityNumeric

           speed    =~ zSpeedNumeric    + zSpeedFigural    + zSpeedVerbal 
           memory   =~ zMemoryFigural   + zMemoryVerbal   + zMemoryNumeric
           capacity =~ zCapacityFigural + zCapacityVerbal + zCapacityNumeric

          g ~~ 0*speed
          g ~~ 0*memory
          g ~~ 0*capacity

        speed ~~ 0*memory
        speed ~~ 0*capacity
        memory ~~ 0*capacity

       zCapacityFigural ~~ a1*zCapacityFigural; a1 > .001
        speed ~~ a2*speed; a2 > .001





           '


# .... 14.2 fit -----------------------------------------------------------------
model14Fit <- sem(model14,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 11.3 Anzeigen des Modelltests --------------------------------------------
model14Fit

# .... 11.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model14Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model14Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 11.5 Plotten -------------------------------------------------------------
semPaths(model14Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (15) = 99, p < .01\n CFI = .96, RMSEA = .6 - .13, SRMR = .04")

################################################################################
################################################################################



# 15. model 15 -------------------------------------------------------------------
# .... 15.1 model specification -------------------------------------------------
model15 <- 'g =~ zSpeed + zMemory + zCapacity'


# .... 15.2 fit -----------------------------------------------------------------
model15Fit <- sem(model15,
                data      = resultsBIS,        #Datensatz mit den in sem.model verwendeten Variablen
                estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 15.3 Anzeigen des Modelltests --------------------------------------------
model15Fit

# .... 15.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(model15Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(model15Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 15.5 Plotten -------------------------------------------------------------
semPaths(model15Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 6,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 2,               #number of digits for coeffiecients
         rotation       = 1,
         reorder = FALSE)  

# text(0,-1.65, labels = "Chi-Square (15) = 99, p < .01\n CFI = .96, RMSEA = .6 - .13, SRMR = .04")


