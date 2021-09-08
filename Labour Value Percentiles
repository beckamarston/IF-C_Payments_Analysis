# set working directory
setwd(dir="//Ler365fs/fbs2/Users/TASPrototype/Non FBS Staff analysis/Rebecca Marston/Uplift Methodologies")

# install packages
#install.packages("openxlsx")
#install.packages("FBSCore")
#install.packages("survey")
#install.packages("srvyr")
#install.packages("readxl")

# loading packages
library(tidyverse)
library(openxlsx)
library(FBSCore)
library(survey)
library(readxl)
library(srvyr)

FBS_data <- read.csv("//ler365fs/fbs2/Users/TASPrototype/Non FBS Staff analysis/Rebecca Marston/Uplift Methodologies/Labour Values.csv")

# Getting error when using survey package - workaround 
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="remove")

design <- svydesign(id = ~ ï..farms,
                    strata = ~ stratum,
                    fpc =  ~ num.pop,
                    data = FBS_data,
                    weight = ~ newwt3yr,
                    nest = TRUE)

# Percentiles -----------------------------------------------------
# get un-weighted percentiles
unweighted_qauntile_gmperha <- quantile(FBS_data$Average.labour.value,c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
view(unweighted_qauntile_gmperha)
# get weighted percentiles 
labourvalues_weighted_quantiles <- svyquantile(x = ~Average.labour.value, 
                                              design = design, 
                                              quantiles = c(0,.1,.2,.25,.3,.4,.5,.55,.6,.7,.75,.8,.9,1), 
                                              alpha=0.05,
                                              ci=FALSE)
# write to csv file                             
write.csv(labourvalues_weighted_quantiles, "labourvalues_weighted_quantiles.csv") 

# Weighted percentiles by SLR size -------------------------------------------------
percentiles_slr_size <- svyby(~Average.labour.value, 
                                ~slrgroup, 
                                design,
                                svyquantile, 
                                quantiles=c(0,.1,.2,.25,.3,.4,.5,.55,.6,.7,.75,.8,.9,1),
                                ci=TRUE,
                                vartype="ci")

# write to csv file                             
write.csv(percentiles_slr_size, "percentiles_slr_size.csv") 
