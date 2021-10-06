library('magrittr')
# library(survey)
options(
  survey.adjust.domain.lonely = TRUE,
  survey.lonely.psu = 'remove'
)
setwd(dir = '//ler365fs/fbs2/Users/TASPrototype/Non FBS Staff analysis/Rebecca Marston/Uplift Methodologies')



#### Design ####
FBS_data <- read.csv('./Labour Values.csv')
quantiles <- c(0,.1,.2,.25,.3,.4,.5,.55,.6,.7,.75,.8,.9,1)
design <- survey::svydesign(
  id = ~ï..farms,
  strata = ~stratum,
  fpc =  ~num.pop,
  data = FBS_data,
  weight = ~newwt3yr,
  nest = TRUE
)



#### Percentiles ####
# un-weighted percentiles
unweighted_qauntile_gmperha <- quantile(
  FBS_data$Average.labour.value,
  seq(0,1,.1)
)
# weighted percentiles
labourvalues_weighted_quantiles <- survey::svyquantile(
  x = ~Average.labour.value,
  design = design,
  quantiles = quantiles,
  alpha = 0.05,
  ci = FALSE
)
# Weighted percentiles by SLR size
percentiles_slr_size <- survey::svyby(
  ~Average.labour.value,
  ~slrgroup,
  design,
  survey::svyquantile,
  quantiles = quantiles,
  ci = TRUE,
  vartype = 'ci'
)



#### Export ####
write.csv(labourvalues_weighted_quantiles, 'labourvalues_weighted_quantiles.csv')
write.csv(percentiles_slr_size, 'percentiles_slr_size.csv')
