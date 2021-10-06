library('magrittr')
# library('dplyr')
# library('tibble')
# library('tidyr')
# library('survey')
# library('FBSCore')
options(
  survey.adjust.domain.lonely = TRUE,
  survey.lonely.psu = 'remove'
)
setwd(dir='//ler365fs/fbs2/Users/TASPrototype/Non FBS Staff analysis/Rebecca Marston/Uplift Methodologies')



#### STEP 1:  Extract FBS Data! ####
varlist1 <- {c(
  'UAA',
  'area.farmed',
  'labour.force',
  'farmer.spouse.AWU',
  'FPD.AWU',
  'FPD.spouses.AWU',
  'unpaid.regular.AWU',
  'unpaid.casual.AWU',
  'manager.AWU',
  'paid.whole.time.AWU',
  'paid.part.time.AWU',
  'paid.casual.AWU',
  'trainee.AWU',
  'AWU',
  'AWU.check',
  'adjusted.AWU',
  'FBI',
  'farm.business.output',
  'farm.business.variable.costs',
  'farm.business.fixed.costs',
  'agriculture.gross.margin',
  'agriculture.income',
  'output.from.agriculture',
  'agriculture.fixed.costs',
  'agriculture.variable.costs',
  'agriculture.unpaid.labour',
  'BPS.income',
  'basic.payment.scheme',
  'agri.environment.income',
  'agri.environment.payments',
  'diversified.income',
  'diversified.output',
  'performance.ratio',
  'total.area'
)}
if (FALSE) {  #very slow!
  for (year in 2018:2019) {
    FBSCore::extract_spec(
      year = year,
      spec = tibble::tibble(variable=varlist1, name=rep(NA,length(varlist1)))
    ) %>%
      saveRDS(paste0('../FBS Data Extracts/Percentiles/', year, '.rds'))
  }
}
extracts <- data.frame()
for (year in 2017:2019) {
  extracts <- dplyr::bind_rows(
    extracts,
    paste0('../FBS Data Extracts/Percentiles/', year, '.rds') %>%
      readRDS() %>%
      dplyr::mutate(
        year = year
      )
  )
}



#### STEP 2:  Clean Data ####
arable <- c('Cereals', 'General cropping', 'Horticulture')
livestock <- c('LFA Grazing Livestock', 'Dairy', 'Lowland Grazing Livestock')

extracts <- extracts %>%
  dplyr::select(append(c('farms', 'year'), varlist1)) %>%
  dplyr::group_by(farms) %>%
  dplyr::summarise_all(
    function (...) {
      if (is.numeric(...)) {
        mean(...)
      } else {
        dplyr::first(...)
      }
    }
  )

fbsdata <- readRDS('../fbs_england_17_18_to_19_20_matched_3yr_sample_reweighted.rds') %>%
  dplyr::left_join(
    extracts,
    by = 'farms'
  ) %>%
  dplyr::filter(5 < UAA) %>%
  dplyr::mutate(
    agriculture.income.per.ha = agriculture.income / UAA,
    agriculture.gross.margin.per.ha = agriculture.gross.margin / UAA,
    base_system_type = dplyr::case_when(
      type %in% arable ~ 'BS_Arable',
      type %in% livestock ~ 'BS_Livestock',
      TRUE ~ 'other'
    ),
    base_system_type_2 = dplyr::case_when(
      type %in% arable ~ 'BS_Arable',
      type %in% livestock[c(1,3)] ~ 'BS_Livestock',
      TRUE ~ 'other'
    ),
    base_system_type_3 = dplyr::case_when(
      type %in% arable ~ 'BS_Arable',
      type %in% livestock[c(2,3)] ~ 'BS_Livestock',
      TRUE ~ 'other'
    )
  )



### STEP 3:  Weighted Percentiles ####
## Should varlist1 be extended?
# varlist1 <- append(varlist1, c(
#   'agriculture.income.per.ha',
#   'agriculture.gross.margin.per.ha',
#   'newwt3yr',
#   'base_system_type',
#   'base_system_type_2',
#   # 'base_system_type_3'
# ))
# or
# varlist1 <- names(fbsdata)
quantiles <- c(0,.1,.2,.25,.3,.4,.5,.55,.6,.7,.75,.8,.9,1)
design <- survey::svydesign(
  ids = ~farms,
  strata = ~stratum,
  fpc =  ~num.pop,
  data = fbsdata,
  weight = ~newwt3yr,
  nest = TRUE
)

# calculating some values ---------------------------------------------- UNUSED!
means <- FBSCore::mean_byfac(
  vars = varlist1,
  factor = 'type',
  design = design
)
totals <- FBSCore::total_byfac(
  vars = varlist1,
  factor = 'type',
  design = design
)
# total value by type 1
totals_bs_type1 <- FBSCore::total_byfac(
  vars = varlist1,
  factor = 'base_system_type',
  design = design
)
# total value by type 2
totals_bs_type2 <- FBSCore::total_byfac(
  vars = varlist1,
  factor = 'base_system_type_2',
  design = design
)
# un-weighted percentiles
unweighted_qauntile_gmperha <- quantile(
  fbsdata$agriculture.gross.margin.per.ha,
  seq(0,1,.1)
)
# weighted percentiles
gmperha_weighted_quantiles <- survey::svyquantile(
  x = ~agriculture.gross.margin.per.ha,
  design = design,
  quantiles = quantiles,
  alpha = 0.05,
  ci = FALSE
)
# weighted percentiles with 95% confidence interval
gmperha_weighted_quantiles_ci <- survey::svyquantile(
  x = ~agriculture.gross.margin.per.ha,
  design = design,
  quantiles = quantiles,
  alpha = 0.05,
  ci = TRUE
)[[2]]
# weighted quantiles per farm type
quantiles_farm_type_gm <- survey::svyby(
  ~agriculture.gross.margin.per.ha,
  ~type,
  design,
  survey::svyquantile,
  quantiles = quantiles,
  ci = TRUE,
  vartype = 'ci'
)
# weighted percentiles per base system type 1
quantiles_bs_gm <- survey::svyby(
  ~agriculture.gross.margin.per.ha,
  ~base_system_type,
  design,
  survey::svyquantile,
  quantiles = quantiles,
  ci = TRUE,
  vartype = 'ci'
)
# weighted percentiles per base system type 2
quantiles_bs_2_gm <- survey::svyby(
  ~agriculture.gross.margin.per.ha,
  ~base_system_type_2,
  design,
  survey::svyquantile,
  quantiles = quantiles,
  ci = TRUE,
  vartype = 'ci'
)
# weighted percentiles per base system type 3
quantiles_bs_3_gm <- survey::svyby(
  ~agriculture.gross.margin.per.ha,
  ~base_system_type_3,
  design,
  survey::svyquantile,
  quantiles = quantiles,
  ci = TRUE,
  vartype = 'ci'
)



#### STEP 4:  Export ####
dir.create('./aw/')
write.csv(totals_bs_type1, './aw/weighted_totals_bs1.csv')
write.csv(totals_bs_type2, './aw/weighted_totals_bs2.csv')
write.csv(gmperha_weighted_quantiles, './aw/gmperha_weighted_quantiles.csv')
write.csv(gmperha_weighted_quantiles_ci, './aw/gmperha_weighted_quantiles_ci.csv')
write.csv(quantiles_farm_type_gm, './aw/gmperha_quantiles_farmtype.csv')
write.csv(quantiles_bs_gm , './aw/gmperha_quantiles_bs.csv')
write.csv(quantiles_bs_2_gm , './aw/gmperha_quantiles_bs_2.csv')
write.csv(quantiles_bs_3_gm , './aw/gmperha_quantiles_bs_3.csv')
