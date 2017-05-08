############################################################
##                      DESCRIPTION                       ##
############################################################

# pre-process the dataset to get ready for data quality report
# and data preparation

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
source("misc.R")

############################################################
##                      INITIALIZE                        ##
############################################################

log.write("start data_pre_process script")

############################################################
##                          LOAD                          ##
############################################################

# read original data
data_orig <-
  read.csv(mp(DATA_ORIGINAL_DIR, "data_main.csv"), stringsAsFactors = FALSE)
meta_orig <-
  read.csv(mp(DATA_ORIGINAL_DIR, "meta_main.csv"), stringsAsFactors = FALSE)
# set MAIN
data_seed <- data_orig
meta_seed <- meta_orig

############################################################
##                         TRIM                           ##
############################################################

to_drop <- c(
  # @TR001
  # 16_frac suffix
  "green16_frac",
  "libert16_frac",
  "other16_frac",
  
  # @TR002
  # _frac2 suffix
  "rep16_frac2",
  "dem16_frac2",
  "rep12_frac2",
  "rep08_frac2",
  "dem12_frac2",
  "dem08_frac2",
  
  # @TR003
  # votes16_ prefix (except clintonh and trumpd)
  "votes16_johnsong",
  "votes16_steinj",
  "votes16_castled",
  "votes16_de_la_fuenter",
  "votes16_mcmulline",
  "votes16_hedgesj",
  "votes16_kahnl",
  "votes16_la_rivag",
  "votes16_hoeflingt",
  "votes16_kenistonc",
  "votes16_smithm",
  "votes16_atwoodf",
  "votes16_kennedya",
  "votes16_kopitkek",
  "votes16_kotlikoffl",
  "votes16_lyttleb",
  "votes16_maldonadoj",
  "votes16_maturenm",
  "votes16_scottr",
  "votes16_silvar",
  "votes16_soltysike",
  "votes16_vacekd",
  "votes16_copelands",
  "votes16_jacobp",
  "votes16_whitej",
  "votes16_mooreheadm",
  "votes16_none_of_these_candidates",
  "votes16_duncanr",
  "votes16_skewesp",
  "votes16_giordanir",

  # @TR004
  # election data scraping scraps
  "name_prev",
  "ST",
  "County",
  "State",
  "votes",
  "reporting",
  "precincts",
  "X",
  "Y",
  "X1",
  
  # @TR005
  # education (redundant)
  "At.Least.High.School.Diploma",
  
  # @TR006
  # weather data scraping scraps
  "nearest_county",
  "temp",
  "precip",
  "temp_bins",
  "lat_bins",
  "lon_bins",
  "precip_bins",
  "elevation_bins",
  "lon",
  "lat",
  "elevation",
  
  # @TR007
  # weather (irrelavent)
  "annual_PRCP",
  "summer_PRCP",
  "spring_PRCP",
  "autumn_PRCP",
  "annual_TAVG",
  "annual_TMAX",
  "annual_TMIN",
  "winter_TMAX",
  "winter_TMIN",
  "summer_TAVG",
  "summer_TMAX",
  "summer_TMIN",
  "spring_TAVG",
  "spring_TMAX",
  "spring_TMIN",
  "autumn_TAVG",
  "autumn_TMAX",
  "autumn_TMIN",
  
  # @TR008
  # race (redundant)
  "White",
  "Black",
  "Hispanic",
  "Asian",
  "Amerindian",
  "Other",
  "White_Asian",
  
  # @TR009
  # socioeconomic indicator
  "CA",
  "S",
  "MAR",
  "CFS",
  "ACFS",
  "MeanALC",
  "MaxALC",
  "Mixedness"
)

# trim unnecessary features
meta_seed <- meta_orig[!(meta_orig$name %in% to_drop),]
data_seed <- data_orig[,!(names(data_orig) %in% to_drop)]

# @TR010
# delete instance of Oglala Lakota, Alaska, counties without name and fips
data_seed <- data_seed[!is.na(data_seed$fips),]
data_seed <-
  data_seed[!(data_seed$name_16 %in% c("", "Alaska", "Oglala Lakota")), ]

############################################################
##                         EDIT                           ##
############################################################

# +----------------------------------+
# |             WEATHER              |
# +----------------------------------+
# change back to the right unit
# @ED001
data_seed$winter_PRCP <- data_seed$winter_PRCP / 100
# @ED002
data_seed$winter_TAVG <- data_seed$winter_TAVG / 10
# update metadata
meta_seed <- metanize(meta_seed, "winter_PRCP", status = "")
meta_seed <- metanize(meta_seed, "winter_TAVG", lbound = -200, ubound = 200, status = "")

############################################################
##                         ADD                            ##
############################################################

# @AD001
# +----------------------------------+
# |          OTHERS' VOTE            |
# +----------------------------------+
# 2016 other parties' vote counts and fraction
data_seed$votes16_others <-
  data_seed$total16 - (data_seed$votes16_clintonh + data_seed$votes16_trumpd)
data_seed$other16_frac <-
  1 - (data_seed$rep16_frac + data_seed$dem16_frac)
# update metadata
meta_seed <-
  metanize(
    meta_seed,
    "votes16_others",
    "elec_other16",
    "Other Parties Votes 2016",
    "election",
    NA,
    "number of votes of other parties than Democratic and Republic 2016",
    "",
    0,
    NA,
    "",
    "",
    "2016",
    "http://www.nytimes.com/elections/results/president"
  )
meta_seed <-
  metanize(
    meta_seed,
    "other16_frac",
    "elec_other16_frac",
    "Other Parties Vote Fraction 2016",
    "election",
    NA,
    "vote fraction of other parties than Democratic and Republic 2016",
    "",
    0,
    1,
    "",
    "",
    "2016",
    "http://www.nytimes.com/elections/results/president"
  )

# @AD002
# +----------------------------------+
# |         ELECTION WINNER          |
# +----------------------------------+
# binary value to indicate republican's victory
data_seed$rep16_win <-
  data_seed$votes16_clintonh < data_seed$votes16_trumpd
data_seed$rep12_win <- data_seed$dem12 < data_seed$rep12
data_seed$rep08_win <- data_seed$dem08 < data_seed$rep08
# update metadata
meta_seed <-
  metanize(
    meta_seed,
    "rep16_win",
    "elec_rep16_win",
    "Republican Win 2016",
    "election",
    NA,
    "indicate whether Republican party win the county in 2016 election or not",
    "",
    NA,
    NA,
    "",
    "",
    "2016",
    "http://www.nytimes.com/elections/results/president"
  )
meta_seed <-
  metanize(
    meta_seed,
    "rep12_win",
    "elec_rep12_win",
    "Republican Win 2012",
    "election",
    NA,
    "indicate whether Republican party win the county in 2012 election or not",
    "",
    NA,
    NA,
    "",
    "",
    "2012",
    "http://www.nytimes.com/elections/2012/results/president.html"
  )
meta_seed <-
  metanize(
    meta_seed,
    "rep08_win",
    "elec_rep08_win",
    "Republican Win 2008",
    "election",
    NA,
    "indicate whether Republican party win the county in 2008 election or not",
    "",
    NA,
    NA,
    "",
    "",
    "2008",
    "http://www.nytimes.com/elections/2008/results/president/map.html"
  )

# AD003
# +----------------------------------+
# |      AGE, GENDER DATA 2015       |
# +----------------------------------+
# this data source does not contain any "," in numbers
# all columns imported as factors due to the first row (metadata)
# NA are marked as ***** or (X)
data_age_gender <-
  read.csv(mp(DATA_ORIGINAL_DIR, "age_gender_15.csv"),
           stringsAsFactors = FALSE)
data_age_gender <- data_age_gender[-1,]
data_age_gender <- data.frame(
  fips = as.numeric(data_age_gender$GEO.id2),
  sex_ratio = as.numeric(data_age_gender$HC01_EST_VC36),
  median_age = as.numeric(data_age_gender$HC01_EST_VC35),
  age_dependency_ratio = as.numeric(data_age_gender$HC01_EST_VC37)
)
data_seed$median_age <- NULL
data_seed <-
  merge(x = data_seed,
        y = data_age_gender,
        all.x = TRUE,
        by = "fips")
# update metadata
data_age_gender_source <-
  "https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_S0101&prodType=table"
meta_seed <-
  metanize(
    meta_seed,
    "sex_ratio",
    "demo_sex_ratio",
    "Sex Ratio",
    "demographics",
    NA,
    "number of males per 100 females",
    "@frac{@text{number of males in the population}}{@text{number of females in the population}}@times{100}",
    0,
    NA,
    "",
    "",
    "2015",
    data_age_gender_source
  )
meta_seed <-
  metanize(
    meta_seed,
    "age_dependency_ratio",
    "demo_age_dependency_ratio",
    "Age Dependency Ratio",
    "demographics",
    "%",
    "number of dependents, aged zero to 14 and over the age of 65, to the total population, aged 15 to 64",
    "@frac{@text{number of people not in working age}}{@text{number of people in working age}}@times{100}",
    0,
    NA,
    "",
    "",
    "2015",
    data_age_gender_source
  )
meta_seed <- metanize(
  meta_seed,
  "median_age",
  status = "",
  year = "2015",
  source = data_age_gender_source
)

# @AD004
# +----------------------------------+
# |     LIFE-EXPECTANCY DATA 2010    |
# +----------------------------------+
# this data source does not contain any "," in numbers
data_life_exp <-
  read.csv(mp(DATA_ORIGINAL_DIR, "life_exp_98-10.csv"),
           stringsAsFactors = FALSE)
data_life_exp <-
  data_life_exp[data_life_exp$Year == 2010, c(3, 5, 8)]
colnames(data_life_exp) <-
  c("fips", "male_life_exp", "female_life_exp")
data_seed <-
  merge(x = data_seed,
        y = data_life_exp,
        all.x = TRUE,
        by = "fips")
data_seed$life_expectancy <-
  (data_seed$male_life_exp * data_seed$sex_ratio
   + data_seed$female_life_exp * 100) / (data_seed$sex_ratio + 100)
data_seed$male_life_exp <- NULL
data_seed$female_life_exp <- NULL
# update metadata
meta_seed <-
  metanize(
    meta_seed,
    "life_expectancy",
    "healthcare_life_expectancy",
    "Life Expectancy",
    "healthcare",
    "year",
    "the average number of years a person born in a given country is expected to live if mortality rates at each age were to remain steady in the future",
    "",
    0,
    NA,
    "",
    "",
    "2010",
    "http://ghdx.healthdata.org/record/united-states-life-expectancy-estimates-county-1985-2010"
  )

# @AD005
# +----------------------------------+
# |       POPULATION DATA 2015       |
# +----------------------------------+
# this data source does not contain any "," in numbers
# all columns imported as factors due to the first row (metadata)
data_population <-
  read.csv(mp(DATA_ORIGINAL_DIR, "population_15.csv"),
           stringsAsFactors = FALSE)
data_population <- data_population[-1,]
data_population <- data.frame(
  fips = as.numeric(data_population$GEO.id2),
  Total.Population = as.numeric(data_population$HC01_VC03),
  voting_age_population = as.numeric(data_population$HC01_VC108)
)
data_seed$Total.Population <- NULL
data_seed <-
  merge(x = data_seed,
        y = data_population,
        all.x = TRUE,
        by = "fips")
# update metadata
data_population_source <-
  "https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_DP05&prodType=table"
meta_seed <-
  metanize(
    meta_seed,
    "Total.Population",
    "demo_population",
    "Population",
    "demographics",
    NA,
    "total population",
    "",
    0,
    NA,
    "",
    "",
    "2015",
    data_population_source
  )
meta_seed <-
  metanize(
    meta_seed,
    "voting_age_population",
    "demo_voting_age_population",
    "Voting Age Population",
    "demographics",
    NA,
    "number of people who are eligible to vote",
    "",
    0,
    NA,
    "can be used to compute voting participation rate",
    "",
    "2015",
    data_population_source
  )

# @AD006
# +----------------------------------+
# |           VOTING POWER           |
# +----------------------------------+
# we cannot use any result before the 2016 election so we must you total12
data_electoral_vote <-
  read.csv(mp(DATA_ORIGINAL_DIR, "electoral_votes_dist_00.csv"),
           stringsAsFactors = FALSE)
data_state_votes <-
  aggregate(
    x = data_seed$total12,
    by = list(data_seed$statecode_prev),
    FUN = "sum"
  )
data_electoral_vote <-
  merge(
    x = data_state_votes,
    y = data_electoral_vote,
    all.x = TRUE,
    by.x = "Group.1",
    by.y = "fips"
  )
data_electoral_vote$voting_power_rate <-
  data_electoral_vote$electaral_votes / data_electoral_vote$x
data_electoral_vote <-
  data.frame(statecode_prev = data_electoral_vote$Group.1,
             voting_power = data_electoral_vote$voting_power_rate)
data_seed <-
  merge(x = data_seed,
        y = data_electoral_vote,
        all.x = TRUE,
        by = "statecode_prev")
data_seed$voting_power <- data_seed$voting_power * data_seed$total12
# update metadata
meta_seed <-
  metanize(
    meta_seed,
    "voting_power",
    "elec_voting_power",
    "Voting Power",
    "election",
    NA,
    "the power of each vote that counts towards the final vote of each state",
    "@frac{@text{state electoral votes } @times @text{ total number of votes of the county}}{@text{total number of votes of the state}}",
    0,
    NA,
    "",
    "",
    "2000-2016",
    "http://psephos.adam-carr.net/countries/u/usa/pres/2000.txt"
  )

# AD007
# +----------------------------------+
# |     VOTING PARTICIPATION RATE    |
# +----------------------------------+
data_seed$voting_participation <- data_seed$voting_age_population
data_seed$voting_participation <-
  data_seed$total12 / data_seed$voting_participation * 100
meta_seed <-
  metanize(
    meta_seed,
    "voting_participation",
    "elec_voting_participation",
    "Voting Participation Rate",
    "election",
    "%",
    "percentage of population participated in the voting",
    "@frac{@text{total number of votes in 2012}}{@text{total voting-population 2015}}@times{100}",
    0,
    100,
    "cannot use 2016 results to calculate this because we are trying to predict the result",
    "",
    "2012",
    "http://www.nytimes.com/elections/2012/results/president.html"
  )

# AD008
# +----------------------------------+
# |       GEOGRAPHIC DATA 2010       |
# +----------------------------------+
# this data source does not contain any "," in numbers
# all columns imported as factors due to the first row (metadata)
# NA are marked as ***** or (X)
data_geog <-
  read.csv(mp(DATA_ORIGINAL_DIR, "geog_10.csv"), stringsAsFactors = FALSE)
data_geog <- data_geog[-1,]
data_geog <- data.frame(fips = as.numeric(data_geog$GEO.id2),
                        area = as.numeric(data_geog$VD067))
data_seed <- merge(x = data_seed,
                   y = data_geog,
                   all.x = TRUE,
                   by = "fips")
data_seed$population_density <-
  data_seed$Total.Population / data_seed$area * 1000000
data_seed$area <- NULL
# update metadata
meta_seed <-
  metanize(
    meta_seed,
    "population_density",
    "demo_population_density",
    "Population Density",
    "demographics",
    "per km^2",
    "population density",
    "@frac{@text{total population}}{@text{total land area}}",
    0,
    NA,
    "",
    "",
    "2015",
    "http://factfinder.census.gov/bkmk/table/1.0/en/DEC/10_SF1/G001/0100000US.05000.003"
  )

# +----------------------------------+
# |               STATE              |
# +----------------------------------+
# convert state FIPS code to factor/nominal
data_seed$statecode_prev <- as.factor(data_seed$statecode_prev)

############################################################
##                      UPDATE                            ##
############################################################

# UP001
# +----------------------------------+
# |       HEALTHCARE DATA 2016       |
# +----------------------------------+
# this data source contains "," in numbers
# NA are marked as "NA" or ""
data_health <-
  read.csv(
    mp(DATA_ORIGINAL_DIR, "health_16.csv"),
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )
# organize data
data_health <- data.frame(
  fips = paste(
    data_health$STATECODE,
    sprintf("%03d", data_health$COUNTYCODE),
    sep = ""
  ),
  Poor.physical.health.days = as.numeric(gsub(
    ",", "", data_health$Poor.physical.health.days.Value
  )),
  Poor.mental.health.days = as.numeric(gsub(
    ",", "", data_health$Poor.mental.health.days.Value
  )),
  Low.birthweight = as.numeric(gsub(
    ",", "", data_health$Low.birthweight.Value
  )),
  Teen.births = as.numeric(gsub(",", "", data_health$Teen.births.Value)),
  Children.in.single.parent.households = as.numeric(
    gsub(
      ",",
      "",
      data_health$Children.in.single.parent.households.Value
    )
  ),
  Adult.smoking = as.numeric(gsub(",", "", data_health$Adult.smoking.Value)),
  Adult.obesity = as.numeric(gsub(",", "", data_health$Adult.obesity.Value)),
  Diabetes = as.numeric(gsub(",", "", data_health$Diabetes.Value)),
  Sexually.transmitted.infections = as.numeric(
    gsub(",", "", data_health$Sexually.transmitted.infections.Value)
  ),
  HIV.prevalence.rate = as.numeric(gsub(
    ",", "", data_health$HIV.prevalence.rate.Value
  )),
  Uninsured = as.numeric(gsub(",", "", data_health$Uninsured.Value)),
  Unemployment = as.numeric(gsub(",", "", data_health$Unemployment.Value)),
  Violent.crime = as.numeric(gsub(",", "", data_health$Violent.crime.Value)),
  Homicide.rate = as.numeric(gsub(",", "", data_health$Homicide.rate.Value)),
  Injury.deaths = as.numeric(gsub(",", "", data_health$Injury.deaths.Value)),
  Infant.mortality = as.numeric(gsub(
    ",", "", data_health$Infant.mortality.Value
  ))
)
data_health_updates <- c(
  "Poor.physical.health.days",
  "Poor.mental.health.days",
  "Low.birthweight",
  "Teen.births",
  "Children.in.single.parent.households",
  "Adult.smoking",
  "Adult.obesity",
  "Diabetes",
  "Sexually.transmitted.infections",
  "HIV.prevalence.rate",
  "Uninsured",
  "Unemployment",
  "Violent.crime",
  "Homicide.rate",
  "Injury.deaths",
  "Infant.mortality"
)
# remove outdated data from MAIN
data_seed <- nullify(data_seed, data_health_updates)
# merge to MAIN
data_seed <-
  merge(x = data_seed,
        y = data_health,
        all.x = TRUE,
        by = "fips")
# update metadata
meta_seed <-
  metanize_v(
    meta_seed,
    data_health_updates,
    status = "",
    year = "2016",
    source = "http://www.countyhealthrankings.org/rankings/data"
  )

# UP002
# +----------------------------------+
# |     HUMAN-DEV DATA 2009-2010     |
# +----------------------------------+
# this data source has been cleaned up since the original dataset is in excel format
data_human_dev <-
  read.csv(
    mp(DATA_ORIGINAL_DIR, "human_dev_09-10.csv"),
    na.strings = c("-", "", "NA"),
    stringsAsFactors = FALSE
  )
# organize data
data_human_dev <- data.frame(
  fips = as.numeric(data_human_dev$fips),
  year = as.numeric(data_human_dev$year),
  Median.Earnings.2010.dollars = data_human_dev$Median.Earnings.2010.dollars,
  Less.Than.High.School = data_human_dev$Less.Than.High.School,
  At.Least.Bachelor.s.Degree = data_human_dev$At.Least.Bachelor.s.Degree,
  Graduate.Degree = data_human_dev$Graduate.Degree,
  School.Enrollment = data_human_dev$School.Enrollment,
  White.not.Latino.Population = data_human_dev$White.not.Latino.Population,
  African.American.Population = data_human_dev$African.American.Population,
  Native.American.Population = data_human_dev$Native.American.Population,
  Asian.American.Population = data_human_dev$Asian.American.Population,
  Population.some.other.race.or.races = data_human_dev$Population.some.other.race.or.races,
  Latino.Population = data_human_dev$Latino.Population,
  Children.Under.6.Living.in.Poverty = data_human_dev$Children.Under.6.Living.in.Poverty,
  Adults.65.and.Older.Living.in.Poverty = data_human_dev$Adults.65.and.Older.Living.in.Poverty,
  Preschool.Enrollment.Ratio.enrolled.ages.3.and.4 = data_human_dev$Preschool.Enrollment.Ratio.enrolled.ages.3.and.4,
  Poverty.Rate.below.federal.poverty.threshold = data_human_dev$Poverty.Rate.below.federal.poverty.threshold,
  Gini.Coefficient = data_human_dev$Gini.Coefficient,
  Child.Poverty.living.in.families.below.the.poverty.line = data_human_dev$Child.Poverty.living.in.families.below.the.poverty.line,
  Management.professional.and.related.occupations = data_human_dev$Management.professional.and.related.occupations,
  Service.occupations = data_human_dev$Service.occupations,
  Sales.and.office.occupations = data_human_dev$Sales.and.office.occupations,
  Farming.fishing.and.forestry.occupations = data_human_dev$Farming.fishing.and.forestry.occupations,
  Construction.extraction.maintenance.and.repair.occupations = data_human_dev$Construction.extraction.maintenance.and.repair.occupations,
  Production.transportation.and.material.moving.occupations = data_human_dev$Production.transportation.and.material.moving.occupations
)
# extract data for only 2010
data_human_dev <- data_human_dev[data_human_dev$year == 2010,-2]
# remove outdated data from MAIN
data_human_dev_updates <- c(
  "Median.Earnings.2010.dollars",
  "Less.Than.High.School",
  "At.Least.Bachelor.s.Degree",
  "Graduate.Degree",
  "School.Enrollment",
  "White.not.Latino.Population",
  "African.American.Population",
  "Native.American.Population",
  "Asian.American.Population",
  "Population.some.other.race.or.races",
  "Latino.Population",
  "Children.Under.6.Living.in.Poverty",
  "Adults.65.and.Older.Living.in.Poverty",
  "Preschool.Enrollment.Ratio.enrolled.ages.3.and.4",
  "Poverty.Rate.below.federal.poverty.threshold",
  "Gini.Coefficient",
  "Child.Poverty.living.in.families.below.the.poverty.line",
  "Management.professional.and.related.occupations",
  "Service.occupations",
  "Sales.and.office.occupations",
  "Farming.fishing.and.forestry.occupations",
  "Construction.extraction.maintenance.and.repair.occupations",
  "Production.transportation.and.material.moving.occupations"
)
data_seed <- nullify(data_seed, data_human_dev_updates)
data_seed <-
  merge(x = data_seed,
        y = data_human_dev,
        all.x = TRUE,
        by = "fips")
# update metadata
meta_seed <-
  metanize_v(
    meta_seed,
    data_human_dev_updates,
    remark = "",
    status = "",
    year = "2010",
    source = "http://www.measureofamerica.org/download-agreement/"
  )

############################################################
##                      ORGANIZE                          ##
############################################################

order <- c(
  # administration
  "fips",
  "name_16",
  "statecode_prev",
  
  # election
  "rep16_win",
  "rep12_win",
  "rep08_win",
  "votes16_trumpd",
  "rep12",
  "rep08",
  "votes16_clintonh",
  "dem12",
  "dem08",
  "votes16_others",
  "other12",
  "other08",
  "total16",
  "total12",
  "total08",
  "rep16_frac",
  "rep12_frac",
  "rep08_frac",
  "dem16_frac",
  "dem12_frac",
  "dem08_frac",
  "other16_frac",
  "other12_frac",
  "other08_frac",
  
  "est_votes_remaining",
  "voting_age_population",
  "voting_participation",
  "voting_power",
  
  # demographics
  "Total.Population",
  "population_density",
  "sex_ratio",
  "age_dependency_ratio",
  "median_age",
  
  # race and ethnicity
  "White.not.Latino.Population",
  "African.American.Population",
  "Native.American.Population",
  "Asian.American.Population",
  "Population.some.other.race.or.races",
  "Latino.Population",
  "SIRE_homogeneity",
  
  # finance
  "Gini.Coefficient",
  "Median.Earnings.2010.dollars",
  "Children.Under.6.Living.in.Poverty",
  "Child.Poverty.living.in.families.below.the.poverty.line",
  "Adults.65.and.Older.Living.in.Poverty",
  "Poverty.Rate.below.federal.poverty.threshold",
  
  # occupation
  "Unemployment",
  "Management.professional.and.related.occupations",
  "Service.occupations",
  "Sales.and.office.occupations",
  "Farming.fishing.and.forestry.occupations",
  "Construction.extraction.maintenance.and.repair.occupations",
  "Production.transportation.and.material.moving.occupations",
  
  # education
  "School.Enrollment",
  "Preschool.Enrollment.Ratio.enrolled.ages.3.and.4",
  "Less.Than.High.School",
  "At.Least.Bachelor.s.Degree",
  "Graduate.Degree",
  
  # healthcare
  "life_expectancy",
  "Infant.mortality",
  "Injury.deaths",
  "Poor.physical.health.days",
  "Poor.mental.health.days",
  "Low.birthweight",
  "Teen.births",
  "Children.in.single.parent.households",
  "Adult.smoking",
  "Adult.obesity",
  "Diabetes",
  "Sexually.transmitted.infections",
  "HIV.prevalence.rate",
  "Uninsured",
  
  # crime
  "Violent.crime",
  "Homicide.rate",
  
  # weather
  "winter_PRCP",
  "winter_TAVG"
)

data_seed <- data_seed[order]
meta_seed <- meta_seed[match(order, meta_seed$name),]

############################################################
##                        SUMMARY                         ##
############################################################

data_summary <- summarize(data_seed)

############################################################
##                        EXPORT                          ##
############################################################

write.csv(
  data_seed,
  file = mp(DATA_PRE_PROCESSED_DIR, "data.csv"),
  row.names = FALSE
)
write.csv(
  meta_seed,
  file = mp(DATA_PRE_PROCESSED_DIR, "meta.csv"),
  row.names = FALSE
)
write.csv(
  data_summary$continuous,
  file = mp(RES_PRE_PROCESSED_DIR, "summary_continuous.csv"),
  row.names = FALSE
)
write.csv(
  data_summary$categorical,
  file = mp(RES_PRE_PROCESSED_DIR, "summary_categorical.csv"),
  row.names = FALSE
)

############################################################
##                       TERMINATE                        ##
############################################################

log.write("end data_pre_process script")
reset.work_space()