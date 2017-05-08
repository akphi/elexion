############################################################
##                      DESCRIPTION                       ##
############################################################

# prepare the dataset by crude feature selection and handling
# of missing data

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
source("misc.R")

############################################################
##                      INITIALIZE                        ##
############################################################

log.write("start data_prepare script")

############################################################
##                          LOAD                          ##
############################################################

data_orig <-
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "data.csv"), stringsAsFactors = FALSE)
meta_orig <-
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "meta.csv"), stringsAsFactors = FALSE)
# set MAIN
data_seed <- data_orig
meta_seed <- meta_orig

############################################################
##              HANDLING POTENTIAL OUTLIERS               ##
############################################################

# @HO001
# +----------------------------------+
# |         ELECTION RESULT          |
# +----------------------------------+
# based on the estimated number of votes remaining at a county
# and the result from past election, we can check if the current
# result of the county is reliable or not

# discard county where all precincts have reported
data_temp <- data_seed[data_seed$est_votes_remaining != 0,]

# calculate the potential number of votes remaining based on voting_age_population
data_temp$potential_rep_increase <-
  data_temp$est_votes_remaining * (
    data_temp$total16 / (
      data_temp$voting_age_population - data_temp$est_votes_remaining
    )
  )

# calculate the potential number of votes for major parties
data_temp$potential_rep_increase <-
  data_temp$potential_rep_increase * (data_temp$rep16_frac + data_temp$dem16_frac)

# calculate the potential number of votes gainable by republican party based on 2012 result
data_temp$potential_rep_increase <-
  data_temp$potential_rep_increase * (data_temp$rep12_frac - data_temp$dem12_frac)

# calculate the current lead of republican party's vote
data_temp$current_rep_lead <-
  data_temp$votes16_trumpd - data_temp$votes16_clintonh

# calculate the predicted lead of republican party after adding the potential increase
data_temp$predicted_rep_lead <-
  data_temp$potential_rep_increase + data_temp$current_rep_lead

# check if the winning party has changed
data_temp$flipable <-
  ifelse(data_temp$current_rep_lead / data_temp$predicted_rep_lead > 0,
         FALSE,
         TRUE)

# remove the counties with uncertain result
log.write(
  paste(
    "Flipable county detected where instance at index (",
    paste(which(data_seed$fips %in% data_temp[data_temp$flipable, ]$fips), collapse = ", "),
    ") with FIPS code (",
    paste(data_seed[data_seed$fips %in% data_temp[data_temp$flipable, ]$fips, ]$fips, collapse = ", "),
    ") having unreliable voting result and hence will be discarded.",
    sep = ""
  )
)
data_seed <- data_seed[data_seed$fips %notin% data_temp[data_temp$flipable,]$fips,]

# @HO002
# +----------------------------------+
# |     OTHER OUTLIERS DETECTION     |
# +----------------------------------+
# perform clamp transformation on the dataset based on the metadata
data_seed <- clamp_bound(data_seed, meta_seed)

############################################################
##                         TRIM                           ##
############################################################

to_drop <- c(
  # @TR011
  #not needed
  "name_16",
  
  # TR012
  # not needed
  "dem08",
  "rep08",
  "total08",
  "other08",
  "dem12",
  "rep12",
  "total12",
  "other12",
  "votes16_trumpd",
  "votes16_clintonh",
  "total16",
  "rep16_frac",
  "dem16_frac",
  "rep12_frac",
  "rep08_frac",
  "dem12_frac",
  "dem08_frac",
  "other12_frac",
  "other08_frac",
  "votes16_others",
  "other16_frac",
  
  # TR013
  # not needed
  "rep08_win",
  "Total.Population",

  # @TR014
  # used for identifying outliers
  "est_votes_remaining",
  "voting_age_population"
)

# trim unnecessary features
meta_seed <- meta_seed[!(meta_seed$name %in% to_drop), ]
data_seed <- data_seed[, !(names(data_seed) %in% to_drop)]

############################################################
##                        MODIFY                          ##
############################################################

# +----------------------------------+
# |               STATE              |
# +----------------------------------+
# convert state FIPS code to factor/nominal
data_seed$statecode_prev <- as.factor(data_seed$statecode_prev)

############################################################
#         MISSING-DATA: AVERAGE DIFFERENCE VECTOR         ##
############################################################

# @HM001
# +----------------------------------+
# |       HEALTHCARE 2014-2016       |
# +----------------------------------+
data_health_14 <-
  read.csv(
    mp(DATA_ORIGINAL_DIR, "health_14.csv"),
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )
data_health_15 <-
  read.csv(
    mp(DATA_ORIGINAL_DIR, "health_15.csv"),
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )
data_health_16 <-
  read.csv(
    mp(DATA_ORIGINAL_DIR, "health_16.csv"),
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )
# select features with missing data
data_health.organize <- function(data_health) {
  result <- data.frame(
    fips = paste(
      data_health$STATECODE,
      sprintf("%03d", data_health$COUNTYCODE),
      sep = ""
    ),
    Low.birthweight = as.numeric(gsub(
      ",", "", data_health$Low.birthweight.Value
    )),
    Teen.births = as.numeric(gsub(",", "", data_health$Teen.births.Value)),
    Sexually.transmitted.infections = as.numeric(
      gsub(",", "", data_health$Sexually.transmitted.infections.Value)
    ),
    HIV.prevalence.rate = as.numeric(gsub(
      ",", "", data_health$HIV.prevalence.rate.Value
    )),
    Violent.crime = as.numeric(gsub(
      ",", "", data_health$Violent.crime.Value
    )),
    Homicide.rate = as.numeric(gsub(
      ",", "", data_health$Homicide.rate.Value
    )),
    Injury.deaths = as.numeric(gsub(
      ",", "", data_health$Injury.deaths.Value
    )),
    Infant.mortality = as.numeric(gsub(
      ",", "", data_health$Infant.mortality.Value
    ))
  )
}
data_health_14 <- data_health.organize(data_health_14)
data_health_15 <- data_health.organize(data_health_15)
data_health_16 <- data_health.organize(data_health_16)
# update data health
data_health_updates <- c(
  "Low.birthweight",
  "Teen.births",
  "Sexually.transmitted.infections",
  "HIV.prevalence.rate",
  "Violent.crime",
  "Homicide.rate",
  "Injury.deaths",
  "Infant.mortality"
)
data_health <-
  combine.prediction_by_diff(
    data_health_16,
    data_health_15,
    data_health_14,
    template = data.frame(fips = data_seed$fips, state = data_seed$statecode_prev),
    attrList = data_health_updates
  )
# merge to MAIN
data_health$state <- NULL
data_seed <- nullify(data_seed, data_health_updates)
data_seed <-
  merge(x = data_seed,
        y = data_health,
        all.x = TRUE,
        by = "fips")
# update metadata
meta_seed <-
  metanize_v(meta_seed, data_health_updates, year = "2014-2016")

# @HM002
# +----------------------------------+
# |       HUMAN-DEV 2009-2010        |
# +----------------------------------+
data_human_dev <-
  read.csv(
    mp(DATA_ORIGINAL_DIR, "human_dev_09-10.csv"),
    na.strings = c("-", "", "NA"),
    stringsAsFactors = FALSE
  )
# select features with missing data
data_human_dev <- data.frame(
  fips = as.numeric(data_human_dev$fips),
  year = as.numeric(data_human_dev$year),
  Less.Than.High.School = data_human_dev$Less.Than.High.School,
  At.Least.Bachelor.s.Degree = data_human_dev$At.Least.Bachelor.s.Degree,
  Graduate.Degree = data_human_dev$Graduate.Degree,
  School.Enrollment = data_human_dev$School.Enrollment,
  Adults.65.and.Older.Living.in.Poverty = data_human_dev$Adults.65.and.Older.Living.in.Poverty,
  Preschool.Enrollment.Ratio.enrolled.ages.3.and.4 = data_human_dev$Preschool.Enrollment.Ratio.enrolled.ages.3.and.4,
  Median.Earnings.2010.dollars = data_human_dev$Median.Earnings.2010.dollars
)
data_human_dev_09 <- data_human_dev[data_human_dev$year == 2009, -2]
data_human_dev_10 <- data_human_dev[data_human_dev$year == 2010, -2]
data_human_dev_updates <- c(
  "Less.Than.High.School",
  "At.Least.Bachelor.s.Degree",
  "Graduate.Degree",
  "School.Enrollment",
  "Adults.65.and.Older.Living.in.Poverty",
  "Preschool.Enrollment.Ratio.enrolled.ages.3.and.4",
  "Median.Earnings.2010.dollars"
)
data_human_dev <-
  combine.prediction_by_diff(
    data_human_dev_10,
    data_human_dev_09,
    template = data.frame(fips = data_seed$fips, state = data_seed$statecode_prev),
    attrList = data_human_dev_updates
  )
# merge to MAIN
data_human_dev$state <- NULL
data_seed <- nullify(data_seed, data_human_dev_updates)
data_seed <-
  merge(x = data_seed,
        y = data_human_dev,
        all.x = TRUE,
        by = "fips")
# update metadata
meta_seed <-
  metanize_v(meta_seed, data_human_dev_updates, year = "2009-2010")

############################################################
#         MISSING-DATA: AVERAGE ADJACENT COUNTIES         ##
############################################################

# @HM003
# fill missing values in weather data based on data of 
# adjacent counties
data_county_adjacency <-
  read.csv(mp(DATA_MISC_DIR, "county_adjacency.csv"))
data_seed <- compute.average.from_source(
  target = data_seed,
  source = merge(
    x = data_county_adjacency,
    y = data_seed,
    all.x = TRUE,
    by.x = "neighbor_code",
    by.y = "fips"
  ),
  by.target = "fips",
  by.source = "code",
  names = c("winter_PRCP",
            "winter_TAVG")
)

############################################################
#        MISSING-DATA: AVERAGE COUNTIES WITHIN STATE      ##
############################################################

# @HM004
# fill missing values by take average of the data of all counties
# within the same state
data_seed <- compute.average.from_source(
  target = data_seed,
  source = data_seed,
  by.target = "statecode_prev",
  by.source = "statecode_prev",
  names = c(
    "winter_PRCP",
    "winter_TAVG",
    "Low.birthweight",
    "Teen.births",
    "Sexually.transmitted.infections",
    "HIV.prevalence.rate",
    "Violent.crime",
    "Homicide.rate",
    "Injury.deaths",
    "Infant.mortality",
    "Adults.65.and.Older.Living.in.Poverty",
    "Preschool.Enrollment.Ratio.enrolled.ages.3.and.4",
    "Median.Earnings.2010.dollars"
  )
)

############################################################
#          MISSING-DATA: AVERAGE ADJACENT STATES          ##
############################################################

# @HM005
# fill missing values by taking average of adjacent states
data_state_adjacency <-
  read.csv(mp(DATA_MISC_DIR, "state_adjacency.csv"))
data_seed <- compute.average.from_source(
  target = data_seed,
  source = merge(
    x = data_state_adjacency,
    y = data_seed,
    all.x = TRUE,
    by.x = "neighbor_code",
    by.y = "statecode_prev"
  ),
  by.target = "statecode_prev",
  by.source = "code",
  names = c("winter_PRCP",
            "winter_TAVG",
            "HIV.prevalence.rate")
)

############################################################
##                      ORGANIZE                          ##
############################################################

order <- c(
  # target
  "rep16_win",
  
  # administration
  "statecode_prev",
  
  # election
  "rep12_win",
  "voting_participation",
  "voting_power",
  
  # demographics
  "sex_ratio",
  "population_density",
  "median_age",
  "age_dependency_ratio",
  
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
  "Poverty.Rate.below.federal.poverty.threshold",
  "Adults.65.and.Older.Living.in.Poverty",
  
  # occupation
  "Unemployment",
  "Management.professional.and.related.occupations",
  "Service.occupations",
  "Sales.and.office.occupations",
  "Farming.fishing.and.forestry.occupations",
  "Construction.extraction.maintenance.and.repair.occupations",
  "Production.transportation.and.material.moving.occupations",
  
  # education
  "Less.Than.High.School",
  "At.Least.Bachelor.s.Degree",
  "Graduate.Degree",
  "School.Enrollment",
  "Preschool.Enrollment.Ratio.enrolled.ages.3.and.4",
  
  # healthcare
  "Uninsured",
  "life_expectancy",
  "Infant.mortality",
  "HIV.prevalence.rate",
  "Sexually.transmitted.infections",
  "Poor.physical.health.days",
  "Poor.mental.health.days",
  "Children.in.single.parent.households",
  "Adult.smoking",
  "Adult.obesity",
  "Diabetes",
  "Low.birthweight",
  "Teen.births",
  "Injury.deaths",
  
  # crime
  "Violent.crime",
  "Homicide.rate",
  
  # weather
  "winter_PRCP",
  "winter_TAVG"
)

data_seed <- data_seed[order]
meta_seed <- meta_seed[match(order, meta_seed$name), ]
names(data_seed) <- meta_seed$var_name

############################################################
##                        SUMMARY                         ##
############################################################

data_summary <- summarize(data_seed)

############################################################
##                        EXPORT                          ##
############################################################

write.csv(data_seed,
          file = mp(DATA_PREPARED_DIR, "data.csv"),
          row.names = FALSE)
write.csv(meta_seed,
          file = mp(DATA_PREPARED_DIR, "meta.csv"),
          row.names = FALSE)
write.csv(
  data_summary$continuous,
  file = mp(RES_PREPARED_DIR, "summary_continuous.csv"),
  row.names = FALSE
)
write.csv(
  data_summary$categorical,
  file = mp(RES_PREPARED_DIR, "summary_categorical.csv"),
  row.names = FALSE
)

############################################################
##                       TERMINATE                        ##
############################################################

log.write("end data_prepare script")
reset.work_space()