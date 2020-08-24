

# source("scripts/functions.R")

  library(readxl)
  library(tidyverse)

# ECRF data --------------------------------------------------------------------
# The ECRF contains a multitude of forms. This brings them all into a list.

# Get the names of all the csv files with the data
  sheets <- paste0(
    "data/data_april_2018/",
    list.files("data/data_april_2018/", pattern = ".csv")
    )

# Import them all into a list
  import <- function(x){
    forms <- list()
    for(i in 1:length(x)){
      df <- read_csv(x[i], skip = 1)
      df <- df[rowSums(is.na(df)) != ncol(df),] # Remove blank rows
      forms[[i]] <- df
    }
    return(forms)
  }

  forms <- import(sheets)

# Remove the last column if it's name starts with "X"

  forms <- lapply(forms, function(x) x <- x[which(!grepl("X.*", names(x)))])

# Convert any wide format forms to long ----------------------------------------
# Create a function to turn the wide datasets into long
  restack <- function(data, ...){
    a <- which(grepl("CENTRE", names(data))) # Columns with CENTRE
    k <- a[2] - 1 # Up to the second occurence of CENTRE
    df <- data[1:k] # Take those data
    names <- names(df)
    for(i in 1:((length(data) / k) - 1)){ # Add the rest, k columns at a time
      e <- data[c((i * k + 1):((i + 1) * k))]
      names(e) <- names
      df <- rbind(df, e)
    }
    return(df)
  }

# Which forms appear to need restacking?
# sapply(forms, names)
# Look for col names containing CENTRE. More than 1 means it needs restacking

  a <- sapply(forms, function(x) length(names(x)[grepl("CENTRE", names(x))]))

# These need to be restacked

  for(i in which(a != 1)){
    forms[[i]] <- restack(forms[[i]])
  }
  rm(a)

# Remove the blank rows
  for(i in 1:length(forms)){
    x <- forms[[i]]
    if(length(x) > 0){
      x <- x[rowSums(is.na(x[5:length(x)])) != ncol(x[5:length(x)]), ]
    }
    forms[[i]] <- x
  }
  rm(x)

# Make sure the first 4 cols are of the same type (all numeric)

  for(i in 1:length(forms)){
    forms[[i]][c(1:4)] <- lapply(forms[[i]][c(1:4)], as.numeric)

  }

# Quick-tidy the names
  for(i in 1:length(forms)){
    names(forms[[i]]) <- tolower(make.names(names(forms[[i]])))

  }

# Create the names objects -----------------------------------------------------
# This is the basic info about each form that is used to link to the dictionary
  form_numbers <- read_excel("data/forms_list.xlsx")
  names(form_numbers) <- c("code", "number", "file", "name")
  form_numbers <- as_data_frame(lapply(form_numbers, tolower))

# with(form_numbers, paste(code, number, name))

# Factor levels from the dictionary --------------------------------------------
  codes <- readxl::read_excel("data/Data_Dictionary.xlsx",
                              sheet = 5)

  names(codes) <- c("list", "value", "label", "rank")

  codes <- arrange(codes, list, value)

  codes <- lapply(codes, tolower)

#///////////////////////////////////////////////////////////////////////////////



# Forms list -------------------------------------------------------------------
# 1  scrv 1  screen visit
# 2  gmut 2  cf genotyping
# 3  incl 3  inclusion_exclusion
# 4  pee  4  pulmonary exacerbation evaluation
# 5  demo 5  demographic_data
# 6  ahis 6  allergy
# 7  phe  7  physical_exam
# 8  vsg  8  vital signs
# 9  msh  9  medical_history_status
# 10 ccm  10 con meds
# 11 labo 12 blood_labs
# 12 dail 13 cf_symptom_diary
# 13 ystd 14 year1_study_data
# 14 der  17 sae1
# 15 sbl  18 sibling form
# 16 ae   19 adverse event form
# 17 cfq3 22 cfqr
# 18 rand 15 randomization form
# 19 rsam 16 report sample form
# 20 bas  20 belfast analysis form
# 21 rcm  21 results consensus panel
# 22 sae  23 sae
# 23 mbt  24 microbiome treatment
# 24 dexc 25 day 0 exacerbation criteria
# 25 scrf 26 screen failure
# 26 unsc 27 unschedule vist inclusion criteria
# 27 esq  28 end of study

#///////////////////////////////////////////////////////////////////////////////



# Randomization/Arms------------------------------------------------------------
# 0 = active, 1 = control
  arms <- read_excel(
    "data/CFMATTERS_randomisation_list_unblinded__04MAY2018.xlsx",
    sheet = 2
    )

  names(arms) <- tolower(names(arms))
  names(arms) <- gsub(" ", "_", names(arms))
  names(arms)[9] <- "centre"
  names(arms)[6] <- "arm"
  names(arms)[10] <- "patient"

  arms$arm <- factor(arms$arm, labels = c("Active", "Control"))
  arms$arm <- relevel(arms$arm, ref = "Control")

  arms$stratification <- factor(arms$stratification)

  arms <- arms[!(arms$centre == 7  & arms$patient == 9), ] #7-9 was a screen fail


#///////////////////////////////////////////////////////////////////////////////

# Conmeds ----------------------------------------------------------------------

  con <- forms[[10]]

  con <- full_join(con, arms, by = c("patient", "centre"))

  names(con)[10] <- "route"

  iva <- filter(con, visite == 0)

  iva$ivacaftor[grepl("vacaft", iva$generic.name)] <- "Yes"
  iva$ivacaftor[grepl("alydec", iva$generic.name)] <- "Yes"

  iva <- filter(iva, ivacaftor == "Yes")
# nrow(distinct(iva, centre, patient))

  arms <- full_join(select(iva, patient, centre, ivacaftor),
                    arms, by = c("patient", "centre"))

  arms$ivacaftor[is.na(arms$ivacaftor)] <- "No"
  arms$ivacaftor <- factor(arms$ivacaftor)


#///////////////////////////////////////////////////////////////////////////////



# VSG --------------------------------------------------------------------------
  vsg <- forms[[8]]

# Merge in tx arm
  vsg <- full_join(vsg, arms, by = c("patient", "centre"))

  names(vsg)[19] <- "fev1.pct.pred"

# Restrict to baseline - +28 days visit.
  vsg$visite2 <- ifelse(vsg$visite %in% c(0, 1, 2, 3, 4, 5), vsg$visite, NA)

  v <- c("Enrollment", "Day 0", "+7 days", "+14 days", "+28 days", "3 months")
  vsg$visite2 <- factor(vsg$visite2, labels = v); rm(v)

# Marker for all three kinds of visits

  vsg <- mutate(vsg, visit_type = case_when(
    visite == 0 ~ "Enrollment",
    visite == 1 ~ "Day 0",
    visite %in% c(0, 1, 2, 3, 4, 5) ~ "In-trial",
    visite %in% c(10:17) ~ "Unscheduled",
    visite > 100 ~ "Pre-trial"))

  vsg$visit_type <- factor(
    vsg$visit_type,
    levels = c("Enrollment", "Pre-trial", "Day 0", "Unscheduled", "In-trial")
  )

  vsg$datvis <- as.POSIXct(vsg$datvis, format = "%d/%m/%y")
  vsg$id <- with(vsg, interaction(centre, patient))

  vsg$days_before <- NA
  vsg$sputum[is.na(vsg$sputum)] <- 0
  for(i in unique(vsg$id)){
    df <- filter(vsg, id == i)
    if(nrow(df) > 1){
      for(j in 2:nrow(df)){
        if(df$sputum[j] == 1 & df$visite[j - 1] %in% c(1, 10:17, 101:112)){
          vsg$days_before[vsg$id == i & vsg$visite == df$visite[j]] <-
            difftime(df$datvis[j], df$datvis[j - 1])
        }
      }
    }
  }

  vsg$days_after <- NA
  for(i in unique(vsg$id)){
    df <- filter(vsg, id == i)
    if(nrow(df) > 1){
      for(j in 1:(nrow(df) - 1)){
        if(df$sputum[j] == 1 & df$visite[j + 1] %in% c(1, 10:17, 101:112)){
          vsg$days_after[vsg$id == i & vsg$visite == df$visite[j]] <-
            difftime(df$datvis[j + 1], df$datvis[j])
        }
      }
    }
  }

  vsg <- arrange(vsg, centre, patient, datvis) %>%
    select(centre, patient, datvis, visite, sputum, days_before, days_after,
           everything())

# Visite
# 0 - Enroll
# 1 - Day 0
# 2 - 7
# 3 - 14
# 4 - 28
# 5 - mo 3

# 10+ are ineligible
# 101+ are post-enrollment but pre-exacerbation

# Convert to numeric to get rid of some "ND" values
  vsg$fev1.pct.pred2 <- as.numeric(vsg$fev1.pct.pred)

# Create a variable that gives the most recent prior, valid, FEV value
  vsg$recent_fev <- NA
  for(i in unique(vsg$id)){
    df <- filter(vsg, id == i & visite %in% c(0, 101:112) &
                   !is.na(fev1.pct.pred2))
      recent_vis <- max(df$visite)
      recent_fev <- filter(df, visite == recent_vis)$fev1.pct.pred2
      vsg$recent_fev[vsg$visite == 1 & vsg$id == i] <- recent_fev
  }

  vsg <- filter(vsg, visite == 1) %>%
    select(id, recent_fev) %>%
    distinct() %>%
    full_join(select(vsg, -recent_fev), by = "id")


  vsg <- filter(vsg, visite == 0) %>%
    select(id, baseline_fev = fev1.pct.pred2) %>%
    distinct() %>%
    full_join(vsg, by = "id")

# ggplot(vsg, aes(x = baseline_fev, y = recent_fev)) +
#   geom_point()w


# If they have a Day 0 row in the vsg file, then they had an eligible
# exacerbation and thus entered the trial.
  vsg <- filter(vsg, visite2 == "Day 0") %>%
    select(patient, centre) %>%
    mutate(intrial = "Yes") %>%
    full_join(vsg, by = c("centre", "patient"))

  vsg$intrial[is.na(vsg$intrial)] <- "No"

  vsg$intrial <- factor(vsg$intrial)

  vsg$datvis <- as.POSIXct(vsg$datvis, format = "%d/%m/%y")

  vsg <- vsg[!(vsg$centre == 2  & vsg$patient == 11), ] #2-11 was a screen fail

# merge intrial back into arm
  arms <- select(vsg, patient, centre, intrial) %>%
    distinct() %>%
    full_join(arms, by = c("centre", "patient"))

  nrow(distinct(vsg, patient, centre))

# with(distinct(select(vsg, patient, centre, arm, intrial)), table(arm, intrial))

#///////////////////////////////////////////////////////////////////////////////



# Microbiome treatment ---------------------------------------------------------

  mbt <- forms[[23]]

  mbt$drug.name[grepl("Tazobactam", mbt$drug.name)] <- "tazobactam"
# You have to do this one first because it throws off tolower()

  mbt$drug.name <- tolower(mbt$drug.name)
# sort(table(mbt$drug.name))

  mbt$drug.name[mbt$drug.name == "1. ceftazidime"] <- "ceftazidime"
  mbt$drug.name[mbt$drug.name == "co-amoxiclav 500/125"] <- "co-amoxiclav"
  mbt$drug.name[mbt$drug.name == "co-amoxivlav"] <- "co-amoxiclav"
  mbt$drug.name[mbt$drug.name == "augmentin"] <- "co-amoxiclav"
  mbt$drug.name[mbt$drug.name == "amoxicillin/ clavulanic acid"] <- "co-amoxiclav"
  mbt$drug.name[mbt$drug.name == "500 mg amoxicillin; 125 mg clavulanacid"] <- "co-amoxiclav"
  mbt$drug.name[mbt$drug.name == ""] <- "co-amoxiclav"
  mbt$drug.name[mbt$drug.name == "astreonam"] <- "aztreonam"
  mbt$drug.name[mbt$drug.name == "ceftazadime"] <- "ceftazidime"
  mbt$drug.name[mbt$drug.name == "ceftazdine"] <- "ceftazidime"
  mbt$drug.name[mbt$drug.name == "doxycyclin"] <- "doxycycline"
  mbt$drug.name[mbt$drug.name == "doxycyline"] <- "doxycycline"
  mbt$drug.name[mbt$drug.name == "moxiflacin"] <- "moxyfloxacin"
  mbt$drug.name[mbt$drug.name == "moxifloxacin"] <- "moxyfloxacin"
  mbt$drug.name[mbt$drug.name == "moxifloxacine"] <- "moxyfloxacin"
  mbt$drug.name[mbt$drug.name == "moxyfloxacine"] <- "moxyfloxacin"
  mbt$drug.name[mbt$drug.name == "tobramycin 180mg"] <- "tobramycin"
  mbt$drug.name[mbt$drug.name == "cetazidime"] <- "ceftazidime"
  mbt$drug.name[mbt$drug.name == "cotrimoxazol"] <- "co-trimoxizole"
  mbt$drug.name[mbt$drug.name == "co-trimazole"] <- "co-trimoxizole"
  mbt$drug.name[mbt$drug.name == "co-trimoxazole"] <- "co-trimoxizole"
  mbt$drug.name[mbt$drug.name == "urfamycine"] <- "urfamycin"
  mbt$drug.name[mbt$drug.name == "ceftazidim"] <- "ceftazidime"
  mbt$drug.name[mbt$drug.name == "ceftaz"] <- "ceftazidime"


  mbt$drug.name2 <- factor(mbt$drug.name, levels = names(sort(table(mbt$drug.name))))

# Adherence, used to determine other means of telling who is in what arm

  names(mbt)[8] <- "diff.tx"

  mbt$diff.tx2 <- factor(mbt$diff.tx, labels = c("Different", "Recommended"))

# Is the 3rd drug always the weird one?
# with(mbt, table(drug.name, occurence))

  mbt <- group_by(mbt, centre, patient) %>%
    mutate(occurence =  as.numeric(occurence)) %>%
    summarise(max_drugs = max(occurence + 1, na.rm = TRUE)) %>%
    full_join(mbt, by = c("centre", "patient"))

  mbt$arm2 <- ifelse(mbt$max_drugs > 2, "Active", "Control")

  mbt <- full_join(mbt, arms, by = c("patient", "centre")) %>%
    filter(!is.na(max_drugs))

  mbt <- mbt[!(mbt$centre == 7  & mbt$patient == 9), ] #7-9 was a screen fail


# nrow(distinct(select(mbt, patient, centre)))

# Add back into arms
  arms <- select(mbt, patient, centre, max_drugs, arm2) %>%
    distinct() %>%
    full_join(arms, by = c("patient", "centre"))

#///////////////////////////////////////////////////////////////////////////////



# GMUT cf genotyping -----------------------------------------------------------

  gmut <- forms[[2]]

  gmut <- gmut[!(gmut$centre == 2  & gmut$patient == 11), ] # 2-11 was a screen fail
  gmut <- gmut[!(gmut$centre == 7  & gmut$patient == 9), ]  # 7-9 was a screen fail

  alleles <- codes$label[codes$list == "c_mutation"]
  alleles <- toupper(alleles)

  alleles <- c("\U0394-F508",
               "R117H",
               "R560 T/K",
               "1717-1 G>A",
               "621+1 G>T",
               "G542X",
               "2789+5 G>A",
               "G852E",
               "N1303K",
               "R553X",
               "W1282X",
               "G551D",
               "OTHER")

#  "\u03b4"

  gmut[c("allele.1", "allele.2")] <- lapply(
    gmut[c("allele.1", "allele.2")],
    factor,
    levels = 1:13,
    labels = alleles
  )

# Add counts for alleles
  gmut <- group_by(gmut, allele.1) %>%
    summarise(n1 = n()) %>%
    full_join(gmut, by = "allele.1") %>%
    mutate(allele.1 = reorder(factor(allele.1), n1))

  gmut <- group_by(gmut, allele.2) %>%
    summarise(n2 = n()) %>%
    full_join(gmut, by = "allele.2") %>%
    mutate(allele.2 = reorder(factor(allele.2), n2))

  gmut <- group_by(gmut, allele.2, allele.1) %>%
    summarise(n3 = n()) %>%
    full_join(gmut, by = c("allele.2", "allele.1"))

  gmut <- ungroup(gmut)

  gmut <- full_join(gmut, arms, by = c("patient", "centre"))

#///////////////////////////////////////////////////////////////////////////////



# Merge arm to other forms forms as needed -------------------------------------

# centre_arms <- function(data, ...){
#   if(nrow(data) > 0){
#     data <- full_join(data, arms, by = c("centre", "patient"))
#   }
# }
#
# target <- c(1, 5, 12, 17) # dail, qrl, screening, demo
# forms[target] <- lapply(forms[target], centre_arms)
# rm(centre_arms, target)

#///////////////////////////////////////////////////////////////////////////////


# Time to next exacerbation ----------------------------------------------------

# Bring in new data that has dates and info on post-tx exacerabations (time to
# event/censoring)
  other_sheets <- paste0(
    "data/site_data/", list.files("data/site_data/", pattern = ".xlsx")
    )

# Import them all into a list
  import2 <- function(x){
    forms2 <- list()
    for(i in 1:length(x)){
      df <- read_excel(x[i])
      df <- df[rowSums(is.na(df)) != ncol(df),] # Remove blank rows
      names(df) <- c("centre", "patient", "post_date", "event", "withdrew",
                     "new_drug", "which_drug", "comment")
      forms2[[i]] <- df
    }
  return(forms2)
  }

  post_ex <- do.call(rbind, import2(other_sheets))

  post_ex <- post_ex[!(post_ex$centre == 2  & post_ex$patient == 11), ] # 2-11 was a screen fail
  post_ex <- post_ex[!(post_ex$centre == 7  & post_ex$patient == 9), ]  # 7-9 was a screen fail

# Clean
  post_ex$patient <- as.numeric(gsub("998",  "", as.character(post_ex$patient)))
  post_ex$patient <- as.numeric(post_ex$patient)
  post_ex$event <- factor(post_ex$event)
  post_ex$withdrew <- factor(post_ex$withdrew)
  post_ex$centre <- as.numeric(post_ex$centre)

# ggplot(day0start, aes(y = day0start_date, x = centre, color = centre)) +
#   geom_jitter(size = 3, width = 0.2) +
#   scale_color_viridis(discrete = TRUE)

# Why are there mismatches with randomization list?
# t1 <- as.character(with(arms,    interaction(centre, patient)))
# t2 <- as.character(with(post_ex, interaction(centre, patient)))
#
# t1[!(t1 %in% t2)] # 9-14, 9-31
# t2[!(t2 %in% t1)] # 2-11

# Add arm
  post_ex <- select(arms, arm, stratification, patient, centre, intrial) %>%
    distinct() %>%
    right_join(post_ex, by = c("patient", "centre"))

# Why are there mismatches with randomization list?
# t1 <- as.character(with(filter(vsg, visite == 1),
#                         interaction(centre, patient)))
# t2 <- as.character(with(post_ex, interaction(centre, patient)))
#
# t1[!(t1 %in% t2)] # Same as for arm 9-14, 9-31

# Add start time - dfrom day zero (first eligible exacerbation)

  post_ex <- filter(vsg, visite == 1) %>%
    select(patient, centre, day0start_date = datvis) %>%
    right_join(post_ex, by = c("patient", "centre"))

# Just those with day 0 exacerbation and a known event/censor date
# post_ex <- filter(post_ex, intrial == "Yes")
# post_ex <- filter(post_ex, !is.na(post_date))

# Calculate time to event
  post_ex$time <- post_ex$post_date - post_ex$day0start_date
  post_ex$time2 <- difftime(post_ex$post_date, post_ex$day0start_date, units = "weeks")

# with(post_ex, plot(time, time2))
  post_ex$event2 <- as.numeric(factor(tolower(post_ex$event))) - 1

  post_ex$new_drug <- tolower(post_ex$new_drug)
  post_ex$new_drug[is.na(post_ex$day0start_date)] <- NA

  post_ex$new_drug2[grepl("yes", post_ex$new_drug)] <- "Yes"
  post_ex$new_drug2[is.na(post_ex$new_drug2) &
                    !is.na(post_ex$day0start_date)] <- "No"
  post_ex$new_drug2 <- factor(post_ex$new_drug2)

  post_ex$which_drug <- tolower(post_ex$which_drug)

  post_ex$Orkambi[grepl("orkambi", tolower(post_ex$which_drug))] <- "Yes"
  post_ex$Orkambi[is.na(post_ex$Orkambi) &
                 !is.na(post_ex$day0start_date)] <- "No"

  post_ex$Orkambi <- factor(post_ex$Orkambi)

#///////////////////////////////////////////////////////////////////////////////



# Total iv days and exacerbations ----------------------------------------------

  oneyear <- read_csv("data/Year_1_study_data.csv", skip = 1)

  names(oneyear) <- tolower(names(oneyear))

# nrow(distinct(select(oneyear, patient, centre)))

# Add arm
  oneyear <- select(vsg, arm, stratification, patient, centre, intrial) %>%
    distinct() %>%
    full_join(oneyear, by = c("patient", "centre")) %>%
    filter(!is.na(visite))

  names(oneyear)[c(9:12)] <- c("ivs", "fev1", "exacerbations", "date")

  oneyear <- filter(oneyear, visite == 7)

#///////////////////////////////////////////////////////////////////////////////



# CFQR -------------------------------------------------------------------------
# The CFQ-R questionnaire has a specific way for calculating the outcome.
# Link with instructions for completion:
# http://www.psy.miami.edu/cfq_QLab/scoring.html

  cfqr <- forms[[21]]
  old_names <- names(cfqr)
  old_names[14:63] <- paste(c(1:50), old_names[14:63], " ")

  cfqr <- cfqr[!(cfqr$centre == 2  & cfqr$patient == 11), ] # 2-11 was a screen fail
  cfqr <- cfqr[!(cfqr$centre == 7  & cfqr$patient == 9), ]  # 7-9 was a screen fail


# old_names[c(6, 10, 13, 15, 17, 18, 23, 28, 30, 32, 34, 35, 43) + 12]

  qs <- paste0("q", c(1:50))

  names(cfqr)[14:63] <- qs

  cfqr[qs] <- lapply((cfqr)[qs], as.numeric)

# lapply(cfqr[qs], table)
# 43 has 5s but that doesn't see to be a prob...the instructions suggest that
# you skip it

# cfqr$q43[cfqr$q43 == 5] <- 4

# We need to reverse items, these are them
  revs <- paste0("q", c(6, 10, 13, 15, 17, 18, 23, 28, 30, 32, 34, 35, 43))

  cfqr[revs] <- lapply(cfqr[revs], function(x) case_when(x == 1 ~ 4,
                                                         x == 2 ~ 3,
                                                         x == 3 ~ 2,
                                                         x == 4 ~ 1))


  item_list <- list(cfqr[paste0("q", c(1:5, 13, 19, 20))],  # physical
                    cfqr[paste0("q", c(7:8, 12, 31, 33))],  # emotion
                    cfqr[paste0("q", c(6, 9:11))],          # vitality
                    cfqr[paste0("q", c(14, 21, 50))],       # eat
                    cfqr[paste0("q", c(15:17))],            # txburden
                    cfqr[paste0("q", c(18, 32, 34))],       # health
                    cfqr[paste0("q", c(22:23, 27:30))],     # social
                    cfqr[paste0("q", c(24:26))],            # body
                    cfqr[paste0("q", c(35:38))],            # role
                    cfqr[paste0("q", c(39))],               # weight
                    cfqr[paste0("q", c(40:42, 44:46))],     # respir
                    cfqr[paste0("q", c(47:49))])            # digest

  items <- sapply(item_list, ncol)

  make_score <- function(x){
    return(ifelse(rowSums(is.na(item_list[[x]]),  na.rm = TRUE) >= items[x] / 2,
                  NA,
                  rowSums(item_list[[x]],  na.rm = TRUE)))
  }

  cfqr$physical <- make_score(1)
  cfqr$emotion  <- make_score(2)
  cfqr$vitality <- make_score(3)
  cfqr$eat      <- make_score(4)
  cfqr$txburden <- make_score(5)
  cfqr$health   <- make_score(6)
  cfqr$social   <- make_score(7)
  cfqr$body     <- make_score(8)
  cfqr$role     <- make_score(9)
  cfqr$weight   <- make_score(10)
  cfqr$respir   <- make_score(11)
  cfqr$digest   <- make_score(12)

  cfqr$all      <- rowSums(cfqr[paste0("q", c(1:42, 44:50))], na.rm = TRUE)
  subs <- c("physical", "emotion", "vitality", "eat", "txburden", "health",
            "social", "body", "role", "weight", "respir", "digest")
  cfqr$all_2    <- rowSums(cfqr[subs], na.rm = TRUE)

# with(cfqr[c(qs, "all", "all_2")][complete.cases(cfqr[c(qs, "all", "all_2")]), ], plot(all, all_2)) # Good

  for(i in 1:12){
    a <- items[i]
    cfqr <- cbind(cfqr, ((cfqr[subs][i] - a) / ((4 * a) - a)) * 100)
  }

  names(cfqr)[78:89] <- paste0(names(cfqr)[78:89], "_s")

# cfqr[78:89]

# table(cfqr$visite)

  Subs <- c( "Body", "Digestive", "Eating", "Emotion", "Health", "Physical",
             "Respiratory", "Role", "Social", "Treatment Burden", "Vitality",
             "Weight")

  cfqr <- full_join(cfqr, arms, by = c("patient", "centre"))

  cfqr_long <- select(cfqr, arm, patient, centre, visite, ends_with("_s")) %>%
    filter(visite < 6) %>%
    gather(type, value, ends_with("_s")) %>%
    mutate(visite = factor(visite, levels = c(0, 1, 3:5),
                           labels = c("Baseline", "Day 0",
                                      "+14 days", "+28 days", "3 months"))) %>%
    filter(!is.na(arm)) %>%
    mutate(type = factor(type, labels = Subs))

#///////////////////////////////////////////////////////////////////////////////

# Demo -------------------------------------------------------------------------

  demo <- forms[[5]]

  demo <- demo[!(demo$centre == 2  & demo$patient == 11), ] # 2-11 was a screen fail
  demo <- demo[!(demo$centre == 7  & demo$patient == 9), ]  # 7-9 was a screen fail

  demo <- filter(demo, visite == 0)

# nrow(distinct(demo, patient, centre))

  names(demo)[12] <- "weekly_alcohol"

  demo$smoking.history <- factor(
    as.numeric(demo$smoking.history),
    labels = c("Current", "Former", "Ever"))

  demo$weekly_alcohol <- as.numeric(demo$weekly_alcohol)

  demo <- mutate(demo, alcohol = case_when(weekly_alcohol == 0 ~ "None",
                                           weekly_alcohol > 0 ~ "Some",
                                           weekly_alcohol > 10 ~ "Moderate/Heavy"))
  demo$alcohol <- "None"
  demo$alcohol <- ifelse(demo$weekly_alcohol > 0, "Some", demo$alcohol)
  demo$alcohol <- ifelse(demo$weekly_alcohol > 10, "Moderate/Heavy", demo$alcohol)
  demo$alcohol[is.na(demo$weekly_alcohol)] <- NA

  demo$alcohol <- factor(demo$alcohol, levels = c("None", "Some", "Moderate/Heavy"))

#///////////////////////////////////////////////////////////////////////////////



# Symptom diary ----------------------------------------------------------------

  symp <- forms[[12]]

  symp <- filter(symp, centre %in% c(1:6, 9))

  names(symp)[10:25] <- c("Breathe",
                          "Feverish",
                          "Tired",
                          "Chills_Sweats",
                          "Cough",
                          "Mucus",
                          "Tightness",
                          "Wheezing",
                          "Sleep",
                          "Worried",
                          "Cranky",
                          "Sad_Depressed",
                          "Frustrated",
                          "Sitting_Prone",
                          "Reduce_Activities",
                          "Miss_Work_School")

  symp[10:25] <- lapply(symp[10:25], factor, levels = 1:5,
                        labels = c("Best", "Ok", "Moderate", "Bad", "Worst"))

  symp <- select(vsg, arm, stratification, patient, centre, intrial) %>%
    distinct() %>%
    full_join(symp, by = c("patient", "centre")) %>%
    filter(!is.na(visite))

  symp <- select(symp, - datvis, - please.specify)


# MISC -------------------------------------------------------------------------
# Add site names
  sites <- c("Cork", "Belfast", "Washington", "Heidelberg", "Papworth",
             "Leuven", "Paris", "Lyon", "Manchester")

  arms$centre      <- factor(arms$centre,      levels = 1:9, labels = sites)
  cfqr$centre      <- factor(cfqr$centre,      levels = 1:9, labels = sites)
  cfqr_long$centre <- factor(cfqr_long$centre, levels = 1:9, labels = sites)
  gmut$centre      <- factor(gmut$centre,      levels = 1:9, labels = sites)
  mbt$centre       <- factor(mbt$centre,       levels = 1:9, labels = sites)
  oneyear$centre   <- factor(oneyear$centre,   levels = 1:9, labels = sites)
  post_ex$centre   <- factor(post_ex$centre,   levels = 1:9, labels = sites)
  vsg$centre       <- factor(vsg$centre,       levels = 1:9, labels = sites)
  con$centre       <- factor(con$centre,       levels = 1:9, labels = sites)
  demo$centre      <- factor(demo$centre,      levels = 1:9, labels = sites)
  symp$centre      <- factor(symp$centre,      levels = 1:9, labels = sites)

# l <- list(arms, cfqr, cfqr_long, gmut, mbt, oneyear, post_ex, vsg)
# lapply(l, function(x) table(x[, "centre"]))


#///////////////////////////////////////////////////////////////////////////////

# Error in weights

  vsg$weight..without.shoes..[as.numeric(vsg$weight..without.shoes..) > 120] <- NA

# Error in dates of birth - 4 people

# age1 <- select(vsg, age., patient, centre) %>%
#   distinct()
#
# age2 <- select(vsg, age., patient, centre) %>%
#   distinct(patient, centre, .keep_all = TRUE)
#
# a <- interaction(age1$patient, age1$centre, age1$age.)
# b <- interaction(age2$patient, age2$centre, age2$age.)
# a[!(a %in% b)]
#
  vsg$age.[vsg$patient == 7 & vsg$centre == "Washington"] <- 40.23
  vsg$age.[vsg$patient == 1 & vsg$centre == "Heidelberg"] <- 36.78
  vsg$age.[vsg$patient == 2 & vsg$centre == "Heidelberg"] <- 28.10
  vsg$age.[vsg$patient == 25 & vsg$centre == "Manchester"] <- 38.23

# Save all the changes ---------------------------------------------------------

  save(forms, arms, form_numbers, codes, oneyear, post_ex, cfqr, cfqr_long, vsg,
       mbt, gmut, sites, con, demo, symp,
       file = "cleaned.RData")

# rm(list = ls())

# gisli <- full_join(vsg, mbt, by = c("centre", "patient", "visite"))
# gisli <- select(gisli, -contains(".y"))
# names(gisli) <- gsub(".x", "", names(gisli))
# write.csv(gisli, file = "gisli.csv", row.names = FALSE)


# Final dates, EoS, for a request Evelyn sent in March 2019.

# esq <- forms[[27]]
#
# names(esq) <- c("centre", "patient", "form", "visit", "date_visit",
#                 "completed", "date_completed", "died", "date_died", "dropped",
#                 "date_dropped", "reason_drop")
#
# esq$centre <- factor(esq$centre, levels = 1:9, labels = sites)
#
# group_by(esq, centre) %>% summarise(date_completed = max(date_completed, na.rm = TRUE))
# group_by(esq, centre) %>% summarise(date_died = max(date_died, na.rm = TRUE))
# group_by(esq, centre) %>% summarise(date_dropped = max(date_dropped, na.rm = TRUE))
#
# filter(vsg, visite2 == "3 months") %>% group_by(centre) %>%
#   summarise(last_date = max(datvis, na.rm = TRUE))
#
# filter(post_ex, post_date < "2018-05-16") %>% group_by(centre) %>%
#   summarise(last_date = max(post_date, na.rm = TRUE))


#///////////////////////////////////////////////////////////////////////////////