########################################################################################################
# Learner Autonomy Meta-Analysis: Main Analyses
########################################################################################################
# Xue Wang's meta-analysis of learner autonomy.

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear Variables
rm(list=ls(all=TRUE))

# Load packages
test<-require(googledrive)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googledrive")
  require(googledrive)
}
test<-require(googlesheets4)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googlesheets4")
  require(googlesheets4)
}
test<-require(plyr)   #rename()
if (test == FALSE) {
  install.packages("plyr")
  require(plyr)
}
test<-require(metafor)   #rma()
if (test == FALSE) {
  install.packages("metafor")
  require(metafor)
}
test<-require(clubSandwich)   #coef_test()
if (test == FALSE) {
  install.packages("clubSandwich")
  require(clubSandwich)
}
test<-require(ggplot2)   #ggplot()
if (test == FALSE) {
  install.packages("ggplot2")
  require(ggplot2)
}
test<-require(dplyr)   #bind_rows()
if (test == FALSE) {
  install.packages("dplyr")
  require(dplyr)
}
test<-require(flextable)   
if (test == FALSE) {
  install.packages("flextable")
  require(flextable)
}
test<-require(officer)   
if (test == FALSE) {
  install.packages("officer")
  require(officer)
}
test<-require(weightr)   
if (test == FALSE) {
  install.packages("weightr")
  require(weightr)
}
test<-require(mpower)   
if (test == FALSE) {
  install.packages("mpower")
  require(mpower)
}
test<-require(robumeta)   
if (test == FALSE) {
  install.packages("robumeta")
  require(robumeta)
}
rm(test)

# Define functions

########################################################################################################
# Load data
# ########################################################################################################
# Clear existing tokens to avoid conflicts
drive_deauth()
gs4_deauth()

# Authenticate for Google Drive
drive_auth(email = "snowxuewang297@gmail.com", cache = FALSE)

# Find the spreadsheet ID
id <- drive_find(pattern = "L2 write autonomy study coding", type = "spreadsheet")$id[1]

# Authenticate for Google Sheets
gs4_auth(email = "snowxuewang297@gmail.com", cache = FALSE)


# load findings and studies
dfstudy <- read_sheet(id, sheet = "Study", col_types = "c")

dfindings <- read_sheet(id, sheet = "Findings", col_types = "c")

rm()

########################################################################################################
# Clean data
########################################################################################################
# drop irrelevant rows/columns
dfstudy <- subset(dfstudy, is.na(Drop) == TRUE)
dfindings <- subset(dfindings, is.na(Drop) == TRUE)

# Remove Drop and Reviewer columns 
dfstudy <- dfstudy %>% 
  select(-Drop, -Reviewer1, -Reviewer2, -StudyLink)

dfindings <- dfindings %>% 
  select(-Drop, -Reviewer1, -Reviewer2, -StudyLink)

# Merge study and findings
df <- merge(dfstudy, dfindings, by = "Study")

# rename columns as necessary

# df <- plyr::rename(df, c("Source of publication" = "Sourceofpublication", 
#                          "Funding status" = "Fundingstatus"))

# reformat columns as necessary
num <- c("TreatmentGroupN", "ControlGroupN","TreatmentN","ControlN","SampleSize", "PretestMean.Exp","PretestSD.Exp",
         "PretestMean.Ctrl","PretestSD.Ctrl", "PosttestMean.Exp","PosttestSD.Exp",
         "PosttestMean.Ctrl","PosttestSD.Ctrl", "PretestES")
df[num] <- lapply(df[num], as.numeric)
rm(num)

########################################################################################################
# Prep data
########################################################################################################
################################################################
# Create unique identifiers (ES, study, program)
################################################################
df$ESId <- as.numeric(rownames(df))
df$StudyID <- as.numeric(as.factor(paste(df$Study)))
table(df$StudyID)

################################################################
# Create categorical variables
################################################################

################################################################
# Create dummies
################################################################
# Method moderators: duration, unit of assignment, measurement developed by researcher, delayed test
df$Longerthan12weeks <- 0
df$Longerthan12weeks[which(df$`12weeks`=="1")] <- 1
table(df$`12weeks`, df$Longerthan12weeks, useNA = "ifany")

# df$TwoHoursAWeek <- 0
# df$TwoHoursAWeek[which(df$`TwohrPerWeek`=="1")] <- 1
# table(df$`TwohrPerWeek`, df$TwoHoursAWeek, useNA = "ifany")

# df$Dosage <- 0
# df$Dosage[which(df$`IntervDosage`>"23")] <- 1
# table(df$`IntervDosage`, df$Dosage, useNA = "ifany")

# df$AssignStudents <- 0
# df$AssignStudents[which(df$`UnitOfAssignment`=="students")] <- 1
# table(df$`UnitOfAssignment`, df$AssignStudents, useNA = "ifany")

df$ResearcherDevelopedMeasurement <- 0
df$ResearcherDevelopedMeasurement[which(df$`MeasureDevelopedByResearcher`=="1")] <- 1
table(df$`MeasureDevelopedByResearcher`, df$ResearcherDevelopedMeasurement, useNA = "ifany")

# df$DelayedTest <- 0
# df$DelayedTest[which(df$`DelayedPost`=="1")] <- 1
# table(df$`DelayedPost`, df$DelayedTest, useNA = "ifany")

# Unit moderators: large sample, university, female
df$LargeSample <-0
df$LargeSample[which(df$`SampleSize`>=76)] <- 1
table(df$`SampleSize`, df$LargeSample, useNA = "ifany")

df$University <-0
df$University[which(df$`College`==1)] <- 1
table(df$`College`, df$University, useNA = "ifany")

df$MajorityFemale <-0
df$MajorityFemale[which(df$`MajorFemale`==1)] <- 1
table(df$`MajorFemale`, df$MajorityFemale, useNA = "ifany")

# Treatment moderators:Strategy instruction, formality, technology, peer support, Collab, feedback, reflection, PD
df$Strategy <-0
df$Strategy[which(df$`StrategyInstructionYes`==1)] <- 1
table(df$`StrategyInstructionYes`, df$Strategy, useNA = "ifany")

df$Formality <-0
df$Formality[which(df$`formality`==1)] <- 1
table(df$`formality`, df$Formality, useNA = "ifany")

df$Technology <- 0
#df$Technology[which(df$`TechnologyUse`==1)] <- 1
df$Technology[which(df$`TargetTech`==1)] <- 1
table(df$`TargetTech`, df$Technology, useNA = "ifany")

df$Peer <- 0
#df$Peer[which(df$`PeerSupport`==1)] <- 1
df$Peer[which(df$`TargetCollab`==1)] <- 1
table(df$`TargetCollab`, df$Peer, useNA = "ifany")

df$Collab <- 0
df$Collab[which(df$`collaborative activities`==1)] <- 1
table(df$`collaborative activities`, df$Collab, useNA = "ifany")

df$Feedback <- 0
#df$Feedback[which(df$`FeedbackProvision`==1)] <- 1
df$Feedback[which(df$`TargetFeed`==1)] <- 1
table(df$`TargetFeed`, df$Feedback, useNA = "ifany")

df$Reflection <- 0
df$Reflection[which(df$`ReflectionSelfEvaluation`==1)] <- 1
table(df$`ReflectionSelfEvaluation`, df$Reflection, useNA = "ifany")

df$PD <- 0
df$PD[which(df$`HasPD`==1)] <- 1
table(df$`HasPD`, df$PD, useNA = "ifany")

# Outcome moderators: academic, cognitive, metacognitive, motivational, social, behavior, multi
df$AcademicOutcome <- 0
df$AcademicOutcome[which(df$`AcademicOutcomes`==1)] <- 1
table(df$`AcademicOutcomes`, df$AcademicOutcome, useNA = "ifany")

df$CognitiveOutcome <- 0
#df$CognitiveOutcome[which(df$`CognitiveOutcomes`==1)] <- 1
df$CognitiveOutcome[which(df$`CognitiveOutcomes` == 1 & 
                            df$MetacognitiveOutcomes != 1 & 
                            df$MotivationalOutcomes != 1 & 
                            df$SocialOutcomes != 1 & 
                            df$BehavioralOutcomes != 1)] <- 1
table(df$`CognitiveOutcomes`, df$CognitiveOutcome, useNA = "ifany")

df$MetacognitiveOutcome <- 0
#df$MetacognitiveOutcome[which(df$`MetacognitiveOutcomes`==1)] <- 1
df$MetacognitiveOutcome[which(df$`MetacognitiveOutcomes` == 1 &
                            df$CognitiveOutcomes != 1 &
                            df$MotivationalOutcomes != 1 &
                            df$SocialOutcomes != 1 &
                            df$BehavioralOutcomes != 1)] <- 1
table(df$`MetacognitiveOutcomes`, df$MetacognitiveOutcome, useNA = "ifany")

df$MotivationalOutcome <- 0
#df$MotivationalOutcome[which(df$`MotivationalOutcomes`==1)] <- 1
df$MotivationalOutcome[which(df$`MotivationalOutcomes` == 1 & 
                                df$CognitiveOutcomes != 1 & 
                                df$MetacognitiveOutcomes != 1 & 
                                df$SocialOutcomes != 1 & 
                                df$BehavioralOutcomes != 1)] <- 1
table(df$`MotivationalOutcomes`, df$MotivationalOutcome, useNA = "ifany")

df$SocialOutcome <- 0
#df$SocialOutcome[which(df$`SocialOutcomes`==1)] <- 1
df$SocialOutcome[which(df$`SocialOutcomes` == 1 & 
                               df$CognitiveOutcomes != 1 & 
                               df$MetacognitiveOutcomes != 1 & 
                               df$MotivationalOutcomes != 1 & 
                               df$BehavioralOutcomes != 1)] <- 1
table(df$`SocialOutcomes`, df$SocialOutcome, useNA = "ifany")

df$BehaviorOutcome <- 0
#df$BehaviorOutcome[which(df$`BehavioralOutcomes`==1)] <- 1
df$BehaviorOutcome[which(df$`BehavioralOutcomes` == 1 & 
                         df$CognitiveOutcomes != 1 & 
                         df$MetacognitiveOutcomes != 1 & 
                         df$MotivationalOutcomes != 1 & 
                         df$SocialOutcomes != 1)] <- 1
table(df$`BehavioralOutcomes`, df$BehaviorOutcome, useNA = "ifany")

df$MultipleOutcome <- 0
num <- c("CognitiveOutcomes", "MetacognitiveOutcomes", 
         "MotivationalOutcomes", "SocialOutcomes", 
         "BehavioralOutcomes")
df[num] <- lapply(df[num], as.numeric)
rm(num)
outcome_sum <- rowSums(df[, c("CognitiveOutcomes", "MetacognitiveOutcomes", 
                              "MotivationalOutcomes", "SocialOutcomes", 
                              "BehavioralOutcomes")], na.rm = TRUE)
df$MultipleOutcome <- ifelse(outcome_sum > 1, 1, 0)
table(outcome_sum, df$MultipleOutcome, useNA = "ifany")

# Settings moderators: China vs other countries, publication year
df$EastAsia <-0
df$EastAsia[which(df$`Country`=="China" | df$`Country`=="Vietnam")] <- 1
table(df$`Country`, df$EastAsia, useNA = "ifany")

df$After2019 <- 0
df$After2019[which(df$`PubYear`>=2019)] <- 1
table(df$`PubYear`, df$After2019, useNA = "ifany")


################################################################
# Create study level
################################################################


################################################################
# Create centered variables
################################################################
vars <- c("Longerthan12weeks", "ResearcherDevelopedMeasurement",
          "LargeSample", "University", "MajorityFemale", "Strategy", "Formality", "Technology", "Peer", "Collab",
          "Feedback", "Reflection", "PD", "AcademicOutcome", "CognitiveOutcome", "MetacognitiveOutcome", 
          "MotivationalOutcome", "SocialOutcome", "BehaviorOutcome", "MultipleOutcome", "EastAsia", "After2019")  

centered <- paste(vars, ".c", sep = "")
df[centered] <- as.data.frame(lapply(df[vars], function(x) x-mean(x)))
rm(vars, centered)

################################################################
# Recode uncentered/categorical as ordered factors
################################################################

################################################################
# Calculate effect sizes
################################################################
df <- escalc(measure = "SMD", m1i = PretestMean.Exp, m2i = PretestMean.Ctrl, 
             sd1i = PretestSD.Exp, sd2i = PretestSD.Ctrl, n1i = TreatmentN, 
             n2i = ControlN, data = df, append = TRUE, var.names=c("ES_pre", "var_pre"))

df <- escalc(measure = "SMD", m1i = PosttestMean.Exp, m2i = PosttestMean.Ctrl, 
             sd1i = PosttestSD.Exp, sd2i = PosttestSD.Ctrl, n1i = TreatmentN, 
             n2i = ControlN, data = df, append = TRUE, var.names=c("ES_post", "var_post"))

df$ES <- df$ES_post - df$ES_pre


################################################################
# Calculate variance
################################################################
#calculate standard errors
df$se<-sqrt(((df$TreatmentN+df$ControlN)/(df$TreatmentN*df$ControlN))+((df$ES*df$ES)/(2*(df$TreatmentN+df$ControlN))))

#calculate variance
df$var<-df$se*df$se

################################################################
# Correct effect sizes for clustering ala Hedges 2007
################################################################

########################################################################################################
# Analyses
########################################################################################################
#################################################################################
# Exploratory Analyses
#################################################################################

#################################################################################
# Descriptive Statistics
#################################################################################
library(tableone)

# identify variables for descriptive tables (study-level and outcome-level)
vars_study <- c("Longerthan12weeks", "LargeSample", "University", "MajorityFemale",
                "Strategy", "Formality", "Technology", "Collab", "Feedback", "PD", "EastAsia", "After2019")

vars_outcome <- c("ResearcherDevelopedMeasurement", "AcademicOutcome", "CognitiveOutcome", "MetacognitiveOutcome", 
                  "MotivationalOutcome", "SocialOutcome", "BehaviorOutcome", "MultipleOutcome")

# 1) make df with *only* the study-level variables of interest and studyIDs in it
# study <- unique(df[c("Authors", "Year", "StudyID")])
study_level <- unique(df[c("Study", "StudyID")])

study_level <- df[c("Study", "StudyID", vars_study)]

# 2) remove duplicated rows
study_level <- unique(study_level)
# 3) make sure it is the correct number of rows (should be same number of studies you have)
length(study_level$StudyID)==length(unique(df$StudyID))
# don't skip step 3 - depending on your data structure, some moderators can be
# study-level in one review, but outcome-level in another

# create the table "chunks"
table_study_df <- as.data.frame(print(CreateTableOne(vars = vars_study, data = study_level, factorVars = vars_study, includeNA = TRUE), showAllLevels = TRUE))
table_outcome_df <- as.data.frame(print(CreateTableOne(vars = vars_outcome, data = df, includeNA = TRUE, factorVars = vars_outcome), showAllLevels = TRUE))

rm(vars_study, vars_outcome)

#################################################################################
# Null Model
#################################################################################
# estimate a covariance matrix
V_list <- impute_covariance_matrix(vi = df$var,  #known correlation vector
                                   cluster = df$StudyID, #study ID
                                   r = 0.80) #assumed correlation 

MVnull <- rma.mv(yi=ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 random = ~1 | StudyID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=df, #define data
                 method="REML") #estimate variances using REML
MVnull

#t-tests of each covariate #
MVnull.coef <- coef_test(MVnull,#estimation model above
                         cluster=df$StudyID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best) #robust standard error
MVnull.coef

#################################################################################
# Metaregression
#################################################################################
# save list of moderators to include
rm(terms)
terms <- c("Longerthan12weeks", "ResearcherDevelopedMeasurement", 
           "LargeSample", "University", "MajorityFemale",
           "Strategy", "Formality","Technology", "Feedback", "Collab", "PD", 
            "AcademicOutcome", "MetacognitiveOutcome", "MotivationalOutcome", 
           "EastAsia", "After2019") 

# use centered versions
terms.c <- paste(terms, ".c", sep = "")

## with interaction
#format moderators into formula (an R-specific type)
# intervention1 <- paste(c("Technology"), ".c", sep = "")
# outcome1 <- paste(c("MetacognitiveOutcome"), ".c", sep = "")
# interact1 <- c()
# for(i in 1:length(intervention1)) {
#  for(j in 1:length(outcome1)) {
#    term <- paste(intervention1[i], outcome1[j], sep = "*")
#    interact1 <- c(interact1, term)
#  }
# }
# 
# intervention2 <- paste(c("PD"), ".c", sep = "")
# outcome2 <- paste(c("AcademicOutcome"), ".c", sep = "")
# interact2 <- c()
# for(i in 1:length(intervention2)) {
#   for(j in 1:length(outcome2)) {
#     term <- paste(intervention2[i], outcome2[j], sep = "*")
#     interact2 <- c(interact2, term)
#   }
# }
# 
# interact <- c(interact1)
# formula <- reformulate(termlabels = c(terms.c, interact))
# formula

## without interaction
formula <- reformulate(termlabels = c(terms.c))

MVfull <- rma.mv(yi=ES, #effect size
                 V = V_list, #variance (this is what changes from HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | StudyID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=df, #define data
                 method="REML") #estimate variances using REML
MVfull

#t-tests of each covariate #
MVfull.coef <- coef_test(MVfull,#estimation model above
                         cluster=df$StudyID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVfull.coef

# Forest plot
#forest(MVfull)

#formula <- reformulate(termlabels = c(terms.c), response = "ES")

# Forest plot with RVE
library(robumeta)

setwd("/Users/xuewang/Desktop/Desktop - Xue’s MacBook Pro/Research/Sally/L2 writing autonomy meta")

df$label <- paste(df$Study)

# Make sure there are no NA values in crucial columns
df <- df %>% 
  filter(!is.na(ES) & !is.na(StudyID) & !is.na(var))


robu_intercept <- robu(formula = ES ~ 1, data = df, studynum = StudyID, var.eff.size = var)
png(filename = "Forest Plot.png", height = 45, width = 12, units = "in", res = 1000, pointsize = 12)
forest.robu(robu_intercept, es.lab = "ESId", study.lab = "label")
dev.off()

# Load required library
library(writexl)  # If not installed, first run: install.packages("writexl")

# Export to Excel
write_xlsx(df, "L2_writing_autonomy_data.xlsx")


#################################################################################
# Calculating Marginal Means
#################################################################################
# re-run model for each moderator to get marginal means for each #

# set up table to store results
means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))

### moderators for means need to be factors...so if true binary, just recode the dummy, if multi-level, need to replace with a multi-level factor variable instead of multiple dummies.

# # make factor moderators for means
# Method moderators: duration, unit of assignment, measurement developed by researcher, delayed test
df$Longerthan12weeks.f <- as.factor(ifelse(df$`X12weeks`==1, "12 weeks or longer", "Less than 12 weeks"))

df$ResearcherDevelopedMeasurement.f <- as.factor(ifelse(df$MeasureDevelopedByResearcher==1, "Researcher-made measure", "Not researcher-made measure"))

#df$DelayedTest.f <- as.factor(ifelse(df$`DelayedPost`==1, "Delayed post-test", "Not delayed post-test"))

# Unit moderators: large sample, university, female
df$LargeSample.f <- as.factor(ifelse(df$SampleSize>=76, "Large Sample", "Small Sample"))

df$University.f <- as.factor(ifelse(df$College==1, "University", "K-12"))

df$MajorityFemale.f <- as.factor(ifelse(df$MajorFemale==1, "Majority female", "Not majority female"))

# Treatment moderators:Strategy instruction, formality, technology, collab, feedback, reflection, PD
df$Strategy.f <- as.factor(ifelse(df$StrategyInstructionYes==1, "Strategy instruction", "Not strategy instruction"))

df$Formality.f <- as.factor(ifelse(df$Formality==1, "From teacher to self-directed", "Teacher-directed"))

df$Feedback.f <- as.factor(ifelse(df$TargetFeed==1, "Feedback intervention", "Not feedback intervention"))

df$Collaboration.f <- as.factor(ifelse(df$Collab==1, "Involve collaboration", "No collaboration"))

df$Technology.f <- as.factor(ifelse(df$TargetTech==1, "Technology intervention", "Not technology intervention"))

df$PD.f <- as.factor(ifelse(df$HasPD==1, "PD", "No PD"))

# Outcome moderators: academic, cognitive, metacognitive, motivational, social, behavior, multi
# df$AcademicOutcome.f <- as.factor(ifelse(df$AcademicOutcome==1, "Academic outcomes", "All other outcomes"))
# 
# df$MetacognitiveOutcome.f <- as.factor(ifelse(df$MetacognitiveOutcome==1, "Metacognitive outcomes", "All other outcomes"))
# 
# df$MotivationalOutcome.f <- as.factor(ifelse(df$MotivationalOutcome==1, "Motivational outcomes", "All other outcomes"))

df$OutcomeType.f <- factor(1* df$AcademicOutcome + 2 * df$CognitiveOutcome +
                             3 * df$MetacognitiveOutcome + 4 * df$MotivationalOutcome +
                             5 * df$SocialOutcome + 6 * df$BehaviorOutcome + 7 * df$MultipleOutcome,
                           labels = c("Academic outcome", "Cognitive outcome", "Metacognitive outcome",
                                      "Motivational outcome", "Social outcome", "Behavioral outcome", "Multiple learner autonomy outcomes"))

# Settings moderators: China vs other countries, publication year
df$EastAsia.f <- as.factor(ifelse(df$EastAsia==1, "East Asia", "Other countries"))

df$After2019.f <- as.factor(ifelse(df$After2019==1, "After 2019", "Before 2019"))

##################################################################################

mods <- c("Longerthan12weeks.f", "ResearcherDevelopedMeasurement.f",
          "LargeSample.f", "University.f", "MajorityFemale.f", "Strategy.f", "Formality.f", "Collaboration.f",
          "Feedback.f", "Technology.f", "PD.f", "OutcomeType.f","EastAsia.f", "After2019.f")

for(i in 1:length(mods)){
  #i <- 15
  formula <- reformulate(termlabels = c(mods[i], terms.c, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=ES, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=df, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=df$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means

#publication bias , selection modeling using weightr package
MVfull_y <- MVfull$ES
MVfull_v <- MVfull$var
weightfunct(effect = df$ES, v = df$var) # implements weight-function models

weightfunct(df$ES, df$var, steps = c(.025)) # The package indeed uses one-tailed 
#p-values for its cutpoints, The steps = c(.025) refers to a one-tailed p-value of 0.025
#This corresponds to a two-tailed p-value of 0.05 (which is the conventional significance threshold)
weightfunct(df$ES, df$var, steps = c(.025, .05))
weightfunct(df$ES, df$var, steps = c(.025, .50))
weightfunct(df$ES, df$var, steps = c(.025, .05, .50))
weightfunct(df$ES, df$var, steps = c(.005,.025, .05))
weightfunct(df$ES, df$var, steps = c(.005, .025, .05, .50)) 
# The weight for the first interval is fixed at 1 (as a reference). 
# Other weights are interpreted relative to this reference
# Studies with p-values between 0.005 and 0.025 are about 2.42 times more likely 
# to be observed/published than studies with p-values < 0.005

terms <- c("Longerthan12weeks", "ResearcherDevelopedMeasurement",
           "LargeSample", "University", "MajorityFemale",
           "Strategy", "Formality", "Technology", "Feedback", "PD", "Collab",
           "AcademicOutcome", "MetacognitiveOutcome", "MotivationalOutcome", 
           "EastAsia", "After2019") 

# adding linear model
weightfunct(df$ES, df$var, steps = c(.005, .025, .05, .50), mods = ~ df$Longerthan12weeks.c + 
              df$ ResearcherDevelopedMeasurement.c + df$LargeSample.c + 
              df$University.c + df$MajorityFemale.c + df$Strategy.c + df$Technology.c + 
              df$Feedback.c + df$PD.c + df$AcademicOutcome.c + df$MetacognitiveOutcome.c +
              df$MotivationalOutcome.c + df$EastAsia.c + df$After2019.c,
            weights = NULL, fe = FALSE, table = FALSE, pval = NULL)

## funnel plot
# use weightr's funnel
funnel(effect = df$ES, v = df$var, type = "se", flip = TRUE)

# Or ggplot
ggplot(df, aes(x = ES, y = se)) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = mean(df$ES), linetype = "dashed", color = "red") +
  geom_segment(aes(x = mean(df$ES) - 1.96*se, xend = mean(df$ES) + 1.96*se, y = se, yend = se),
               data = data.frame(se = seq(0, max(df$se), length.out = 100)),
               color = "gray", alpha = 0.3) +
  scale_y_reverse() +  # Reverse y-axis to match metafor's orientation
  labs(x = "Effect Size (ES)", y = "Standard Error (SE)", 
       title = "Funnel Plot") +
  theme_minimal()

# Or use metafor's funnel
library(metafor)
# Fit a random-effects model
meta_obj <- rma(yi = df$ES, vi = df$var, method = "REML")

metafor::funnel(meta_obj, 
       yaxis = "sei",           # Standard errors on y-axis
       xlab = "Effect Size",
       ylab = "Standard Error",
       level = c(90, 95, 99),   # Multiple contour levels (90%, 95%, 99% CI)
       shade = c("white", "darkgray", "gray"), # Different shading for each level
       refline = coef(meta_obj), # Reference line at the estimated effect
       pch = 1,                # Hollow circles
       col = "black",           # Black points
       back = "lightgray",          # White background
       lty = 3)                 # Dotted lines for contours

## trim and fill using metafor
# The default method is "R0" but you can also use "L0" or "Q0"
tf_result <- trimfill(meta_obj)

# Summarize the results
summary(tf_result)

sum(tf_result$fill)

# You can also look at the k0 value (estimated number of missing studies)
tf_result$k0

# Try different estimators
tf_result_L0 <- trimfill(meta_obj, estimator = "L0")
tf_result_Q0 <- trimfill(meta_obj, estimator = "Q0")

# Check how many studies were imputed with each method
sum(tf_result$fill)      # R0 method
sum(tf_result_L0$fill)   # L0 method
sum(tf_result_Q0$fill)   # Q0 method

# Create a funnel plot showing the trim and fill results
# This will display both the observed studies and the imputed studies
# Create enhanced funnel plot with trim and fill results
metafor::funnel(tf_result, 
                yaxis = "sei",
                xlab = "Effect Size", 
                ylab = "Standard Error",
                level = c(90, 95, 99),
                shade = c("white", "darkgray", "gray"),
                refline = coef(tf_result),
                pch = ifelse(tf_result$fill, 16, 1),  # This correctly assigns point types
                col = ifelse(tf_result$fill, "red", "black"),  # And colors
                back = "lightgray",
                lty = 3,
                legend = FALSE)
# Add custom legend with correct labels
legend("topright", 
       inset = c(0.01, 0.01),  # Adjust position as needed
       legend = c("90% CI", "95% CI", "99% CI", "Outside 99% CI"),
       fill = c("white", "darkgray", "gray", "lightgray"),
       border = "black",
       bg = "white",
       cex = 0.9,  # Adjust size as needed
       title = "Confidence Regions")

# Alternative approach using pch.fill parameter
metafor::funnel(tf_result, 
                yaxis = "sei",
                xlab = "Effect Size", 
                ylab = "Standard Error",
                level = c(90, 95, 99),
                shade = c("lightgray", "gray", "darkgray"),
                refline = coef(tf_result),
                # For observed studies
                pch = 1,
                col = "black",
                # For imputed studies  
                pch.fill = 19,  # Filled circles for imputed
                col.fill = "red",  # Red for imputed
                back = "white",
                lty = 3,
                legend = TRUE)

## density plot
test <- weightfunct(effect = df$ES, v = df$var, steps = c(.005, .025, .05, .50))
density(test)

# Or, if you want a custom density plot of the effect sizes:
library(ggplot2)
ggplot(data.frame(ES = df$ES), aes(x = ES)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density Plot of Effect Sizes",
       x = "Effect Size",
       y = "Density")


#################################################################################
# Percent Variance Explained
#################################################################################
##% explained by model
# Variation of first (smaller) model
tot_var_null <- sum(MVnull$sigma2) #total variation in effect sizes (omega2 + tau2)

# Variation of second (larger) model
tot_var_covs <- sum(MVfull$sigma2) #total variation in effect sizes after covariates

# difference between the two
perc_var_explained <- 100*(1-tot_var_covs/tot_var_null)
print(perc_var_explained) #R2 for model 

#################################################################################
# Heterogeneity
#################################################################################
# 95% prediction intervals
print(PI_upper <- round(MVfull$b[1] + (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2])), 2))
print(PI_lower <- round(MVfull$b[1] - (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2])), 2))

#################################################################################
# Empirical Bayes
#################################################################################

########################################################################################################
# Output
########################################################################################################

#################################################################################
# Create k's & n's  
#################################################################################

#################################################################################
# Format Output   
#################################################################################
# Descriptives Table
table_study_df$Category <- row.names(table_study_df)
rownames(table_study_df) <- c()
table_study_df <- table_study_df[c("Category", "level", "Overall")]
table_study_df$Category[which(substr(table_study_df$Category, 1, 1)=="X")] <- NA
table_study_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_study_df$Category)
table_study_df$Category[which(table_study_df$Category=="n")] <- "Total Studies"
table_study_df$Category[which(table_study_df$Category=="PubType")] <- "Publication Type"
table_study_df$level[which(table_study_df$level=="1")] <- "Yes"
table_study_df$level[which(table_study_df$level=="0")] <- "No"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_study_df$Category)) {
  if(is.na(table_study_df$Category[i])) {
    table_study_df$Category[i] <- table_study_df$Category[i-1]
  }
}

table_outcome_df$Category <- row.names(table_outcome_df)
rownames(table_outcome_df) <- c()
table_outcome_df <- table_outcome_df[c("Category", "level", "Overall")]
table_outcome_df$Category[which(substr(table_outcome_df$Category, 1, 1)=="X")] <- NA
table_outcome_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_outcome_df$Category)
table_outcome_df$Category[which(table_outcome_df$Category=="n")] <- "Total Effect Sizes"
table_outcome_df$level[which(table_outcome_df$level=="1")] <- "Yes"
table_outcome_df$level[which(table_outcome_df$level=="0")] <- "No"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_outcome_df$Category)) {
  if(is.na(table_outcome_df$Category[i])) {
    table_outcome_df$Category[i] <- table_outcome_df$Category[i-1]
  }
}

# MetaRegression Table
### Nothing to format

# Marginal Means Table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

#########################
#########################
#########################
# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(df[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(df[c("StudyID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]

#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# Descriptives Table
myreport <- body_add_par(x = myreport, value = "Table: Descriptive Statistics", style = "Normal")
descriptives_study <- flextable(head(table_study_df, n=nrow(table_study_df)))
descriptives_study <- add_header_lines(descriptives_study, values = c("Study Level"), top = FALSE)
descriptives_study <- theme_vanilla(descriptives_study)
descriptives_study <- merge_v(descriptives_study, j = c("Category"))
descriptives_study <- set_table_properties(descriptives_study, width = 1, layout = "autofit")
myreport <- body_add_flextable(x = myreport, descriptives_study, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = " ", style = "Normal")

descriptives_outcome <- flextable(head(table_outcome_df, n=nrow(table_outcome_df)))
descriptives_outcome <- delete_part(descriptives_outcome, part = "header")
descriptives_outcome <- add_header_lines(descriptives_outcome, values = c("Outcome Level"))
descriptives_outcome <- theme_vanilla(descriptives_outcome)
descriptives_outcome <- merge_v(descriptives_outcome, j = c("Category"))
descriptives_outcome <- set_table_properties(descriptives_outcome, width = 1, layout = "autofit")
myreport <- body_add_flextable(x = myreport, descriptives_outcome, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# MetaRegression Table
model_null <- flextable(head(MVnull.coef, n=nrow(MVnull.coef)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)
#myreport <- body_add_par(x = myreport, value = "", style = "Normal")

model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full <- colformat_double(model_full,  j = colkeys, digits = 2)
model_full <- colformat_double(model_full,  j = c("p_Satt"), digits = 3)
#model_full <- autofit(model_full)
model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression"))
model_full <- theme_vanilla(model_full)
model_full <- set_table_properties(model_full, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_full, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalmeans <- add_footer_lines(marginalmeans, tablenote, )
marginalmeans <- set_table_properties(marginalmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans, keepnext = FALSE)

# Heterogeneity Assessment
# 95% PI
myreport <- body_add_par(x = myreport, value = paste("95% PI: ", PI_lower, " to ", PI_upper, sep = ""), style = "Normal")
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Percent variance explained
myreport <- body_add_par(x = myreport, value = paste("Percent Variance Explained: ", round(perc_var_explained, 2), "%", sep = ""), style = "Normal")
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Write to word doc
file = paste("TableResults.docx", sep = "")
print(myreport, file)



# count method 1

#academic <- subset(df, df$MetacognitiveOutcome==1)
#academic <- academic[, c("Authors", "Year")]
#academic_unique <- academic %>%
#  distinct()

# Print the first few rows of the unique data
#print(academic_unique)

# count method 2

#count <- df %>%
#  filter(AcademicOutcome == 1) %>%
#  distinct(StudyID) %>%
#  nrow()

# print(paste("Count of unique StudyID with at least one AcademicOutcome equal to 1:", count))