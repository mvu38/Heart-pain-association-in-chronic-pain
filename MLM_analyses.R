library(lmerTest)
library(ggplot2)

# read data
data <- read.csv("pain-heart rate association 60 days.csv")

# MLM as in Dudarev et al., 2022
pubm1 <- lmer(pain ~ day_from_start_norm + averageBPM + averageHRV + pain_yesterday + (1|pp), data=data)
summary(pubm1)
confint(pubm1)
plot_model(pubm1,type="pred",show.data=TRUE,colors="Accent")

# MLM as in Dudarev et al., 2022, with symptomatic pain medication
pubm1pk <- lmer(pain ~ day_from_start_norm + averageBPM*PK_prop + averageHRV * PK_prop + pain_yesterday + (1|pp), data=data)
summary(pubm1pk)

# MLM as in Dudarev et al., 2022, with type of chronic pain (effect coded)
pubm1pt2 <- lmer(pain ~ averageBPM * paintype_ef + averageHRV * paintype_ef + day_from_start_norm + pain_yesterday + (1|pp), data=data)
summary(pubm1pt2)


# preregistered analysis
pm1 <- lmer(pain~ averageBPM * day_from_start_norm * PK_prop * paintype_ef + (1|pp), data=data)
summary(pm1)


# pain as predictor
bpm <- lmer(averageBPM ~ day_from_start_norm + pain_yesterday + avBPMyesterday + averageHRV + (1|pp),data=data)
summary(bpm)

hrv <- lmer(averageHRV ~ day_from_start_norm + pain_yesterday + avHRVyesterday + averageBPM + (1|pp),data=data)
summary(hrv)

# pain as predictor, as preregistered
bpm_prereg <- lmer(averageBPM ~  pain_yesterday * PK_prop * paintype_ef * day_from_start_norm + (1|pp),data=data)
summary(bpm_prereg)

hrv_prereg <- lmer(averageHRV ~ pain_yesterday * PK_prop * paintype_ef * day_from_start_norm + (1|pp),data=data)
summary(hrv_prereg)


####################################################################### exclude cases
# remove cases 79167, 79183, 79211, (surgeries during the study)
# and cases 79179, 79259, 79303, 79347, 79187, 79339, 81175, 82191,79172 (inj/surj before the study)
data_pp_excluded <- data[which(data$LDC_ID!= 79167 & data$LDC_ID!= 79183 & data$LDC_ID!= 19211 & 
                              data$LDC_ID!= 79179 & data$LDC_ID!= 79259 & data$LDC_ID!= 79303 & 
                              data$LDC_ID!= 79347 & data$LDC_ID!= 79187 & data$LDC_ID!=79339 &
                              data$LDC_ID!= 81175 & data$LDC_ID!= 72191 & data$LDC_ID!= 79172), ]


# MLM as in Dudarev et al., 2022
pubm1 <- lmer(pain ~ day_from_start_norm + averageBPM + averageHRV + pain_yesterday + (1|pp), data=data_pp_excluded)
summary(pubm1)

# # MLM as in Dudarev et al., 2022 with painkillers
pubm1pk <- lmer(pain ~ day_from_start_norm + averageBPM*PK_prop + averageHRV * PK_prop + pain_yesterday + (1|pp), data=data_pp_excluded)
summary(pubm1pk)

# # MLM as in Dudarev et al., 2022 with pain type
pubm1pt <- lmer(pain ~ averageBPM * paintype_f * day_from_start_norm + pain_yesterday + (1|pp), data=data_pp_excluded)
summary(pubm1pt)

# preregistered
pm1 <- lmer(pain~ averageBPM * day_from_start_norm * PK_prop * paintype_f + (1|pp), data=data_pp_excluded)
summary(pm1)
plot_model(pm1, type="int", show.data=TRUE,colors="Dark2")

# pain as predictor
bpm <- lmer(averageBPM ~ day_from_start_norm + pain_yesterday + avBPMyesterday + averageHRV + (1|pp),data=data_pp_excluded)
summary(bpm)

hrv <- lmer(averageHRV ~ day_from_start_norm + pain_yesterday + avHRVyesterday + averageBPM + (1|pp),data=data_pp_excluded)
summary(hrv)

# pain as predictor, as preregistered
bpm_prereg <- lmer(averageBPM ~ day_from_start_norm + pain_yesterday * PK_prop * paintype_ef + (1|pp),data=data_pp_excluded)
summary(bpm_prereg)

hrv_prereg <- lmer(averageHRV ~ day_from_start_norm + pain_yesterday * PK_prop * paintype_ef + (1|pp),data=data_pp_excluded)
summary(hrv_prereg)