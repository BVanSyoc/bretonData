library("tidyverse")
library("readxl")

kb <- read.table("https://raw.githubusercontent.com/BVanSyoc/bretonData/master/kbCleanedAll.txt",
                 sep = "\t", header = TRUE, stringsAsFactors = TRUE)
rr <- read.table("https://raw.githubusercontent.com/BVanSyoc/bretonData/master/rrCleanedAll.txt",
                 sep = "\t", header = TRUE, stringsAsFactors = TRUE)


# ---- Tttests ----



## ---- SumStats ----

### Summary statistics by sex
#kb summary table with lean Vo2 Seperated by sex also shows number of participants in each age group
kbsumStats <- kb %>% 
  mutate(Sex = factor(Sex),
         Participant = factor(Participant))%>% 
  group_by(Sex) %>% 
  summarize(n = length(unique(Participant)),
            avgLeanVO2 = round(mean(KB_VO2_Lean), 3),
            sdLeanVO2 = round(sd(KB_VO2_Lean), 3),
            nage18to29 = length(which(ageCats %in% "18 to 29")) / 10,
            nage30to39 = length(which(ageCats %in% "30 to 39")) / 10,
            nage40to49 = length(which(ageCats %in% "40 to 49")) / 10)
kbsumStats <- data.frame(kbsumStats)
kbsumStats
#kballtt <- t.test(KB_VO2_Lean ~ Sex, data = kb)
t1 <- t.test(x = kb$KB_VO2_Lean[kb$Sex == "M" & !kb$Bout == 1], y = kb$KB_VO2_Lean[kb$Sex == "F" & !kb$Bout == 1])
kballtt

cohD <- abs(kbsumStats$avgLeanVO2[1] - kbsumStats$avgLeanVO2[2]) / sqrt((kbsumStats$sdLeanVO2[1]^2 + kbsumStats$sdLeanVO2[2]^2) / 2)

# real runner summary stats
rrsumStats <- rr %>% 
  filter(!Bout == 1) %>% 
  mutate(Sex = factor(Sex),
         Participant = factor(Participant)) %>% 
  group_by(Sex) %>% 
  summarize(n = length(unique(Participant)),
            avgLeanVO2 = round(mean(RR_VO2_Lean), 3),
            sdLeanVO2 = round(sd(RR_VO2_Lean), 3), 
            nage18to29 = length(which(ageCats %in% "18 to 29")) / 8,
            nage30to39 = length(which(ageCats %in% "30 to 39")) / 8,
            nage40to49 = length(which(ageCats %in% "40 to 49")) / 8)
rrsumStats <- data.frame(rrsumStats)
rrsumStats

rralltt <- t.test(x = rr$RR_VO2_Lean[rr$Sex == "M" &!rr$Bout == 1], y = rr$RR_VO2_Lean[rr$Sex == "F" &!rr$Bout == 1])
rralltt

cohD <- abs(rrsumStats$avgLeanVO2[1] - rrsumStats$avgLeanVO2[2]) / sqrt((rrsumStats$sdLeanVO2[1]^2 + rrsumStats$sdLeanVO2[2]^2) / 2)
cohD

# ----sumStatsnoOuts----
# OUTLIERS: 2 standard deviations from the mean
# first define the outliers from the work we've already done
kbsds <- kb %>% 
  gather(key = Variable, value = value, KB_HR, KB_RPE, KB_VO2, KB_VO2_Lean, KB_RER) %>% 
  group_by(Variable) %>% 
  summarize(avg = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            avgplus2x = avg + 2*sd,
            avgminus2x = avg - 2*sd)
# make dataframe without outliers
kbshowOuts <- kb %>% # look within kb
  # make vertical
  gather(key = "Variable", value = value, 
         KB_HR, KB_RPE, KB_VO2, KB_VO2_Lean, KB_RER) %>%  
  #calculate sd
  merge(kbsds, by = "Variable")
# quick "for" loop to determine who is in and out
kbOuts <- "NA"
for(i in 1:nrow(kbshowOuts)) {
  kbshowOuts[i, "Outs"] <- ifelse(kbshowOuts$value[i] < kbshowOuts$avgminus2x[i] |
                                    kbshowOuts$value[i] > kbshowOuts$avgplus2x[i],
                                  "Out",
                                  "Stay")
}

# remove outliers
kbNoOuts <- kbshowOuts %>% 
  filter(Outs == "Stay") %>% 
  mutate(avg = NULL,
         sd = NULL,
         avgplus2x = NULL,
         avgminus2x = NULL,
         Outs = NULL) %>% 
  spread(key = Variable, value = value)

# show just the  outliers
kbOuts <- kbshowOuts %>% 
  filter(Outs == "Out") %>% 
  mutate(avg = NULL,
         sd = NULL,
         avgplus2x = NULL,
         avgminus2x = NULL,
         Outs = NULL) %>% 
  spread(key = Variable, value = value)
percb1 <- length(which(kbOuts$Bout == 1))/nrow(kbOuts)*100

kb$ageCats <- factor(kb$ageCats)#created age cats a factor 
kbsumStatsnoOuts <- kbNoOuts %>%   
  mutate(Sex = factor(Sex),
         Participant = factor(Participant))%>% 
  group_by(Sex) %>% 
  summarize(n = length(unique(Participant)),
            avgLeanVO2 = round(mean(KB_VO2_Lean,na.rm = TRUE), 3),
            sdLeanVO2 = round(sd(KB_VO2_Lean,na.rm = TRUE), 3))
kbsumStatsnoOuts

kballttOuts <- t.test(x = kbNoOuts$KB_VO2_Lean[kbNoOuts$Sex == "M"], y = kbNoOuts$KB_VO2_Lean[kbNoOuts$Sex == "F"])
kballttOuts

#now for real runner

# OUTLIERS: 2 standard deviations from the mean
# first define the outliers from the work we've already done
rrsds <- rr %>% 
  gather(key = Variable, value = value, RR_HR, RR_RPE, RR_VO2, RR_VO2_Lean, RR_RER) %>% 
  group_by(Variable) %>% 
  summarize(avg = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            avgplus2x = avg + 2*sd,
            avgminus2x = avg - 2*sd)
# make dataframe without outliers
rrshowOuts <- rr %>% # look within rr
  # make vertical
  gather(key = "Variable", value = value, 
         RR_HR, RR_RPE, RR_VO2, RR_VO2_Lean, RR_RER) %>%  
  #calculate sd
  merge(rrsds, by = "Variable")
# quick "for" loop to determine who is in and out
rrOuts <- "NA"
for(i in 1:nrow(rrshowOuts)) {
  rrshowOuts[i, "Outs"] <- ifelse(rrshowOuts$value[i] < rrshowOuts$avgminus2x[i] |
                                    rrshowOuts$value[i] > rrshowOuts$avgplus2x[i],
                                  "Out",
                                  "Stay")
}

# remove outliers
rrNoOuts <- rrshowOuts %>% 
  filter(Outs == "Stay") %>% 
  mutate(avg = NULL,
         sd = NULL,
         avgplus2x = NULL,
         avgminus2x = NULL,
         Outs = NULL) %>% 
  spread(key = Variable, value = value)

# show just the  outliers
rrOuts <- rrshowOuts %>% 
  filter(Outs == "Out") %>% 
  mutate(avg = NULL,
         sd = NULL,
         avgplus2x = NULL,
         avgminus2x = NULL,
         Outs = NULL) %>% 
  spread(key = Variable, value = value)

percb2 <- length(which(rrOuts$Bout == 1))/nrow(rrOuts)*100

rr$ageCats <- factor(rr$ageCats)#created age cats a factor 
rrsumStatsnoOuts <- rrNoOuts %>%   
  mutate(Sex = factor(Sex),
         Participant = factor(Participant))%>% 
  group_by(Sex) %>% 
  summarize(n = length(unique(Participant)),
            avgLeanVO2 = round(mean(RR_VO2_Lean,na.rm = TRUE), 3),
            sdLeanVO2 = round(sd(RR_VO2_Lean,na.rm = TRUE), 3))
rrsumStatsnoOuts

rrallttOuts <- t.test(x = rrNoOuts$RR_VO2_Lean[rrNoOuts$Sex == "M"], y = rrNoOuts$RR_VO2_Lean[rrNoOuts$Sex == "F"])
rrallttOuts

## Anova of Bouts
kb$Bout <- factor(kb$Bout)
kbbdif <- aov(KB_VO2_Lean ~ Bout, data = kb)
summary(kbbdif)
kbbdif
pvals <- TukeyHSD(kbbdif)
# subset for significant variables
sigs <- pvals$Bout[which(pvals$Bout[, "p adj"] < 0.05),]
boxplot(KB_VO2_Lean ~ Bout, data = kb)

rr$Bout <- factor(rr$Bout)
rrmbdif <- aov(RR_VO2_Lean ~ Bout, data = filter(rr, Sex == "M"))
summary(rrmbdif)
rrmbdif
pvalsrr <- TukeyHSD(rrmbdif)
# subset for significant variables
sigsrr <- pvalsrr$Bout[which(pvalsrr$Bout[, "p adj"] < 0.05),]
boxplot(RR_VO2_Lean ~ Bout, data = filter(rr, Sex == "M"), main="VO2 lean vs Bout, RR Males")



rr$Bout <- factor(rr$Bout)
rrfbdif <- aov(RR_VO2_Lean ~ Bout, data = filter(rr, Sex == "F"))
summary(rrfbdif)
rrfbdif
TukeyHSD(rrfbdif)
pvalsrrf <- TukeyHSD(rrfbdif)
# subset for significant variables
sigsrrf <- pvalsrrf$Bout[which(pvalsrrf$Bout[, "p adj"] < 0.05),]
boxplot(RR_VO2_Lean ~ Bout, data = filter(rr, Sex == "F"), main = "Vo2Lean vs Bout, Females only")

rr$Bout <- factor(rr$Bout)
rrbdif <- aov(RR_VO2_Lean ~ Bout, data = rr)
summary(rrbdif)
rrbdif
TukeyHSD(rrbdif)

pvalsrrdif <- TukeyHSD(rrbdif)
# subset for significant variables
sigsrr <- pvalsrrdif$Bout[which(pvalsrrdif$Bout[, "p adj"] < 0.05),]
boxplot(RR_VO2_Lean ~ Bout, data = rr, main = "VO2Lean vs Bout, RR all data")

## ---- removeBoutone---

kbb2t10 <- filter(kb, !Bout == 1)
kbsumStats2to10 <- kbb2t10 %>%   
  mutate(Sex = factor(Sex),
         Participant = factor(Participant))%>% 
  group_by(Sex) %>% 
  summarize(n = length(unique(Participant)),
            avgLeanVO2 = round(mean(KB_VO2_Lean), 3),
            sdLeanVO2 = round(sd(KB_VO2_Lean), 3))
kb2to10tt <- t.test(x = kbb2t10$KB_VO2_Lean[kbb2t10$Sex == "M"], y = kbb2t10$KB_VO2_Lean[kbb2t10$Sex == "F"])
kb2to10tt

kbsdsb1 <- kbb2t10 %>% 
  gather(key = Variable, value = value, KB_HR, KB_RPE, KB_VO2, KB_VO2_Lean, KB_RER) %>% 
  group_by(Variable) %>% 
  summarize(avg = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            avgplus2x = avg + 2*sd,
            avgminus2x = avg - 2*sd)
kbshowOutsb1 <- kbb2t10 %>% # look within kb
  # make vertical
  gather(key = "Variable", value = value, 
         KB_HR, KB_RPE, KB_VO2, KB_VO2_Lean, KB_RER) %>%  
  #calculate sd
  merge(kbsdsb1, by = "Variable")
# quick "for" loop to determine who is in and out
kbOuts <- "NA"
for(i in 1:nrow(kbshowOuts)) {
  kbshowOutsb1[i, "Outs"] <- ifelse(kbshowOutsb1$value[i] < kbshowOutsb1$avgminus2x[i] |
                                    kbshowOutsb1$value[i] > kbshowOutsb1$avgplus2x[i],
                                  "Out",
                                  "Stay")}

kbNoOutsb1 <- kbshowOutsb1 %>% 
  filter(Outs == "Stay") %>% 
  mutate(avg = NULL,
         sd = NULL,
         avgplus2x = NULL,
         avgminus2x = NULL,
         Outs = NULL) %>% 
  spread(key = Variable, value = value)

# show just the  outliers
kbOutsb1 <- kbshowOutsb1 %>% 
  filter(Outs == "Out") %>% 
  mutate(avg = NULL,
         sd = NULL,
         avgplus2x = NULL,
         avgminus2x = NULL,
         Outs = NULL) %>% 
  spread(key = Variable, value = value)

kb$ageCats <- factor(kb$ageCats)#created age cats a factor 
kbsumStatsnoOutsb1 <- kbNoOutsb1 %>%   
  mutate(Sex = factor(Sex),
         Participant = factor(Participant))%>% 
  group_by(Sex) %>% 
  summarize(n = length(unique(Participant)),
            avgLeanVO2 = round(mean(KB_VO2_Lean,na.rm = TRUE), 3),
            sdLeanVO2 = round(sd(KB_VO2_Lean,na.rm = TRUE), 3))
kbsumStatsnoOutsb1

kbb1Outstt <- t.test(x = kbOutsb1$KB_VO2_Lean[kbOutsb1$Sex == "M"], y = kbOutsb1$KB_VO2_Lean[kbOutsb1$Sex == "F"])
kbb1Outstt

## T Test power

## COHENs D FOR EFFECT SIZE: MEAN1 - MEAN2 / ALL SD

cohend <- cohen.d(d = kb$KB_VO2_Lean, f = kb$Sex, conf.level = 0.95)

manual.cohend <- abs(kbsumStats$avgLeanVO2[kbsumStats$Sex == "F"] - kbsumStats$avgLeanVO2[kbsumStats$Sex == "M"]) / sd(kb$KB_VO2_Lean)

powerKB <- pwr.t.test(n = nrow(kb), d = manual.cohend, sig.level = 0.05, power = NULL)

#Regressions
plot(KB_HR ~ KB_RPE, data = kb)
lmkb <- lm(formula = KB_HR ~ KB_RPE, data = kb)
cor(kb$KB_HR, kb$KB_RPE)

## ---- Regressions ----

## KETTLEBELL
kbReg <- lm(KB_HR ~ KB_RPE, data = kb)
sumkb <- summary(kbReg)
sumkb
equKb <- paste0("y = ", round(kbReg$coefficients[2], 2), "x + ", round(kbReg$coefficients[1], 2))
equKb
rkb <- round(sumkb$r.squared, 2)
plot(KB_HR ~ KB_RPE, data = kb, main = "HR vs RPE kettlebells, all data")
abline(lm(KB_HR ~ KB_RPE, data = kb), col = "red")
text(x = 9, y = 190, label = equKb)
text(x = 9, y = 180, label = paste0("r2 = ", rkb))

# see if a curved line looks better
ggplot(data = kb, aes(x = KB_RPE, y = KB_HR)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  ggtitle("HR vs RPE kettlebells all data - curved fit")

## REAL RUNNER
rrReg <- lm(RR_HR ~ RR_RPE, data = rr)
sumrr <- summary(rrReg)
sumrr
equRr <- paste0("y = ", round(rrReg$coefficients[2], 2), "x + ", round(rrReg$coefficients[1], 2))
equRr
rRr <- round(sumrr$r.squared, 2)
plot(RR_HR ~ RR_RPE, data = rr, main = "HR vs RPE real runner, all data")
abline(lm(RR_HR ~ RR_RPE, data = rr), col = "red")
text(x = 8, y = 190, label = equRr)
text(x = 8, y = 180, label = paste0("r2 = ", rRr))

# see if a curved line looks better
ggplot(data = rr, aes(x = RR_RPE, y = RR_HR)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  ggtitle("HR vs RPE real runner all data - curved fit")

## plot each participant on their own line
kb$Participant <- factor(kb$Participant)
ggplot(data = kb, aes(x = KB_RPE, y = KB_HR, group = Participant, color = Participant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Sex) +
  ggtitle("Individual regression lines - kettlebells")

rr$Participant <- factor(rr$Participant)
ggplot(data = rr, aes(x = RR_RPE, y = RR_HR, group = Participant, color = Participant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Sex) +
  ggtitle("individual regression lines - real runner")

## GET INDIVIDUAL SLOPES
kbSlopes <- kb %>% 
  group_by(Participant) %>% 
  summarize(
    slope = round(lm(KB_HR ~ KB_RPE)$coefficients[2], 2),
    significance = summary(lm(KB_HR ~ KB_RPE))$coefficients[2,4],
    x = mean(KB_HR),
    y = mean(KB_RPE)
  )
kbSlopes <- data.frame(kbSlopes)
kbSlopes

rrSlopes <- rr %>% 
  group_by(Participant) %>% 
  summarize(
    slope = round(lm(RR_HR ~ RR_RPE)$coefficients[2], 2),
    significance = summary(lm(RR_HR ~ RR_RPE))$coefficients[2,4],
    x = mean(RR_HR),
    y = mean(RR_RPE)
  )
rrSlopes <- data.frame(rrSlopes)
rrSlopes
## ---- Regressions minus Bout1 ----

kbNo1 <- kb %>% filter(!Bout == 1)

## KETTLEBELL
kbNo1Reg <- lm(KB_HR ~ KB_RPE, data = kbNo1)
sumkbNo1 <- summary(kbNo1Reg)
sumkbNo1
equKb <- paste0("y = ", round(kbNo1Reg$coefficients[2], 2), "x + ", round(kbNo1Reg$coefficients[1], 2))
equKb
rkbNo1 <- round(sumkbNo1$r.squared, 2)
plot(KB_HR ~ KB_RPE, data = kbNo1, main = "HR vs RPE kettlebells, no Bout 1")
abline(lm(KB_HR ~ KB_RPE, data = kbNo1), col = "red")
text(x = 9, y = 190, label = equKb)
text(x = 9, y = 180, label = paste0("r2 = ", rkbNo1))

# see if a curved line looks better
ggplot(data = kbNo1, aes(x = KB_RPE, y = KB_HR)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  ggtitle("HR vs RPE kettlebells no bout 1 - curved fit")

## REAL RUNNER
rrNo1 <- rr %>% filter(!Bout == 1)
rrNo1Reg <- lm(RR_HR ~ RR_RPE, data = rrNo1)
sumrrNo1 <- summary(rrNo1Reg)
sumrrNo1
equRr <- paste0("y = ", round(rrNo1Reg$coefficients[2], 2), "x + ", round(rrNo1Reg$coefficients[1], 2))
equRr
rRr <- round(sumrrNo1$r.squared, 2)
plot(RR_HR ~ RR_RPE, data = rrNo1, main = "HR vs RPE real runner, no bout 1")
abline(lm(RR_HR ~ RR_RPE, data = rrNo1), col = "red")
text(x = 8, y = 190, label = equRr)
text(x = 8, y = 180, label = paste0("r2 = ", rRr))

# see if a curved line looks better
ggplot(data = rrNo1, aes(x = RR_RPE, y = RR_HR)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  ggtitle("HR vs RPE real runner no bout 1 - curved fit")

### ---- GENERAL ANOVA MODEL ----

kb$Age <- factor(kb$Age)
allkb <- aov(KB_HR ~ ageCats + Sex + Bout, data = kb)
summary(allkb)
t <- TukeyHSD(allkb)
# significant Bouts
sigBouts <- t$Bout[which(t$Bout[, "p adj"] < 0.05),]

# visualize all of these on a ggplot
age <- ggplot(data = kb, aes(x = ageCats, y = KB_HR)) +
  geom_boxplot()
sex <- ggplot(data = kb, aes(x = Sex, y = KB_HR)) +
  geom_boxplot()
bout <- ggplot(data = kb, aes(x = Bout, y = KB_HR)) +
  geom_boxplot()
library(gridExtra)
grid.arrange(age, sex, bout, ncol = 3)

## ---- rmCorr ----
library('rmcorr')

kbrmc <- rmcorr(participant = Participant,
                measure1 = KB_HR,
                measure2 = KB_RPE, 
                dataset = filter(kb, !Bout == 1),
                CIs = "analytic")
kbrmc
ggplot(filter(kb, !Bout == 1), aes(x = KB_HR, y = KB_RPE, group = Participant, color = Participant, scales = "free")) +
  geom_point() +
  geom_line(aes(y = kbrmc$model$fitted.values), linetype = 1) +
  theme_bw()

rrrmc <- rmcorr(participant = Participant,
                measure1 = RR_HR,
                measure2 = RR_RPE, 
                dataset = filter(rr, !Bout == 1),
                CIs = "analytic",)
rrrmc


ggplot(filter(rr, !Bout == 1), aes(x = RR_HR, y = RR_RPE, group = Participant, color = Participant, scales = "free")) +
  geom_point() +
  geom_line(aes(y = rrrmc$model$fitted.values), linetype = 1) +
  theme_bw() 

## ---- BlandAltman ----

# make columns for the means and differences
kbBA <- kb %>% 
  mutate(RPE10 = KB_RPE * 10,
         avgHRRPE = (KB_HR + RPE10) / 2,
         diff = KB_HR - RPE10)

kbBA <- filter(kbBA, !Bout == 1)
plot(x = kbBA$avgHRRPE, y = kbBA$diff, main = "Bland Altman: kettlebells, no bout1")
abline(h = mean(kbBA$diff), col = "red")
abline(h = mean(kbBA$diff) + 2*sd(kbBA$diff), col = "blue")
abline(h = mean(kbBA$diff) - 2*sd(kbBA$diff), col = "blue")
text(x = 110, y = 70, label = paste0("mean: ", round(mean(kbBA$diff), 2)))
text(x = 110, y = 0, labels = paste0("CIHigh: ", round(mean(kbBA$diff) + 2*sd(kbBA$diff), 2)))
text(x = 110, y = -10, labels = paste0("CILow:", round(mean(kbBA$diff) - 2*sd(kbBA$diff), 2)))

## REAL RUNNER
rrBA <- rr %>% 
  mutate(RPE10 = RR_RPE * 10,
         avgHRRPE = (RR_HR + RPE10) /2,
         diff = RR_HR - RPE10)

plot(x = rrBA$avgHRRPE, y = rrBA$diff, main = "Bland Altman: real runner all data")
abline(h = mean(rrBA$diff), col = "red")
abline(h = mean(rrBA$diff) + 2*sd(rrBA$diff), col = "blue")
abline(h = mean(rrBA$diff) - 2*sd(rrBA$diff), col = "blue")
text(x = 90, y = 90, label = paste0("mean: ", round(mean(rrBA$diff), 2)))
text(x = 90, y = 0, labels = paste0("CIHigh: ", round(mean(rrBA$diff) + 2*sd(rrBA$diff), 2)))
text(x = 90, y = -10, labels = paste0("CILow:", round(mean(rrBA$diff) - 2*sd(rrBA$diff), 2)))

rrBA <- filter(rrBA, !Bout == 1)
plot(x = rrBA$avgHRRPE, y = rrBA$diff, main = "Bland Altman: real runner no bout 1")
abline(h = mean(rrBA$diff), col = "red")
abline(h = mean(rrBA$diff) + 2*sd(rrBA$diff), col = "blue")
abline(h = mean(rrBA$diff) - 2*sd(rrBA$diff), col = "blue")
text(x = 100, y = 90, label = paste0("mean: ", round(mean(rrBA$diff), 2)))
text(x = 100, y = 0, labels = paste0("CIHigh: ", round(mean(rrBA$diff) + 2*sd(rrBA$diff), 2)))
text(x = 100, y = -10, labels = paste0("CILow:", round(mean(rrBA$diff) - 2*sd(rrBA$diff), 2)))


kbBA <- kb %>%  
  filter(!Bout == 1) %>% 
  group_by(Participant) %>% 
  summarize(avgHR = mean(KB_HR),
            avgRPE10 = mean(KB_RPE * 10)) %>% 
  mutate(avgHRRPE =(avgHR + avgRPE10) /2,
         diff = avgHR - avgRPE10)

#kbBA <- filter(kbBA, !Bout == 1)
plot(x = kbBA$avgHRRPE, y = kbBA$diff, main = "Bland Altman: kettlebells, all data avg by Participant no bout 1")
abline(h = mean(kbBA$diff), col = "red")
abline(h = mean(kbBA$diff) + 2*sd(kbBA$diff), col = "blue")
abline(h = mean(kbBA$diff) - 2*sd(kbBA$diff), col = "blue")
text(x = 120, y = 55, label = paste0("mean: ", round(mean(kbBA$diff), 2)))
text(x = 120, y = 0, labels = paste0("CIHigh: ", round(mean(kbBA$diff) + 2*sd(kbBA$diff), 2)))
text(x = 120, y = -5, labels = paste0("CILow:", round(mean(kbBA$diff) - 2*sd(kbBA$diff), 2)))


rrBA <- rr %>%  
  filter(!Bout == 1) %>% 
  group_by(Participant) %>% 
  summarize(avgHR = mean(RR_HR),
            avgRPE10 = mean(RR_RPE * 10)) %>% 
  mutate(avgHRRPE =(avgHR + avgRPE10) /2,
         diff = avgHR - avgRPE10)

#rrBA <- filter(rrBA, !Bout == 1)
plot(x = rrBA$avgHRRPE, y = rrBA$diff, main = "Bland Altman: real runner, all data avg by Participant no bout 1")
abline(h = mean(rrBA$diff), col = "red")
abline(h = mean(rrBA$diff) + 2*sd(rrBA$diff), col = "blue")
abline(h = mean(rrBA$diff) - 2*sd(rrBA$diff), col = "blue")
text(x = 125, y = 55, label = paste0("mean: ", round(mean(rrBA$diff), 2)))
text(x = 125, y = 0, labels = paste0("CIHigh: ", round(mean(rrBA$diff) + 2*sd(rrBA$diff), 2)))
text(x = 125, y = -5, labels = paste0("CILow:", round(mean(rrBA$diff) - 2*sd(rrBA$diff), 2)))
