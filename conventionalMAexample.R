##########preparation
#install metafor and tidyverse. Only required once. 
install.packages("metafor")
install.packages("tidyverse")
#load metafor and tidyverse(required every time you want to use it)
library(metafor)
library(tidyverse)
#name data file and read in .csv 
dat1 <- read.csv("SC sample data.csv")
#calculate overall effect size, in this case standardized mean difference (Hedges g), and variance. 
dat1 <- escalc(measure="SMD", m1i=intmean, sd1i=intsd, n1i=intn,
               m2i=cmean, sd2i=csd, n2i=cn, data=dat1)
#display dataset with effect size and variance
dat1



##########analyses

#run overall random effects meta-analysis
overallresult <- rma(yi, vi, data=dat1)
#display overall result
overallresult

#forest plot
forest.rma(overallresult, slab = dat1$studyauthor, header="Study")


##check for influence
#influence analysis
influence(overallresult)


##categorical moderator

#moderator test to calculate qbetween value for categorical moderator
mod.gradeq <- rma(yi, vi, mods = ~ factor(graderange), data=dat1)
mod.gradeq

# Save QM Test and write it into a text file
Qgrade_collapsed_string <- paste(mod.gradeq[["QMdf"]])
Qgrade_type1 <- data.frame(CollapsedQMdf = Qgrade_collapsed_string)
Qgrade_type2 <- round(mod.gradeq$QM,2)
Qgrade_type3 <- round(mod.gradeq$QMp,3)

QgradeQ <- paste(
  "Qb(",Qgrade_collapsed_string,") =", 
  Qgrade_type2,
  ", p =", 
  Qgrade_type3,
  collapse = " "
)

cat(QgradeQ, "\n")
gradeQtest <- data.frame(Text = QgradeQ)
write.table(gradeQtest, file = "QgradeQ.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

#moderator test to get mean effect size for each group categorical moderator (removes intercept)
mod.grade <- rma(yi, vi, mods = ~ factor(graderange)-1, data=dat1)
#Display moderator result
mod.grade

##save moderator results to table
#Only save table results to new data item
mod.grade_table <-coef(summary(mod.grade))
#calculate participants in each group and add it to the table, then save the table.
gradeSumParticipants <- dat1 %>%
  group_by(graderange) %>%
  summarise(nexp = sum(intn, na.rm = TRUE),
            nctrl = sum(cn, na.rm = TRUE))
gradeNumComp <- dat1 %>%
  count(graderange)
gradeNumComp <- rename(gradeNumComp, kcomparisons = n)
grade_type_table.final<- cbind(gradeSumParticipants,gradeNumComp[c(2)], mod.grade_table) 
write.csv(grade_type_table.final, "Mod.gradeResult.csv")


##continuous moderator
#continuous moderator test
mod.cont <- rma(yi, vi, mods = ~ cont, data=dat1)
mod.cont




#########publication bias analyses

#standard funnel plot
funnel(overallresult)
# carry out trim-and-fill analysis
trimandfill <- trimfill(overallresult)
trimandfill
funnel(trimandfill)
#Eggers regression
regtest(overallresult)
#Rosenthal, Orwin, & Rosenberg Fail Safe N test 
fsn(yi, vi, data=dat1)
fsn(yi, vi, data=dat1, type="Orwin")
fsn(yi, vi, data=dat1, type="Rosenberg")