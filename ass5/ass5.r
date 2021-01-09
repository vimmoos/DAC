library(tidyverse)
dat <- read.table ("./decision.dat",header=TRUE) %>% as_tibble
subj1 <- dat %>% filter(subjNo == 1 & isDots == 1 , ER == 0)
subj1 <- subj1[,c("RT","cohFac","blocknum")]

avgsubj1 <- subj1 %>% group_by(cohFac,blocknum) %>% summarise_all(mean)

cohFact1 <- subj1[subj1$cohFac == 1,"RT"][[1]]
cohFact0 <- subj1[subj1$cohFac == 0,"RT"][[1]]


t.test(RT~cohFac,avgsubj1,paired =  TRUE)
t.test(RT~cohFac,subj1)
t.test(sample(cohFact0,400),sample(cohFact1,400),paired =  TRUE)

onlyDotsAll <- dat[dat$isDots == 1 & dat$ER == 0,]
avByBlock <- onlyDotsAll %>%
    group_by(blocknum,subjNo) %>% summarise_all(mean)
avByBlock <- avByBlock[,c("RT","cohFac","blocknum","subjNo","isLeft")]

t.test(RT~cohFac,avByBlock,paired =  TRUE)

library(MASS)
wilcox.test(RT~cohFac,avByBlock,paired =  TRUE)

aov(RT~cohFac,subj1) %>% summary
png(file="difference_bplot.png")
boxplot(RT~cohFac,data=subj1, main="Effect of coherence on RT",
        names = c("low", "high"),
        xlab="coherence", ylab="response time")
dev.off()

library(lsr)
aov(RT~cohFac,data = subj1) %>% etaSquared

aov(RT~cohFac*isLeft,data = onlyDotsAll) %>% summary

png(file="interaction_plot.png",width =  800)
interaction.plot(onlyDotsAll$cohFac,onlyDotsAll$isLeft,onlyDotsAll$RT,
                             ylab = "mean of response time",
                             xlab =  "coherence level",
                             xaxt = "n")
axis(1,
     at = 1:2,
     labels = c("low","high"))
dev.off()

aov(RT~cohFac*isLeft,data = onlyDotsAll) %>% etaSquared


library(rstatix)

avgbycondition <- onlyDotsAll %>%
    group_by(cohFac,isLeft,subjNo) %>% summarise_all(mean) %>% ungroup
avgbycondition <- avgbycondition[,c("RT","cohFac","subjNo","isLeft")]

aov <-
    anova_test(data = avgbycondition, dv = RT ,wid= subjNo,
                  within = c(cohFac,isLeft))
