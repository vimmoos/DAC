library(tidyverse)

## Question 1
dat <-  as_tibble(read.table("./decision.dat", header= T))
## %>%
##     mutate_at(vars(isDots,cohFac),factor)

subj8 <- dat[dat$subjNo==8,]


model <- glm(ER ~ RT + isDots + isLeft + cohFac + blocknum,
             data = subj8,
             family = binomial(link="logit"))

odds <- exp(cbind(Odds_Ratio=coef(model), confint(model)))
an <- anova (model,test="Chisq")

jpeg (file ="resvslev.jpg")
plot (model,5)
dev.off ()

## Question 2
library(ROCR)
p <- predict (model,type ="response")
pr <- prediction (p,subj8$ER)

prf <- performance(pr,measure = "tpr",x.measure = "fpr")

jpeg (file ="roc.jpg")
plot (prf)
dev.off ()

auc <- performance (pr,measure= "auc")
auc <- auc@y.values [[1]]

## Question 3
library (MASS)
 mode_complicated<- glm(formula = ER ~ RT + isDots + cohFac + isLeft +
                        blocknum + isDots * cohFac,
                        family = binomial(link = "logit"),
                        data = subj8)
model.step <- stepAIC (model,trace=3)


library(corrplot)
cor <- cor(dplyr::select(subj8,RT,isLeft,cohFac,blocknum,isDots))
subj8.sel <- dplyr::select(subj8,RT,isLeft,cohFac,blocknum,isDots)


corrplot.mixed(cor, lower =  "number",
                    upper = "circle",
                    tl.col = "black"
                    )
library (ggplot2)
library (GGally)
ggcorr (subj8.sel,label=TRUE,label_round = 3)

ggsave("corr.jpg")
