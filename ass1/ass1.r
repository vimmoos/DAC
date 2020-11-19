
library(tidyverse)
library(ggplot2)
dat <- read.csv('disruptions-2019-Q4.csv')
dat.tib <- as_tibble(dat)


hours <- dat.tib %>%
    mutate(duration_hours= duration_minutes / 60)


count(hours,cause_group)

known_causes <- filter(hours,cause_group != "" & cause_group != "unknown" ) %>% filter(duration_hours < 20)

ggplot(known_causes,aes(duration_hours))+
    geom_histogram(binwidth = 1)

ggsave("known_causes.jpg")


avg_delay_causes <- group_by(known_causes,cause_group) %>%
    summarize(avg=mean(duration_hours))

ggplot(avg_delay_causes,aes(cause_group,avg)) +
    geom_bar(stat="identity")

ggsave("avg_delay_causes.jpg")

## table4a wrong because missing '' arround the numebers ... explain what gather does

people <-
    tribble(
        ~name,~key,~value,
        "Phillip Woods", "age", 45,
        "Phillip Woods","height",186,
        "Phillip Woods","age", 50,
        "Jessica Cordero","age",37,
        "Jessica Cordero","height",156)

## the operation spread on people fails because there is no unique combination of keys which indentify uniquely each row

tibble(x =c("a,b,c", "d,e,f,g", "h,i,j")) %>%
    separate(x,c("one", "two", "three"),extra="merge")

tibble(x =c("a,b,c", "d,e", "f,g,i")) %>%
    separate(x,c("one", "two", "three"),fill="left")


## res <- tibble (act_r = c( 0.58,
##                         0.77,
##                         0.95,
##                         1.14,
##            1.33,
##                         1.51,
##                         1.70,
##                         1.88,
##                         2.07,
##            2.25),
##            human =c ( 0.60,
##                       0.65,
##                       0.70,
##                       0.86,
##                       1.12,
##                       1.50,
##                       1.79,
##                       2.13,
##                       2.15,
##                       2.5))

## ggplot(res,aes(x=c(1,2,3,4,5,6,7,8,9,10),fill="dio"))+
##     geom_point(aes(y=res$human,fill="human"),color='black',fill="human")+
##     geom_point(aes(y=res$act_r,fill="act-r"),color='red',fill="act-r") +
##     ylab("execution time") + xlab("number of item")+ labs(fill="who")+
##     scale_fill_discrete(name = "Dose", labels = c("A", "B"))+
##     theme(legend.position = "bottom")+
##     guides(fill = guide_legend()) + theme_classic() + theme(legend.title = element_text("who"))

## ggsave("result-experiment.png")
