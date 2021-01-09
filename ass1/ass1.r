
library(tidyverse)
library(ggplot2)
dat <- read.csv('disruptions-2019-Q4.csv')
dat.tib <- as_tibble(dat)


hours <- dat.tib %>%
    mutate(duration_hours= duration_minutes / 60)


count(hours,cause_group)

known_causes <- filter(hours,cause_group != "" & cause_group != "unknown" ) %>% filter(duration_hours < 20)

ggplot(known_causes,aes(duration_hours))+
    geom_histogram(binwidth = 1) + theme(text = element_text(size = 20))

ggsave("known_causes.jpg")


avg_delay_causes <- group_by(known_causes,cause_group) %>%
    summarize(avg=mean(duration_hours))

ggplot(avg_delay_causes,aes(cause_group,avg)) +
    geom_bar(stat="identity") + theme(axis.text = element_text(size = 8.5))

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
