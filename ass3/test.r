require(tidyverse)
require(gtools)

## When you use this data, it is important to realize that this concerns
## train disruptions reported by NS as a (national) disruption report. A
## failure message is not generated for every train that is delayed or
## canceled; The rule of thumb applied by NS is that a message is issued
## when several trains are canceled or delayed. In addition, it is good
## to realize that more failures have also been reported since 2017,
## because NS then introduced a new system that also reported short-term
## failures more quickly. Comparing the number of disruptions from 2017
## with the number of disruptions in previous years is therefore not
## possible (unless you do not count all short-term disruptions).


## The source for fault messages is always NS; At NS, the travel
## information department checks 24 hours a day to see if there are any
## disruptions. The error messages in the open data are the same as the
## messages on the signs at the station and on the NS website.


## In the outage data you will find the following columns:

## rdt_id - This is the ID that Ride the Trains uses for a
## fault. When you open an outage in the outage archive, you will see
## the ID processed in the URL of the outage page. For example, in
## this train disruption between Amsterdam South and Schiphol the ID
## 12345.

## ns_lines - These are the routes that NS links to a fault. In the
## case of the failure in the example, this is Schiphol-Almere
## C./Hilversum/Utrecht C. A problem with these routes (if you want
## to analyze the data) is that they are not standardized. This
## column is therefore less suitable for use for analyzes.

## rdt_lines - These are the routes that Driving the Trains has
## linked to a disruption. This is always based on the list of routes
## of Driving the Trains, and the link is based on the stations where
## a fault is located. In the example, the disruption is between
## Amsterdam South and Schiphol. Driving the Trains then combines the
## Amersfoort-Schiphol, Lelystad-Schiphol and Utrecht-Schiphol
## routes. The trajectories are always linked in alphabetical order,
## separated by a comma.

## rdt_lines_id - This is the ID of the routes that Ride the Trains
## has linked, separated by a comma.

## rdt_station_names - Based on the link with the routes, Rijden de
## Trains also calculates which stations are affected by a train
## disruption. This column lists the station names of the affected
## stations, separated by a comma.

## rdt_station_codes - These are the codes (abbreviations) of the
## affected stations, separated by a comma. The station codes can be
## found in the dataset with stations (see above).

## cause_en - This is the cause of a malfunction, in Dutch. If during
## a fault the cause is adjusted by NS, the last used cause is
## displayed in this column.

## cause_en - The cause of the failure translated into English.

## statistical_cause_en - For statistical purposes, Driving the
## Trains also keeps a 'statistical cause'. When the cause of the
## failure changes, information about the actual cause of a failure
## is sometimes lost. If the fault cause is adjusted to a cause like
## a previous fault, the original cause is shown in this column. For
## example, for this signal failure at Woerden, the last cause is
## 'a previous disturbance', but for statistics we assume the cause
## is major signal and points failure.

## statistical_cause_en - The statistical cause, but in English.

## cause_group - The group in which the failure is classified. These
## are the headings on the failure causes list, such as Accidents or
## Equipment Problems. In the column, the group is stored in English.

## start_time - The time when the failure started.

## end_time - The time when the outage ended.

## duration_minutes - The failure duration in minutes.


pre_proc <- function() {
  read.csv("disruptions-2019-Q4.csv") %>%
    as_tibble() %>%
    mutate_at(vars(name, cause, group), factor) %>%
    mutate_at(vars(sid), as.integer) %>%
    rename(
      id = rdt_id, delay = duration_minutes,
      cause = statistical_cause_en,
      names = rdt_station_names,
      group = cause_group) %>%
    dplyr::select(id, delay, cause, names, group) %>%
    separate(names, as.character(c(1:33)),
      sep = ",") %>%
    gather(
      "sid", "name",
      -delay, -cause,
      -id, -group)
}

test <- pre_proc()


filter_data <- defmacro(df, col, what,
  expr = df [df$col == what, ])

se <- function(col) {
  col %>%
    na.omit() %>%
    (function(x) sd(x) / sqrt(length(x)))
}

sum_data <- defmacro(df, group,
  expr = df %>%
    group_by(group) %>%
    summarize(
      m = mean(delay, na.rm = TRUE),
      stde = se(delay)))
