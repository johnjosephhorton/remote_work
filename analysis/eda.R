#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(magrittr)
    library(tidyr)
    library(ggrepel)
    library(tidyr)
    library(broom)
    library(lme4)
    library(broom.mixed)
    library(reshape2)
    library(scales)
    library(JJHmisc)
})
    
df.raw <- read.csv("../etl/gcs.csv",
                   stringsAsFactors = FALSE) 

colnames(df.raw) <- c("id", "time", "status", "pub_cat",
                      "gender", "age", "geo", "weight", "q", "response_time")


## Add geograph information 
r <- sapply(df.raw$geo, function(x) as.vector(strsplit(x, split = "-")))
df.raw$region <- unlist(lapply(r, function(x) x[2]))
df.raw$state <- unlist(lapply(r, function(x) x[3]))

## Separate out workers that are out of the labor force 
df.working <- df.raw %>% filter(q != "None of the above / Not working for pay")

## Capture numbers from the analysis for call-out in the writeup 
addParam <- genParamAdder("../writeup/params.tex")
num.obs <- nrow(df.raw)
num.obs.working <- nrow(df.working)
lfpr <- num.obs.working / num.obs

addParam("\\LFPRhat", (lfpr %>% multiply_by(100)) %>% formatC(digits = 0, format = "f"))

addParam("\\numObs", nrow(df.raw) %>% formatC(big.mark = ","))
addParam("\\numObsWorking", nrow(df.working) %>% formatC(big.mark = ","))
addParam("\\SurveyStart", df.raw %$% time %>% min %>% as.Date %>% as.character)
addParam("\\SurveyEnd", df.raw %$% time %>% max %>% as.Date %>% as.character)

####################
## Overall responses 
####################

df.working.summary <- df.working %>%
    group_by(q) %>%
    summarise(num.obs = n()) %>%
    ungroup %>%
        mutate(frac = num.obs / sum(num.obs)) %>% 
    mutate(se = sqrt(frac * (1-frac))/sqrt(num.obs)) %>%
    mutate(lb = frac - 1.96 * se,
           ub = frac + 1.96*se)


df.working.summary$q <- with(df.working.summary, reorder(q, frac, mean))

GetPE <- function(answer){
    mu <- df.working.summary %>% filter(q == answer) %$% frac %>% multiply_by(100) %>% formatC(digits = 1, format = "f")
    lb <- df.working.summary %>% filter(q == answer) %$% lb %>% multiply_by(100) %>% formatC(digits = 1, format = "f")
    ub <- df.working.summary %>% filter(q == answer) %$% ub %>% multiply_by(100) %>% formatC(digits = 1, format = "f")
    c("mu" = mu, "lb" = lb, "ub" = ub)
}


laid.off <- GetPE("I have recently been furloughed or laid-off")

addParam("\\LaidOff", laid.off["mu"])
addParam("\\LaidOffLB", laid.off["lb"])
addParam("\\LaidOffUB", laid.off["ub"])


wfh <- GetPE("Used to commute, now work from home")

addParam("\\WFH", wfh["mu"])
addParam("\\WFHLB", wfh["lb"])
addParam("\\WFHUB", wfh["ub"])

alreadyWFH <- GetPE("Used to work from home and still do")

addParam("\\alreadyWFH", alreadyWFH["mu"])
addParam("\\alreadyWFHLB", alreadyWFH["lb"])
addParam("\\alreadyWFHUB", alreadyWFH["ub"])

stillCommute <- GetPE("I continue to commute to work")

addParam("\\stillCommute", stillCommute["mu"])
addParam("\\stillCommuteLB", stillCommute["lb"])
addParam("\\stillCommuteUB", stillCommute["ub"])


g <- ggplot(data = df.working.summary, aes(y = q, x = frac)) +
    geom_segment(aes(x = 0, y = q, yend = q, xend = frac), colour = "grey") +
    geom_point()  +
    geom_errorbarh(aes(xmin = frac - 2*se, xmax = frac + 2*se), height = 0.1) + 
    theme_bw() +
    xlab("% of working respondents") +
    scale_x_continuous(label = scales::percent_format(accuracy = 1)) +
    ylab("")

JJHmisc::writeImage(g, "working_summary", width = 5, height = 2, path = "../writeup/plots/")


############
## By gender                                         
############

## TODO: call out in text how many are missing.

df.working.gender <- df.working %>%
    filter(gender != "Unknown") %>% 
    group_by(q, gender) %>%
    summarise(num.obs = n()) %>%
    ungroup %>%
    group_by(gender) %>% 
        mutate(frac = num.obs / sum(num.obs)) %>% 
    mutate(se = sqrt(frac * (1-frac))/sqrt(num.obs)) 

df.working.gender$short.q <- with(df.working.gender, gsub('(.{1,15})(\\s|$)', '\\1\n', q))

g <- ggplot(data = df.working.gender, aes(x = gender, y = frac, group = q)) +
    geom_line(position =  position_dodge(0.1), alpha = 0.2) +
    geom_point(position = position_dodge(0.1)) + 
    theme_bw() +
    geom_errorbar(aes(ymin = frac - 2*se, ymax = frac + 2*se), width = 0, position = position_dodge(0.1)) + 
    geom_text_repel(data = df.working.gender %>% filter(gender == "Male"), aes(label = short.q), xlim = c(2, NA), segment.colour = NA) +
    expand_limits(x = 3) + 
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    xlab("") +
    ylab("% of respondents") 

JJHmisc::writeImage(g, "gender", width = 6, height = 5, path = "../writeup/plots/")

######################
## By geography/region
######################

## TODO: Report fraction missing gender

df.working.region <- df.working %>%
    filter(region != "NA") %>% 
    group_by(q, region) %>%
    summarise(num.obs = n()) %>%
    ungroup %>%
    group_by(region) %>% 
        mutate(frac = num.obs / sum(num.obs)) %>% 
    mutate(se = sqrt(frac * (1-frac))/sqrt(num.obs)) 

df.working.region$short.q <- with(df.working.region,
                                  gsub('(.{1,17})(\\s|$)', '\\1\n', q))

## TODO: Some reason for this particular ordering
df.working.region$region <- with(df.working.region,
                                 factor(region, levels = c("NORTHEAST", "MIDWEST", "WEST", "SOUTH")))

df.working.region$q <- with(df.working.region, reorder(q, frac, mean))

g <- ggplot(data = df.working.region %>% filter(q != "Used to work from home, but now I commute"),
            aes(x = region, y = frac, group = q)) +
    geom_line(position =  position_dodge(0.1), alpha = 0.2) +
    geom_point(position = position_dodge(0.1)) + 
    theme_bw() +
    geom_errorbar(aes(ymin = frac - 2*se, ymax = frac + 2*se), width = 0, position = position_dodge(0.1)) + 
    expand_limits(x = 3) + 
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    facet_wrap(~short.q, ncol = 4, scale = "free_y") + 
    xlab("") +
    ylab("% of respondents") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

JJHmisc::writeImage(g, "region", width = 6.5, height = 2.5, path = "../writeup/plots/")

df.working.state <- df.working %>%
    filter(state != "NA") %>% 
    group_by(q, state) %>%
    summarise(num.obs = n()) %>%
    ungroup %>%
    group_by(state) %>% 
        mutate(frac = num.obs / sum(num.obs)) %>% 
    mutate(se = sqrt(frac * (1-frac))/sqrt(num.obs)) 

codes <- list(
"Alabama"="AL",
"Alaska"="AK",
"Arizona"="AZ",
"Arkansas"="AR",
"California"="CA",
"Colorado"="CO",
"Connecticut"="CT",
"Delaware"="DE",
"Florida"="FL",
"Georgia"="GA",
"Hawaii"="HI",
"Idaho"="ID",
"Illinois"="IL",
"Indiana"="IN",
"Iowa"="IA",
"Kansas"="KS",
"Kentucky"="KY",
"Louisiana"="LA",
"Maine"="ME",
"Maryland"="MD",
"Massachusetts"="MA",
"Michigan"="MI",
"Minnesota"="MN",
"Mississippi"="MS",
"Missouri"="MO",
"Montana"="MT",
"Nebraska"="NE",
"Nevada"="NV",
"New Hampshire"="NH",
"New Jersey"="NJ",
"New Mexico"="NM",
"New York"="NY",
"North Carolina"="NC",
"North Dakota"="ND",
"Ohio"="OH",
"Oklahoma"="OK",
"Oregon"="OR",
"Pennsylvania"="PA",
"Rhode Island"="RI",
"South Carolina"="SC",
"South Dakota"="SD",
"Tennessee"="TN",
"Texas"="TX",
"Utah"="UT",
"Vermont"="VT",
"Virginia"="VA",
"Washington"="WA",
"West Virginia"="WV",
"Wisconsin"="WI",
"Wyoming"="WY")

####################
## Make Chloropleths
####################

lc.codes <- codes
names(lc.codes) <- tolower(names(codes))

states_map <- map_data("state")
states_map$state <- with(states_map, as.character(lc.codes[as.character(region)]))

df.combo <- df.working.state %>% left_join(states_map, by = "state")  %>% filter(q != "Used to work from home, but now I commute")

g <- ggplot(df.combo, aes(long, lat, group = group))+
    facet_wrap(~q, ncol = 2, scales = "free") + 
    geom_polygon(aes(fill = frac), color = "white")+
    scale_fill_viridis_c(option = "C") +
    theme_bw()

JJHmisc::writeImage(g, "geo", width = 8, height = 6, path = "../writeup/plots/")
