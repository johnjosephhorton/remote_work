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
colnames(df.raw) <- c("id", "time", "status", "pub_cat", "gender", "age", "geo", "weight", "q", "response_time")
df.raw$region <- with(df.raw, as.vector(strsplit(geo, "-"))[[2]])
r <- sapply(places, function(x) as.vector(strsplit(x, split = "-")))
df.raw$region <- unlist(lapply(r, function(x) x[2]))
df.raw$state <- unlist(lapply(r, function(x) x[3]))

df.working <- df.raw %>% filter(q != "None of the above / Not working for pay")

addParam("\\numObsWorking", nrow(df.working) %>% formatC(big.mark = ","))

df.raw$time %>% max

addParam <- genParamAdder("../writeup/params.tex")
addParam("\\numObs", nrow(df.raw) %>% formatC(big.mark = ","))


addParam("\\SurveyStart", df.raw %$% time %>% min)
addParam("\\SurveyEnd", df.raw %$% time %>% max)

            
df.working.summary <- df.working %>%
    group_by(q) %>%
    summarise(num.obs = n()) %>%
    ungroup %>%
        mutate(frac = num.obs / sum(num.obs)) %>% 
        mutate(se = sqrt(frac * (1-frac))/sqrt(num.obs))


         

df.working.summary$q <- with(df.working.summary, reorder(q, frac, mean))


g <- ggplot(data = df.working.summary, aes(y = q, x = frac)) +
    geom_segment(aes(x = 0, y = q, yend = q, xend = frac), colour = "grey") +
    geom_point()  +
    geom_errorbarh(aes(xmin = frac - 2*se, xmax = frac + 2*se), height = 0.1) + 
    theme_bw() +
    xlab("% of working respondents") +
    scale_x_continuous(label = scales::percent_format(accuracy = 1)) +
    ylab("")

JJHmisc::writeImage(g, "working_summary", width = 5, height = 2, path = "../writeup/plots/")

print(g)


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

print(g)



## Grab US state
df.working.region <- df.working %>%
    filter(region != "NA") %>% 
    group_by(q, region) %>%
    summarise(num.obs = n()) %>%
    ungroup %>%
    group_by(region) %>% 
        mutate(frac = num.obs / sum(num.obs)) %>% 
    mutate(se = sqrt(frac * (1-frac))/sqrt(num.obs)) 

df.working.region$short.q <- with(df.working.region, gsub('(.{1,17})(\\s|$)', '\\1\n', q))

df.working.region$region <- with(df.working.region, factor(region, levels = c("NORTHEAST", "MIDWEST", "WEST", "SOUTH")))

df.working.region$q <- with(df.working.region, reorder(q, frac, mean))

g <- ggplot(data = df.working.region %>% filter(q != "Used to work from home, but now I commute"), aes(x = region, y = frac, group = q)) +
    geom_line(position =  position_dodge(0.1), alpha = 0.2) +
    geom_point(position = position_dodge(0.1)) + 
    theme_bw() +
    geom_errorbar(aes(ymin = frac - 2*se, ymax = frac + 2*se), width = 0, position = position_dodge(0.1)) + 
#    geom_text_repel(data = df.working.region %>% filter(region == "WEST"), aes(label = short.q), xlim = c(2, NA), segment.colour = NA) +
    expand_limits(x = 3) + 
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    facet_wrap(~short.q, ncol = 4, scale = "free_y") + 
    xlab("") +
    ylab("% of respondents") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


JJHmisc::writeImage(g, "region", width = 6.5, height = 2.5, path = "../writeup/plots/")

print(g)
