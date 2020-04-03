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
})
    
df.raw <- read.csv("../etl/gcs.csv",
                   stringsAsFactors = FALSE) 

colnames(df.raw) <- c("id", "time", "status", "pub_cat", "gender", "age", "geo", "weight", "q", "response_time")

df.working <- df.raw %>% filter(q != "None of the above / Not working for pay")

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
    scale_x_continuous(label = scales::percent) +
    ylab("")

JJHmisc::writeImage(g, "working_summary", width = 6, height = 2, path = "../writeup/plots/")

print(g)
