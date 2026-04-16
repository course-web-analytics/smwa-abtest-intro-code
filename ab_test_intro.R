#' ab_test_intro.R
#' 
#' Code accompanying Lecture 4: A/B Tests: Introduction
#' of Social Media and Web Analytics
#' at TiSEM in 2026
#' 
#' 

# --- Libraries --- # 
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(infer)
library(vtable)
library(car)
library(tidyr)
library(tibble)
library(rstatix)

# --- Load and Inspect Data --- # 
df <- read_csv("data/test_data.csv")

glimpse(df)

# --- Balance Testing --- # 
df %>%
    select(YOURCODE) %>%
    group_by(YOURCODE) %>%
    summarize(YOURCODE )


df %>%
    select(YOURCODE) %>%
    YOURCODE

# --- Treatment Effects --- # 
# YOUR CODE HERE

# --- Sample Size Planning --- #
# With continuous outcomes
power.t.test(sd = sd(df$purch), # ideally using
                                # pre-experiment data!
             delta = 1, # minimum detectable effect
             sig.level = 0.95, # alpha: industry standard
             power=0.80 # 1 - beta: industry standard
             )

# With proportions
power.prop.test(p1=0.07,
                p2=0.07 + 0.01, # d = 0.01
                sig.level=0.05,
                power=0.80
                )


