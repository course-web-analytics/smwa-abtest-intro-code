#' abtest_intro_solutions.R
#'
#' Solutions for Lecture 4: A/B Tests: Introduction
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
# Summary of means by group for baseline variables
df %>%
    select(group, days_since, visits, past_purch,
           chard, sav_blanc, syrah, cab) %>%
    group_by(group) %>%
    summarize(across(everything(), list(mean = mean)))

# Formal balance test using vtable's st()
df %>%
    select(group, days_since, visits, past_purch,
           chard, sav_blanc, syrah, cab) %>%
    st(group = 'group', group.test = TRUE)

# --- Treatment Effects --- #
# Look at means of outcome variables by group
df %>%
    select(group, open, click, purch) %>%
    group_by(group) %>%
    summarize(across(everything(), list(mean = mean)))

# Visualize click rates for email A vs B
df %>%
    filter(group != 'ctrl') %>%
    mutate(click = as.factor(click)) %>%
    ggplot(aes(x = group, fill = click)) +
    geom_bar(position = "fill") +
    theme_bw()

# Does email A have higher open rate than B? (proportions test)
df %>%
    filter(group != "ctrl") %>%
    mutate(open = as.logical(open)) %>%
    infer::prop_test(open ~ group,
              alternative = "greater")

# Does email A have higher open rate than B? (regression)
mod <- lm(open ~ group,
          data = df %>% filter(group != "ctrl")
          )
tidy(mod)

# Does email A have higher click rate than B?
mod <- lm(click ~ group,
          data = df %>% filter(group != "ctrl")
          )
tidy(mod)

# Click rate across all groups (including control)
mod <- lm(click ~ group,
          data = df
          )
tidy(mod) %>%
  mutate(across(c(estimate, statistic, std.error), ~round(., 5)))

# Test: does email A lead to different click thru than B?
linearHypothesis(mod, c("groupemail_A = groupemail_B"))

# Pairwise proportions test for click
props <-
    df %>%
    mutate(click = if_else(click == 1, "true", "false")) %>%
    group_by(click = click, group) %>%
    count() %>%
    tidyr::pivot_wider(names_from = group, values_from = n, values_fill = 0) %>%
    tibble::column_to_rownames("click") %>%
    as.matrix()

props %>%
    pairwise_prop_test()

# Does email A lead to higher average purchases than B?
mod <- lm(purch ~ group,
          data = df %>% filter(group != "ctrl")
          )
tidy(mod)

# Do the emails lead to higher average purchases than control?
mod <- lm(purch ~ group,
          data = df
          )
tidy(mod)

# Test: does email A lead to different purchases than B?
linearHypothesis(mod, c("groupemail_A = groupemail_B"))

# Pairwise t-tests for purchases
df %>%
    pairwise_t_test(purch ~ group)

# --- Sample Size Planning --- #
# With continuous outcomes
power.t.test(sd = sd(df$purch), # ideally using
                                # pre-experiment data!
             delta = 1, # minimum detectable effect
             sig.level = 0.05, # alpha: industry standard
             power=0.80 # 1 - beta: industry standard
             )

# With proportions
power.prop.test(p1=0.07,
                p2=0.07 + 0.01, # d = 0.01
                sig.level=0.05,
                power=0.80
                )
