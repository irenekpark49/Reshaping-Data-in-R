---
title: "Irene Park's BS 803 Homework 3"
author: "Irene Park"
date: ''
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(haven)
library(naniar)
```


# National Alzheimer Coordination Center (NACC) Dataset
```{r}
normal <- read_sas("C:/Irene Park's Documents/Academics/MS Applied Biostatistics/BS 803 - Statistical Programming for Biostatisticians/Class 1 - Reshaping Data/Homework 1/exercise1.sas7bdat") %>% 
#Select Variables for Analysis
  dplyr::select(id = NACCID, 
                visit = vnumber, 
                sex = SEX, 
                age = NACCAGEB,
                years_passed = naccyrs,
                cognitive_status = NACCUDSD, 
                mmse_score = NACCZMMS, 
                immediate_score = NACCZLMI, 
                delayed_score = NACCZLMD, 
                digit_forward = NACCZDFT) %>%
#Reorder Factor Levels
  mutate(sex = recode_factor(sex, 
                             "1"="Male",
                             "2"="Female"), 
         cognitive_status = recode_factor(cognitive_status, 
                                          "1"="Normal Cognition",
                                          "2"="Impaired not MCI",
                                          "3"="MCI",
                                          "4"="Dementia")) %>%
#Select Participants with Normal Cognitive Status   
  filter(cognitive_status=="Normal Cognition") %>%
#Drop Cognitive status and Years Since Baseline
  select(-cognitive_status, -years_passed) %>% 
#Convert Missing Data to NA
  naniar::replace_with_na_all(condition = ~.x %in% c(99, -99)) %>%
#Calculate Average Neuropsychological Scores
  rowwise() %>%
  mutate(cognition = mean(c(mmse_score, immediate_score, delayed_score, digit_forward), na.rm=TRUE))



#Reshape Dataset from Long to Wide
normal_wide <- normal %>%
  pivot_wider(names_from = visit, 
              values_from = c(mmse_score, immediate_score, delayed_score, digit_forward, cognition), 
              names_sort = TRUE) 
```








