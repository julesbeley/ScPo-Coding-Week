# This is a test
#test 2
library(tidyverse)
library(rio)
library(dplyr)
library(readr)
suppressPackageStartupMessages(library(tidyverse))
rne <- read_csv("Repertoire-national-des-elus.csv")

#dplyr & tidyr mainly
#the pipe: %>% (ctrl/cmd + shift + M)
#the verbs: filter (and slice), arrange, select (and rename), distinct, mutate, group_by, summarise, sample_n (number) / sample_frac (fraction)
#stringr might be a useful companion if you have to deal with characters/strings

# == to say equal or %in% because it allows us to have several values

rne %>%
  filter(`Code sexe` == "F") %>%
  mutate(`Code sexe` = recode (`Code sexe`, "F" = "Female", "M" = "Male")) %>%
  arrange(`Date de naissance`) %>% #arrange ((desc())
  # select(-`Code profession`) %>%
  group_by(`Libellé de la profession`) %>%
  summarise(n = n(), age = mean(Age)) %>%
  arrange(`Nom de l'élu`)%>%
  View()

#long and large formats

rne %>%
  gather("Office", "Value", `Conseiller Municipal`:Maire) %>%
  filter(Value  %in% "true")  %>% #remove all values which are not true
  filter(! (`Date de naissance` %in% lubridate::ymd("1900-01-01")))  %>%
  select(-Value)  %>% #Delete column
  group_by(Office) %>% 
  summarise(age = mean(Age, na.rm = TRUE)) %>%
  View()
