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
  arrange(`Nom de l'élu`) %>%
  View()

#long and large formats

rne %>%
  gather("Office", "Value", `Conseiller Municipal`:Maire) %>%
  filter(Value  %in% "true")  %>% #remove all values which are not true
  filter(!(`Date de naissance` %in% lubridate::ymd("1900-01-01")))  %>%
  select(-Value)  %>% #Delete column
  group_by(Office) %>%
  summarise(age = mean(Age, na.rm = TRUE)) %>%
  View()

rne %>%
  gather("Office", "Value", `Conseiller Municipal`:Maire) %>%
  filter(Value  %in% "true")  %>% #remove all values which are not true
  select(-Value)  %>% #Delete column
  group_by(`Identifiant`) %>%
  summarise(
    offices = n(),
    occupation = unique(`Libellé de la profession`),
    gender = unique(`Code sexe`)
  ) %>%
  ungroup()  %>%
  group_by(occupation, gender) %>%
  summarise(offices = mean(offices)) %>%
  View()

rne %>%
  filter(`Code sexe` %in% "F") %>% #keep only women
  group_by(`Libellé de la profession`) %>% #group by professions
  arrange(Age) %>% #order by age
  slice(2) %>% #keep only the second for each
  View ()
#2 eme plus jeune pour chaque profession
#  mutate() create a variable for the rank

rne %>%
  mutate(
    number = case_when(
      `Nombre de mandats` %in% 1 ~ "one",
      `Nombre de mandats` %in% 2 ~ "two",
      `Nombre de mandats` %in% 3 ~ "three" ,
      `Nombre de mandats` %in% 4 ~ "four",
      TRUE  ~ "NA_Character_"
    )
  )  %>%
  View()
#Primary keys = unique identifiers for each observation
#Forein keys = allows for mergeing datasets - relates to another dataset

? bind_rows

#avoid binding columns, bettet to merge (order can be different from one variable to another)
#fuzzy join in case almost identical key but not quite


library(dplyr)
library(ggplot2)
library(scales)
rne %>%
  count(`Libellé de la profession`, sort = TRUE) %>%
  filter(!is.na(`Libellé de la profession`))  %>% #removes N.A at the top
  arrange(n) %>%
  filter (n > 1000) %>% #removes very little observervations
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>% # factors in order
  mutate(
    coord = if_else(n > 40000, n - 2000, n + 2000),
    colour = if_else(n > 40000, "white", "black")
  ) %>%
  ggplot(aes(x = occupation, y = n)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + # flip x and y axis
  geom_text(
    aes(label = occupation, y = coord, colour = colour),
    hjust = "inward",
    vjust = "center",
    size = 2
  ) +
  scale_color_manual(values = c("black", "white"), guide = FALSE) +
  xlab("") +
  ylab("") +
  ylim (0, NA) +
  scale_x_discrete(labels = NULL) +
  theme(axis.ticks.y = element_blank())

#define the x axis label, but removes it.