library(here)
library(dplyr)

# Solution to advent of code 2022 https://adventofcode.com/
# Day 1

calories<-as.data.frame(readLines(here("data/day1_puz1.txt")))
names(calories)<-"calories"
calories<- calories %>% mutate(group_ind = ifelse(calories == "", 1, 0),
                    elf = cumsum(group_ind))%>%
                    filter(calories !="")%>%
                    group_by(elf)%>%
                    summarise(tot_cal = sum(as.numeric(calories)))%>%
                    arrange(-tot_cal) %>%
                    head(3)%>% # first row for puzzle one answer
                    ungroup()%>%
                    summarise(elf_tot_3 = sum(tot_cal)) # for puzzle two answer

