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

# Day 2
# Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
# Strategy guide: 
#    The first column is what your opponent is going to play: 
#     A for Rock, B for Paper, and C for Scissors
#     The second column, you reason, must be what you should play in response:
#     X for Rock, Y for Paper, and Z for Scissors.
# Your total score is the sum of your scores for each round. 
# The score for a single round is the score for the shape you 
# selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus 
# the score for the outcome of the round (0 if you lost, 3 if 
# the round was a draw, and 6 if you won)
day2 <- readLines(here("data/day2_puz1.txt"))

# Puzzle one 
tibble(col1 = substr(day2, start = 1, stop = 1),
       col2 = substring(day2, 2))%>%
       mutate(score = case_when (col1 == "A" & col2 == " X" ~  4,
           col1 == "A" & col2 == " Y" ~  8, 
           col1 == "A" & col2 == " Z" ~  3,
           col1 == "B" & col2 == " X" ~  1,
           col1 == "B" & col2 == " Y" ~  5,
           col1 == "B" & col2 == " Z" ~  9,
           col1 == "C" & col2 == " X" ~  7,
           col1 == "C" & col2 == " Y" ~  2,
           col1 == "C" & col2 == " Z" ~  6))%>%
           summarise(tot_score = sum(score))

# Puzzle two  
# X means you need to lose, Y means you need 
# to end the round in a draw, and Z means you need to win. 

tibble(col1 = substr(day2, start = 1, stop = 1),
       col2 = substring(day2, 2))%>%
       mutate(score = case_when (col1 == "A" & col2 == " X" ~  3,
           col1 == "A" & col2 == " Y" ~  4, 
           col1 == "A" & col2 == " Z" ~  8,
           col1 == "B" & col2 == " X" ~  1,
           col1 == "B" & col2 == " Y" ~  5,
           col1 == "B" & col2 == " Z" ~  9,
           col1 == "C" & col2 == " X" ~  2,
           col1 == "C" & col2 == " Y" ~  6,
           col1 == "C" & col2 == " Z" ~  7))%>%
           summarise(tot_score = sum(score))
