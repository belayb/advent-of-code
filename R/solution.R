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


# Day 3: rearranging rucksacks
# Each rucksack has two large compartments.
# All items of a given type are meant to go into exactly one of the two compartments.
# The Elf that did the packing made an error 
# The Elves have made a list of all of the items currently in each rucksack
# Every item type is identified by a single lowercase or uppercase letter (that is, a and A refer to different types of items).
# A given rucksack always has the same number of items in each of its two compartments, so the first half of the characters represent items in the first compartment, while the second half of the characters represent items in the second compartment.
# find the item that appers in both compartments 
# priority:
#Lowercase item types a through z have priorities 1 through 26.
#Uppercase item types A through Z have priorities 27 through 52. 
# challange:
#Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?

day3 <- readLines(here("data/day3_puz1.txt"))

find_common<- function(text){
    common <- intersect(strsplit(substring(text, 1,nchar(text)/2), '')[[1]],
               strsplit(substring(text, nchar(text)/2 +1,nchar(text)), '')[[1]])
    return(common)
    }

priorty <- function(common_txt){
              if (common_txt == tolower(common_txt)){
                which(common_txt== letters[1:26])
                 }
            else {
               which(common_txt== LETTERS[1:26])+26
                 }
                }
common_badge <- function(x) Reduce(intersect, lapply(x, function(x) strsplit(x, '')[[1]]))

# Puzzel 1
# apply common_text to each element of input 
common_txt<- unlist(lapply(day3, find_common))
sum(unlist(lapply(common_txt, priorty)))

# puzzel 2

# split input in to chunk of size 3
day3_chunk <- split(day3, ceiling(seq_along(day3)/3))
# apply common badge and priorty function for each chunk
sum(unlist(lapply(lapply(day3_chunk, common_badge), priorty)))


# Day 4
day4 <- readLines(here("data/day4_puz1.txt"))
#puzzle 1
overlap <- function(text){
      text <- strsplit(strsplit(text, ',')[[1]], ' ') # nolint
      sections <- sapply(paste("c(", gsub("\\-", ":", text), ")"), 
                           function(x) eval(parse(text = x)))
      dim_common <-  length(intersect(sections[[1]], sections[[2]]))                               # nolint
(dim_common == length(sections[[1]]) | dim_common == length(sections[[2]]))
}
sum(unlist(lapply(day4, overlap)))


#day5 - puzzle one 

day5 <- read.fwf(here("data/day5_puz1.txt"), widths = rep(4,9), n=8, header = FALSE)
day5 <- lapply(day5, \(x) rev(na.omit(sub(".+([A-Z]).+", "\\1", x)[gsub(" ", "", x) != ""])))

day5_guides <- readLines(here("data/day5_puz1_instractions.txt"))
day5_guides<- as.data.frame(do.call('rbind', lapply(day5_guides, function(x) as.numeric(strsplit(x, " " )[[1]][c(2,4,6)]))))

 for( i in 1: nrow(day5_guides)){
  day5[[day5_guides$V3[i]]] <- append(day5[[day5_guides$V3[i]]], rev(tail(day5[[day5_guides$V2[i]]], day5_guides$V1[i])))
  day5[[day5_guides$V2[i]]] <- head(day5[[day5_guides$V2[i]]], -day5_guides$V1[i])
 }

sapply(day5, function(x) tail(x, 1))
 
# day 5 - puzzle two
 for( i in 1: nrow(day5_guides)){
  day5[[day5_guides$V3[i]]] <- append(day5[[day5_guides$V3[i]]], tail(day5[[day5_guides$V2[i]]], day5_guides$V1[i]))
  day5[[day5_guides$V2[i]]] <- head(day5[[day5_guides$V2[i]]], -day5_guides$V1[i])
 }

sapply(day5, function(x) tail(x, 1))


