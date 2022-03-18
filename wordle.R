library(httr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(stringr)
source("wordle_func.R")

url <- "https://www.nytimes.com/games/wordle/main.18637ca1.js"
wordle_content <- fetch_content(url)
wordle_list <- wordle_script_2_list(wordle_content)

wordle_letters <- words_to_letters(wordle_list)

wordle_factors <- letters_to_factors(wordle_letters)

wordle_prob <- plot_hists(wordle_factors, "Wordle Word List")
wordle_letters <- entropy_word(wordle_letters, wordle_prob)
wordle_letters <- wordle_letters[order(wordle_letters$Entropy, decreasing = TRUE), ]

url <- "https://www-cs-faculty.stanford.edu/~knuth/sgb-words.txt"
knuth_content <- fetch_content(url)
knuth_list <- knuth_script_2_list(knuth_content)

knuth_letters <- words_to_letters(knuth_list)
knuth_factors <- letters_to_factors(knuth_letters)

knuth_prob <- plot_hists(knuth_factors, "Knuth Word List")

knuth_letters <- entropy_word(knuth_letters, knuth_prob)

knuth_letters <- knuth_letters[order(knuth_letters$Entropy, decreasing = TRUE), ]
print(knuth_letters[1:10, ])

guess <- c("T", "A", "B", "O", "O")
color_codes <-c("G", "G", "B", "Y", "B")

T_A <- knuth_letters[apply(knuth_letters[,1:5], 1, wordle_find, guess = guess, 
                    color_codes = color_codes),]
