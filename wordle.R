library(httr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(stringr)
source("~/Desktop/wordle_func.R")

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
green.global <- setNames(data.frame(matrix(nrow = 1, ncol = 5)), c("Pos_1", "Pos_2", "Pos_3", "Pos_4", "Pos_5"))

word <- c("S", "O", "R", "E", "S")
color_codes <- c("B", "G", "B", "B", "B")

green.res <- green_letters(word, color_codes, knuth_letters, green.global)
yellow.res <- yellow_letters(word, color_codes, green.res[[1]], green.res[[2]])
black.res <- black_letters(word, color_codes, yellow.res, green.res[[2]])
head(black.res)

next.res <- letters_to_factors(black.res)
next.prob <- plot_hists(next.res, "NEXT")
black.res <- entropy_word(black.res, next.prob)
black.res <- black.res[order(black.res$Entropy, decreasing = TRUE), ]

head(black.res)