fetch_content <- function(url){
  return(GET(url) %>% content(as = "text", encoding = "UTF-8"))
}

wordle_script_2_list <- function(wordle_script){ 
  wordle <- substr(wordle_script,
  str_locate(wordle_script, "cigar")[,"start"],
  str_locate(wordle_script, "shave")[,"end"]) %>%
    str_remove_all("\"") %>%  
    str_split(",") %>% 
    data.frame() %>% 
    select(word = 1) %>% 
    mutate(word = toupper(word))
  return(as.list(wordle)$word)
}

knuth_script_2_list <- function(knuth_script){
  knuth <- gsub("[\r\n]", ",", knuth_script) %>%
    str_split(",") %>%
    data.frame() %>%
    select(word = 1) %>%
    mutate(word = toupper(word))
    knuth <- as.list(knuth)$word
  return(knuth[knuth != ""])
  
}

words_to_letters <- function(words){
  letter_mat <- matrix(nrow = length(words),
                       ncol = 7)
  for(i in 1:length(words)){
    word <- words[i]
    letter_mat[i,7] <- word
    for(j in 1:5){
      letter_mat[i,j] <- substr(word, j, j)
    }
  }
  letter_mat <- data.frame(letter_mat)
  return(setNames(letter_mat, c("Pos_1", "Pos_2", "Pos_3", "Pos_4", "Pos_5", "Entropy", "Word")))
}

letters_to_factors <- function(letter_mat){
  factor_mat <- matrix(nrow = length(letter_mat[,1]), ncol = 5)
  for(i in 1:5){
    factor_mat[,i] <- as.numeric(factor(letter_mat[,i], levels = LETTERS))
  }
  return(factor_mat)
}

plot_hists <- function(factor_mat, title.text){
  factor_prob <- matrix(nrow = 26, ncol = 6)
  for(i in 1:6){
    if(i < 6){
      hist_letters <- factor_mat[, i]
      ylab_text = paste("P(L|pos=", i, ")",sep = "")
    }
    else{
      ylab_text = "P(L)"
      hist_letters <- c(factor_mat)
    }
    factor_prob[,i] <- hist(hist_letters, probability = T, xlab = "", ylab = "", 
       yaxt = "n", xaxt = "n", main = "", labels = LETTERS, breaks = seq(0,26,by = 1),
       xaxs = "i", yaxs = "i", xlim = c(-1, 28), ylim = c(0,0.375))$density
    title(xlab = "Letter", ylab = ylab_text, main = title.text)
    axis(2, at = seq(0,0.35, by = 0.1), tck = 0.03)
    axis(1, at = c(-1,27), labels = c("", ""), tck = 0)
  }
  row.names(factor_prob) <- LETTERS
  return(factor_prob)
}

entropy <- function(p){
  return(sum(-p*log2(p)))
}

word_2_ent <- function(word, prob_mat){
  prob <- c(prob_mat[word[1], 1], 
            prob_mat[word[2], 2],
            prob_mat[word[3], 3],
            prob_mat[word[4], 4],
            prob_mat[word[5], 5])
  return(entropy(prob))
  }

entropy_word <- function(letter_mat, prob_mat){
  letter_mat$Entropy <- apply(letter_mat[,1:5], 1, word_2_ent, prob_mat = prob_mat)
  return(letter_mat)
}

wordle_find <- function(letter_mat, color_codes, guess){
  grn_idx <- which(color_codes == "G")
  ylw_idx <- which(color_codes == "Y")
  blk_idx <- which(color_codes == "B")
  grn_ltr <- guess[grn_idx]
  ylw_ltr <- guess[ylw_idx]
  blk_ltr <- guess[blk_idx]
  grn_bool <- grn_ltr %in% letter_mat[grn_idx]
  if(any(ylw_ltr %in% blk_ltr)){
    mgk_ltr <- ylw_ltr[ylw_ltr %in% blk_ltr]
    mgk_idx <- which(guess == mgk_ltr & color_codes == "Y")
    # mgk_bool <- mgk_bool
    return(as.logical(prod(c(grn_bool, ylw_bool, mgk_idx))))
  }
  ylw_bool <- ylw_ltr %in% letter_mat[!(c(1:5) %in% grn_idx) & !(c(1:5) %in% ylw_idx)]
  blk_bool <- !(blk_ltr %in% letter_mat[!(1:5) %in% grn_idx])
  
  return(as.logical(prod(c(grn_bool, ylw_bool, blk_bool))))
}

# wordle.game <- function(wordle_word





















# green_letters <- function(word_letters, color_codes, letter_mat, green.global){
#   if(length(green.global) < 1){
#     green.global <- setNames(data.frame(matrix(nrow = 1, ncol = 5)), c("Pos_1", "Pos_2", "Pos_3", "Pos_4", "Pos_5"))
#    
#   }
#   
#   if("G" %in% toupper(color_codes)){
#     green_idx <- which(toupper(color_codes) == "G")
#     green.global[, green_idx] <- word_letters[green_idx]
#     for(i in green_idx){
#       letter_mat <- subset(letter_mat, letter_mat[, i] == word_letters[i])
#     }
#   }
#   return(list(letter_mat, green.global))
# }
# 
# yellow_letters <- function(word_letters, color_codes, letter_mat, green.global){
#   if("Y" %in% toupper(color_codes)){
#     yellow_idx <- c(1:5)
#     yellow_idx <- yellow_idx[is.na(green.global)]
#     yellow_idx <- yellow_idx[yellow_idx != which(color_codes == toupper("Y"))]
#     word_letters <- word_letters[which(color_codes == toupper("Y"))]
#     letter_mat <- letter_mat[apply(letter_mat[, yellow_idx], 1, function(x) word_letters %in% x), ]
#   }
#   return(letter_mat)
# }
# 
# black_letters <- function(word_letters, color_codes, letter_mat, green.global){
#   black_idx <- c(1:5)
#   black_idx <- black_idx[is.na(green.global)]
#   word_letters <- unique(word_letters[which(color_codes == toupper("B"))])
#   print(word_letters)
#   print(black_idx)
#   letter_mat <- letter_mat[apply(letter_mat[, black_idx], 1, function(x) !(word_letters %in% x)), ]
#   letter_mat <- na.omit(letter_mat)
#   return(letter_mat)
# }
# 
