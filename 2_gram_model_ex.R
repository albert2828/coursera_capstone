library(dplyr)
library(ggplot2)
library(tm)
library(tokenizers)
library(stringr)

source("algorithm.R")

file.txt <- "./final/en_Us/en_US.twitter.txt"
con <- file(file.txt, "r")
twitter_corpus <- readLines(con)
close(con)
length(twitter_corpus)
set.seed(221)
selection <- sample(1:length(twitter_corpus), 500000)
twitter_corpus <- twitter_corpus[selection]

twitter_dict <- dict(my_preprocess(twitter_corpus), 3)
twitter_dict <- twitter_dict %>% filter(Freq>=3) %>%
            arrange(desc(Freq))
twitter_dict$dictionnary <- as.character(twitter_dict$dictionnary)

set.seed(11)
inTrain <- sample(1:500000, 450000)
training <- twitter_corpus[inTrain]
testing <- twitter_corpus[-inTrain]

training <- my_preprocess(training)

twitter_2_grams <- unlist(tokenize_ngrams(training, n=2))
twitter_2_grams <- data.frame(table(twitter_2_grams))
colnames(twitter_2_grams) <- c("two_gram", "freq")
twitter_2_grams <- twitter_2_grams %>% filter(freq>=3) %>% 
            arrange(desc(freq))
twitter_2_grams[,1] <- as.character(twitter_2_grams[,1])

sep_word <- function(w){
            s <- strsplit(w, " ")
            s <- unlist(s)
            return(s)
}

sep_2_grams <- sapply(twitter_2_grams[,1], sep_word)
sep_2_grams <- data.frame(t(sep_2_grams))
row.names(sep_2_grams) <- 1:dim(sep_2_grams)[1]
colnames(sep_2_grams) <- c("first_word", "second_word")
twitter_data <- cbind(twitter_2_grams, sep_2_grams)

twitter_data$first_word_freq <- rep(0, dim(twitter_data)[1])
for(i in 1:dim(twitter_dict)[1]){
            s <- twitter_data$first_word==twitter_dict[i,1]
            twitter_data$first_word_freq[s] = twitter_dict[i,2]
}
#V = sum(twitter_dict$Freq)
prob <- twitter_data$freq/twitter_data$first_word_freq
twitter_data <- cbind(twitter_data, prob)
rownames(twitter_data) <- 1:length(prob)
twitter_data <- twitter_data %>% arrange(desc(prob))

predict_txt <- function(s){
            s <- suppressWarnings(my_preprocess(s))
            w <- word(s, 1,-2)
            w_n <- word(s, -1)
            if(w_n%in%twitter_data$first_word){
                        w_n1 <- twitter_data %>% filter(first_word==w_n) %>%
                                    arrange(desc(prob)) %>%
                                    select(second_word)
                        w_n1 <- w_n1$second_word[1:3]
            }
            else {
                        w_n1 <- twitter_dict %>% arrange(desc(Freq))
                        w_n1 <- twitter_dict$dictionnary[1:3]
            }
            return(w_n1)
}
t0 <- Sys.time()
predict_txt("i love")
t1 <- Sys.time()
(prediction_time <- difftime(t1,t0))

## Evaluation
# Apply pre-processing to training data //
# Separate into 3-grams and store the 2-gram of the beginning of the sentence and the last word into
# different columns
# Apply the predict_text_function and store the result into a new data_frame
# Evaluate if the last word of each 3-gram is in the three predicted words and annotate the position of the
# correct result if the case
testing <- testing[1:10000]
testing_sentences <- unlist(tokenize_sentences(testing, 
                                                lowercase = TRUE, 
                                                strip_punct = TRUE))
testing_sentences <- my_preprocess(testing_sentences)
testing_sentences <- unique(testing_sentences)
evaluation <- data.frame(testing_sentences, 
                         first_sentence = rep("", length(testing_sentences)),
                         result = rep("", length(testing_sentences)))
evaluation$first_sentence <- word(testing_sentences, 1, -2)
evaluation$result <- word(testing_sentences, -1)

predictions <- data.frame(pred1 = predict_txt(evaluation$first_sentence[1])[1],
                          pred2 = predict_txt(evaluation$first_sentence[1])[2],
                          pred3 = predict_txt(evaluation$first_sentence[1])[3])
for(i in 2:length(testing_sentences)){
            predictions <- rbind(predictions, 
                                 predict_txt(evaluation$first_sentence[i]))
}
evaluation <- cbind(evaluation, predictions)
evaluation$pred2[is.na(evaluation$pred2)] <- "the"
evaluation$pred3[is.na(evaluation$pred3)] <- "in"

evaluation$correct <- rep(FALSE, dim(evaluation)[1])
for(i in 1:dim(evaluation)[1]){
            evaluation$correct[i] <- evaluation$result[i] %in% evaluation[i,4:6]
            
}
(accuracy <- sum(evaluation$correct)/dim(evaluation)[1])

