library(dplyr)
library(ggplot2)
library(tm)
library(tokenizers)
library(stringr)

# Train the model
## Preprocess the model *
## Construct the k-grams and k-grams tables, with k=1,...,n *
## Estimate probabilities of n-grams
## Construct a dictionary *

substituions <- function(x){
            x <- gsub("â", " ", x)
            x <- gsub("€", " ", x)
            x <- gsub("™", " ", x)
            x <- gsub('“', ' ' , x, fixed = TRUE)
            x <- gsub('“', ' ' , x, fixed = TRUE)
            x <- gsub("œ", ' ' , x, fixed = TRUE)
            x <- gsub("ðÿ", ' ' , x, fixed = TRUE)
            return(x)
}

my_preprocess <- function(corpus){
            # Receives a character vector and apply a pre-proccess to it and returns 
            # a character vecctor 
            corpus <- gsub("â", " ", corpus)
            corpus <- gsub("€", " ", corpus)
            corpus <- gsub("™", " ", corpus)
            corpus <- gsub('“', ' ' , corpus, fixed = TRUE)
            corpus <- gsub('“', ' ' , corpus, fixed = TRUE)
            corpus <- gsub("œ", ' ' , corpus, fixed = TRUE)
            corpus <- gsub("ðÿ", ' ' , corpus, fixed = TRUE)
            orpus <- gsub("™", ' ' , corpus, fixed = TRUE)
            corpus <- Corpus(VectorSource(corpus), 
                             readerControl = list(readPlain, language="en_US", load=TRUE))
            corpus <- tm_map(corpus, content_transformer(tolower))
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            profanity_list <- unique(lexicon::profanity_banned)
            corpus <- tm_map(corpus, removeWords, profanity_list)
            my_stop_words <- c("t", "s")
            corpus <- tm_map(corpus, removeWords, my_stop_words)
            corpus <- tm_map(corpus, removeWords, stopwords("english"))
            corpus <- tm_map(corpus, stripWhitespace)
            corpus <- sapply(corpus, as.character)
            corpus <- trimws(corpus, which = c("both", "left", "right"), 
                   whitespace = "[ \t\r\n]")
            return(corpus)
}

dict <- function(corpus, min_freq=3){
            # Creates a dictionary and keeps the percentage o covering parameter
            corpus <- suppressWarnings(my_preprocess(corpus))
            dictionnary <- tokenize_ngrams(corpus, n=1)
            dictionnary <- unlist(dictionnary)
            dictionnary <- data.frame(table(dictionnary))
            s <- dictionnary$Freq <= min_freq
            dictionnary <- dictionnary[s,]
            dictionnary <- arrange(dictionnary, desc(Freq))
            dictionnary[,1] <- as.character(dictionnary[,1])
            return(dictionnary)
}

two_gram_probs <- function(corpus, dict){
            two_gram <- unlist(tokenize_ngrams(corpus, n=2))
            two_gram <- data.frame(table(two_gram))
            colnames(two_gram) <- c("two_gram", "freq")
            two_gram$two_gram <- as.character(two_gram$two_gram)
            two_gram <- two_gram %>% arrange(desc(freq))
            two_gram$first_word <- word(two_gram$two_gram, 1,1)
            two_gram$second_word <- word(two_gram$two_gram, -1)
            two_gram$first_word_freq <- rep(0, dim(two_gram)[1])
            for(i in dim(dict)[1]){
                        s <- two_gram$first_word == dict$dictionary[i]
                        two_gram$first_word_freq <- dict$Freq[i]
            }
            two_gram$prob <- two_gram$freq/two_gram$first_word_freq
            two_gram <- two_gram %>% arrange(desc(prob)) 
            return(two_gram)
}

predict_two_gram <- function(s, dict, model){
            s <- suppressWarnings(my_preprocess(s))
            w_n <- word(s, -1)
            if(w_n%in% model$first_word){
                        w_n1 <- model %>% filter(first_word==w_n) %>%
                                    arrange(desc(prob)) %>%
                                    select(second_word)
                        w_n1 <- w_n1$second_word[1:3]
            }
            else {
                        w_n1 <- dict %>% arrange(desc(Freq))
                        w_n1 <- dict$dictionary[1:3]
            }
            return(w_n1)
}


evaluate_two_gram <- function(s, dictionary, model){
            testing_sentences <- unlist(tokenize_sentences(s, 
                                                   lowercase = TRUE, 
                                                   strip_punct = TRUE))
            testing_sentences <- my_preprocess(testing_sentences)
            evaluation <- data.frame(testing_sentences)
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
            return(evaluation)
}