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
            corpus <- Corpus(VectorSource(corpus), 
                             readerControl = list(readPlain, language="en_US", load=TRUE))
            corpus <- tm_map(corpus, content_transformer(tolower))
            corpus <- tm_map(corpus, content_transformer(substituions))
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            profanity_list <- unique(lexicon::profanity_banned)
            corpus <- tm_map(corpus, removeWords, profanity_list)
            my_stop_words <- c("t", "s")
            corpus <- tm_map(corpus, removeWords, my_stop_words)
            corpus <- tm_map(corpus, stripWhitespace)
            corpus <- sapply(corpus, as.character)
            return(corpus)
}

dict <- function(corpus, min_freq=3){
            # Creates a dictionary and keeps the percentage o covering parameter
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

predict_two_gram <- function(s, dict, two_gram_probs){
            s <- suppressWarnings(my_preprocess(s))
            w_n <- word(s, -1)
            if(w_n%in% probs$first_word){
                        w_n1 <- probs %>% filter(first_word==w_n) %>%
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




# Predict
## Decompose the sentence into single words and construct the (n-1)-gram model
## Search through out the dictionary the n-grams that start with the (n-1)-gram from previous step
## Return top m n-grams with the highest probabilities
## Return a m-length vector with the last word of each of the m-grams of the previous step

## Smoothing

# Evaluation 
## Use prediction function from previous sections to test the model