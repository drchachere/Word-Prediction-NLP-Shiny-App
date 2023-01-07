library(shiny)
library(quanteda)
library(dplyr)
library(tm)
library(data.table)
library(ggplot2)

prepStrForGrep <- function(x){
    final_str <- gsub(" ", "_", x)
    final_str <- paste("^", final_str, sep="")
    final_str <- paste(final_str, "_.*", sep="")
    final_str
}

extractLastWord <- function(x){
    words_vec <- unlist(strsplit(x, "_"))
    final_word <- words_vec[length(words_vec)]
    final_word
}

removeFirstWord <- function(x){
    input_vec <- unlist(strsplit(x, " "))
    output <- input_vec[2:length(input_vec)]
    paste(output, collapse =" ")
}

n4_col_sums <- readRDS("n4_col_sums.RData")
n3_col_sums <- readRDS("n3_col_sums.RData")
n2_col_sums <- readRDS("n2_col_sums.RData")
n1_col_sums <- readRDS("n1_col_sums.RData")

createBars <- function(output_df) {
    g <- ggplot(output_df, aes(prediction, probability)) + 
        geom_col() + 
        xlab("Predictions") +
        ylab("Probability") +
        coord_flip()
    g
}

createText <- function(output_df){
    beg <- "Matching as many last words possible, the top predictions are...\n\n"
    end <- paste(output_df$prediction, collapse = ", ")
    message <- paste(beg, end, sep="")
    message
}

predictNextWord <- function(phrase){
    len <- length(unlist(strsplit(phrase, " ")))
    if (len > 3) {
        predictNextWord(removeFirstWord(phrase))
    } else if (len==0) {
        ngram_of_interest <- n1_col_sums
        ngram_of_interest$prediction.prob <- round(ngram_of_interest$freq/sum(ngram_of_interest$freq),
                                                   3)
        ngram_of_interest <- arrange(ngram_of_interest, desc(freq))
        poss_outcomes_list <- row.names(ngram_of_interest)
        results_list <- poss_outcomes_list[1:10]
        prob_list <- unlist(ngram_of_interest$prediction.prob)
        prob_list <- prob_list[1:10]
        output_df <- data.frame(prediction = results_list[!is.na(results_list)], 
                             probability = prob_list[!is.na(prob_list)])
        output_df$prediction <- factor(output_df$prediction, levels = output_df$prediction[order(output_df$probability)])
        output_df
        
    } else if (len==3) {
        phrase <- tolower(removePunctuation(phrase))
        
        ngram_of_interest <- data.frame(freq = n4_col_sums[grep(prepStrForGrep(phrase),
                                                                row.names(n4_col_sums),
                                                                perl=TRUE),])
        row.names(ngram_of_interest) <- row.names(n4_col_sums)[grep(prepStrForGrep(phrase), 
                                                                    row.names(n4_col_sums), 
                                                                    perl=TRUE)]
        if(nrow(ngram_of_interest) >= 3) {
            ngram_of_interest$prediction.prob <- round(ngram_of_interest$freq/sum(ngram_of_interest$freq),
                                                       3)
            ngram_of_interest <- arrange(ngram_of_interest, desc(freq))
            poss_outcomes_list <- row.names(ngram_of_interest)
            results_list <- unlist(lapply(poss_outcomes_list, extractLastWord))
            results_list <- results_list[1:10]
            prob_list <- unlist(ngram_of_interest$prediction.prob)
            prob_list <- prob_list[1:10]
            output_df <- data.frame(prediction = results_list[!is.na(results_list)], 
                                 probability = prob_list[!is.na(prob_list)])
            output_df$prediction <- factor(output_df$prediction, 
                                        levels = output_df$prediction[order(output_df$probability)])
            output_df
        } else {
            predictNextWord(removeFirstWord(phrase))
        }
    } else if (len==2) {
        phrase <- tolower(removePunctuation(phrase))
        
        ngram_of_interest <- data.frame(freq = n3_col_sums[grep(prepStrForGrep(phrase),
                                                                row.names(n3_col_sums),
                                                                perl=TRUE),])
        row.names(ngram_of_interest) <- row.names(n3_col_sums)[grep(prepStrForGrep(phrase), 
                                                                    row.names(n3_col_sums), 
                                                                    perl=TRUE)]
        if(length(ngram_of_interest$freq) >= 3) {
            ngram_of_interest$prediction.prob <- round(ngram_of_interest$freq/sum(ngram_of_interest$freq),
                                                       3)
            ngram_of_interest <- arrange(ngram_of_interest, desc(freq))
            poss_outcomes_list <- row.names(ngram_of_interest)
            results_list <- unlist(lapply(poss_outcomes_list, extractLastWord))
            results_list <- results_list[1:10]
            prob_list <- unlist(ngram_of_interest$prediction.prob)
            prob_list <- prob_list[1:10]
            output_df <- data.frame(prediction = results_list[!is.na(results_list)], 
                                 probability = prob_list[!is.na(prob_list)])
            output_df$prediction <- factor(output_df$prediction, 
                                        levels = output_df$prediction[order(output_df$probability)])
            output_df
        } else {
            predictNextWord(removeFirstWord(phrase))
        }
    } else {
        phrase <- tolower(removePunctuation(phrase))
        ngram_of_interest <- data.frame(freq = n2_col_sums[grep(prepStrForGrep(phrase),
                                                                row.names(n2_col_sums),
                                                                perl=TRUE),])
        row.names(ngram_of_interest) <- row.names(n2_col_sums)[grep(prepStrForGrep(phrase), 
                                                                    row.names(n2_col_sums), 
                                                                    perl=TRUE)]
        if(length(ngram_of_interest$freq) >= 3) {
            ngram_of_interest$prediction.prob <- round(ngram_of_interest$freq/sum(ngram_of_interest$freq),
                                                       3)
            ngram_of_interest <- arrange(ngram_of_interest, desc(freq))
            poss_outcomes_list <- row.names(ngram_of_interest)
            results_list <- unlist(lapply(poss_outcomes_list, extractLastWord))
            results_list <- results_list[1:10]
            prob_list <- unlist(ngram_of_interest$prediction.prob)
            prob_list <- prob_list[1:10]
            output_df <- data.frame(prediction = results_list[!is.na(results_list)], 
                                 probability = prob_list[!is.na(prob_list)])
            output_df$prediction <- factor(output_df$prediction, 
                                        levels = output_df$prediction[order(output_df$probability)])
            output_df
        } else {
            predictNextWord(removeFirstWord(phrase))
        }
    }
}


# Define server logic required to draw a bar chart with top 10 next word predictions
shinyServer(function(input, output) {
    phrase_ <- reactive({as.character(input$phrase_)})
    
    output_df <- reactive({
        predictNextWord(phrase_())
    })
    
    output$message <- renderText({
        createText(output_df())
    })
    
    output$barChart <- renderPlot({
        createBars(output_df())
    })

})
