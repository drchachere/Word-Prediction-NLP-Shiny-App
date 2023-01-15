library(quanteda)
library(dplyr)
library(tm)
library(data.table)

blog_data <- readLines("./final/en_US/en_US.blogs.txt")
blog_data_sample <- sample(blog_data, 30000, replace=FALSE)
blog_data_sample <- tolower(blog_data_sample)
news_data <- readLines("./final/en_US/en_US.news.txt")
news_data_sample <- sample(news_data, 30000, replace=FALSE)
news_data_sample <- tolower(news_data_sample)
twit_data <- readLines("./final/en_US/en_US.twitter.txt")
twit_data_sample <- sample(twit_data, 30000, replace=FALSE)
twit_data_sample <- tolower(twit_data_sample)
lang_data_sample <- append(blog_data_sample, news_data_sample)
lang_data_sample <- append(lang_data_sample, twit_data_sample)
lang_data_sample <- corpus(lang_data_sample)

single_tokens <- tokens(lang_data_sample, remove_punct=TRUE, remove_numbers=TRUE)
n_tokens_4 <- tokens_ngrams(single_tokens, n=4)
n_tokens_3 <- tokens_ngrams(single_tokens, n=3)
n_tokens_2 <- tokens_ngrams(single_tokens, n=2)

n4_tokens_fm <- dfm(n_tokens_4)
n4_tokens_fm <- dfm_trim(n4_tokens_fm, min_termfreq=1)
n4_col_sums <- colSums(n4_tokens_fm)
output4 <- data.table(data.frame(ngram=names(n4_col_sums), freq=n4_col_sums))

n3_tokens_fm <- dfm(n_tokens_3)
n3_tokens_fm <- dfm_trim(n3_tokens_fm, min_termfreq=1)
n3_col_sums <- colSums(n3_tokens_fm)
output3 <- data.table(data.frame(ngram=names(n3_col_sums), freq=n3_col_sums))

n2_tokens_fm <- dfm(n_tokens_2)
n2_tokens_fm <- dfm_trim(n2_tokens_fm, min_termfreq=1)
n2_col_sums <- colSums(n2_tokens_fm)
output2 <- data.table(data.frame(ngram=names(n2_col_sums), freq=n2_col_sums))

n1_tokens_fm <- dfm(single_tokens)
n1_col_sums <- colSums(n1_tokens_fm)

col_sums_list <- list()

n1_col_sums <- as.data.frame(n1_col_sums)
names(n1_col_sums) <- "freq"
n2_col_sums <- as.data.frame(n2_col_sums)
names(n2_col_sums) <- "freq"
n3_col_sums <- as.data.frame(n3_col_sums)
names(n3_col_sums) <- "freq"
n4_col_sums <- as.data.frame(n4_col_sums)
names(n4_col_sums) <- "freq"

saveRDS(n1_col_sums, file="n1_col_sums.RData")
saveRDS(n2_col_sums, file="n2_col_sums.RData")
saveRDS(n3_col_sums, file="n3_col_sums.RData")
saveRDS(n4_col_sums, file="n4_col_sums.RData")