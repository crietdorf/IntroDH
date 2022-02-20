# used libraries
library(stylo)
library(readr)


#testing word grams

# path to drama file and for output file
path_to_drama <- "Code/small-sample.txt"
output_path <- "Code/word-bi-grams.csv"
output_path2 <- "Code/frequency-word-bi-grams.csv"


#### making the ngram ####

# assigns the file contents to a string, uses readr package
sample_word_bi_gram <- read_file(path_to_drama)
# string gets split into a vector of consecutive words
vector_of_words <- txt.to.words(sample_word_bi_gram)
# makes the ngrams and stores them in object
word_bi_grams <- make.ngrams(my.vector.of.words, ngram.size = 2)
# writes ngrams in a csv
write.csv(word_bi_grams, file = output_path)

#### checking frequency ####

list_of_frequency <- make.frequency.list(word_bi_grams, value = TRUE, head = NULL, relative = TRUE) # nolint
write.csv(list_of_frequency, file = output_path2)