# used libraries
library(stylo)
library(stringr)

# path to drama file and for output file
path_to_drama <- "Code/small-sample.txt"

# making the sample text into vector
sample_txt <- read_file(path_to_drama)
vector_of_words <- txt.to.words(sample_txt)

#returns the char count per word
char_count <- nchar(vector_of_words)
#returns the average char count (total amount of chars / number of words)
avg_chars <- sum(char_count) / length(char_count)