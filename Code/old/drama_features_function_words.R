library(stylo)
library(readr)

# source for function words: https://www.cambridge.org/core/books/abs/shakespeare-computers-and-the-mystery-of-authorship/list-of-200-function-words/3E628C06BFD3A548FF836902580B2633


#testing function word frequency

# path to drama file and for output file
path_to_drama <- "Code/small-sample.txt"
path_to_feature <- "Code/function-words.txt"
#output_path <- "Code/function-word-frequency.csv"


#### reading the txts ####

# assigns the file contents to a string, uses readr package
sample_txt <- read_file(path_to_drama)
vector_of_words <- txt.to.words(sample_txt)

feature_txt <- read_file(path_to_feature)
vector_of_features <- txt.to.words(feature_txt)

# makes the table of frequencies (uses one sample text and funtion word text)
feature_frequency_sample <- make.table.of.frequencies(vector_of_words, vector_of_features)


#example for multiple text comparison
#txt.1 = "txt1"
#txt.2 = "txt2"
#txt.3 = "txt3"
#my.corpus.raw = list(txt.1, txt.2, txt.3)
#my.corpus.clean = lapply(my.corpus.raw, txt.to.words)
#function_words = vector_of_features
#make.table.of.frequencies(my.corpus.clean, function_words)
