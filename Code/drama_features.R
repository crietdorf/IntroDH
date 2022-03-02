# used libraries
library(stylo)

line_length = function(drama){
  # create counter for number of words and lines
  line_counter = 0
  word_counter = 0
  # for each (not empty) text line in drama count number of words and increase line counter by one
  for (line in drama){
    if (line != ""){
      line_counter = line_counter + 1
      word_counter = word_counter + length(txt.to.words(line))
    } 
  }
  # divide number of words by number of lines and save result in dataframe
  line_length = as.data.frame(word_counter / line_counter)
  # rename column
  colnames(line_length) = c("average line length")
  # save dataframe to csv file
  #write.csv(line_length, paste("feature-results/",drama_name,"-line-length.csv"), row.names = FALSE)
  write.table( line_length, sep=",",  gsub(" ", "", paste("feature-results/",drama_name,"-line-length.csv")), row.names = FALSE, col.names=FALSE)
}

word_length = function(drama){
  # words in drama
  words = txt.to.words(drama)
  # character count per word
  char_count = nchar(words)
  # calculate average char count and save result in dataframe
  word_length = as.data.frame(sum(char_count) / length(char_count))
  # rename column
  colnames(word_length) = c("average word length")
  # save dataframe to csv file
  #write.csv(word_length, paste("feature-results/",drama_name,"-word-length.csv"), row.names = FALSE)
  write.table( word_length, sep=",",  gsub(" ", "", paste("feature-results/",drama_name,"-word-length.csv")), row.names = FALSE, col.names=FALSE)
}

punctuation = function(drama){
  # create 'empty' list (list with one entry)
  punctuation = ("")
  # for each (not empty) text line in drama extract all punctuation marks 
  for (line in drama){
    if (line != ""){
      punctuation = append(punctuation, str_extract_all(line, "[:punct:]")[[1]])
    } 
  }
  # convert list into dataframe and count frequencies (remove first entry)
  punctuation = as.data.frame((table(punctuation[-1])))
  # order dataframe by frequency (decreasing)
  punctuation = punctuation[order(punctuation[,2], decreasing = TRUE), ]
  # rename columns
  colnames(punctuation) = c("punctuation mark", "frequency")
  # make frequency relative
  punctuation$frequency = as.numeric(as.character(punctuation$frequency)) / sum(punctuation$frequency)
  # save dataframe to csv file
  #write.csv(punctuation, paste("feature-results/",drama_name,"-punctuation.csv"), row.names = FALSE)
  write.table( punctuation, sep=",",  gsub(" ", "", paste("feature-results/",drama_name,"-punctuation.csv")), row.names = FALSE, col.names=FALSE)
  
}

char_n_grams = function(drama, n){
  # divide drama text into words
  drama_words = txt.to.words(drama)
  # create 'empty' list (list with one entry)
  all_n_grams = ("")
  # split each word into its characters
  # and create tri-grams if word length is greater than 2
  for (word in drama_words){
    drama_chars = txt.to.features(word, features = "c")
    if (nchar(word)>(n-1)) {
      char_n_grams = make.ngrams(drama_chars,n)
      all_n_grams = append(all_n_grams, char_n_grams)
    }
  }
  # convert list into dataframe and count frequencies (remove first entry)
  all_n_grams = as.data.frame((table(all_n_grams[-1])))
  # order dataframe by frequency (decreasing)
  all_n_grams = all_n_grams[order(all_n_grams[,2], decreasing = TRUE), ]
  # rename columns
  colnames(all_n_grams) = c(paste(n,"-gram"), "frequency")
  # make frequency relative
  all_n_grams$frequency = as.numeric(as.character(all_n_grams$frequency)) / sum(all_n_grams$frequency)
  # save dataframe to csv file
  #write.csv(all_n_grams, paste("feature-results/",drama_name,"-character-",n,"-grams.csv"), row.names = FALSE)
  write.table( all_n_grams, sep=",",  gsub(" ", "", paste("feature-results/",drama_name,"-character-",n,"-grams.csv")), row.names = FALSE, col.names=FALSE)
}

word_n_grams = function(drama, n){
  # create 'empty' list (list with one entry)
  all_n_grams = ("")
  # for each text line in drama extract n grams
  for (line in drama){
    words = txt.to.words(line)
    # extract n grams if line length is greater than n-1
    if (length(words) > (n-1)){
      all_n_grams = append(all_n_grams, make.ngrams(words, n))
    } 
  }
  # convert list into dataframe and count frequencies (remove first entry)
  all_n_grams = as.data.frame((table(all_n_grams[-1])))
  # order dataframe by frequency (decreasing)
  all_n_grams = all_n_grams[order(all_n_grams[,2], decreasing = TRUE), ]
  # rename columns
  colnames(all_n_grams) = c(paste(n,"-gram"), "frequency")
  # make frequency relative
  all_n_grams$frequency = as.numeric(as.character(all_n_grams$frequency)) / sum(all_n_grams$frequency)
  # save dataframe to csv file
  #write.csv(all_n_grams, paste("feature-results/",drama_name,"-word-",n,"-grams.csv"), row.names = FALSE)
  write.table( all_n_grams, sep=",",  gsub(" ", "", paste("feature-results/",drama_name,"-word-",n,"-grams.csv")), row.names = FALSE, col.names=FALSE)
}

function_word_frequency = function(drama, function_words){
  # drama and function words
  words = txt.to.words(drama)
  features = txt.to.words(function_words)
  # make list of frequencies (uses drama text and funtion words)
  frequencies = make.table.of.frequencies(words, features, absent.sensitive = FALSE,  relative = FALSE)
  # convert list into data frame
  class(frequencies)="vector"
  feature_frequency = data.frame(features, frequencies)
  # rename columns
  colnames(feature_frequency) = c("function word", "frequency")
  # make frequency relative
  feature_frequency$frequency = as.numeric(as.character(feature_frequency$frequency)) / sum(feature_frequency$frequency)
  # save dataframe to csv file
  #write.csv(feature_frequency, paste("feature-results/",drama_name,"-function-words.csv"), row.names = FALSE)
  write.table( feature_frequency, sep=",",  gsub(" ", "", paste("feature-results/",drama_name,"-function-words.csv")), row.names = FALSE, col.names=FALSE)
}

# path to function words file
path_to_function_words = "function-words.txt"
# path to folder with plays
plays = list.files("plays/")
# process all files in folder
for (i in 1:length(plays)) {
  # get drama name
  drama_name = substr(plays[i], 0, 12)
  # read lines of drama file and function words 
  drama = readLines(gsub(" ", "", paste("plays/",plays[i])))
  function_words = readLines(path_to_function_words)
  # run functions
  line_length(drama)
  word_length(drama)
  punctuation(drama)
  char_n_grams(drama, 3)
  char_n_grams(drama, 4)
  word_n_grams(drama, 1)
  word_n_grams(drama, 2)
  word_n_grams(drama, 3)
  function_word_frequency(drama, function_words)
}


