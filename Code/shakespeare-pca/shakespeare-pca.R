# used libraries
library(stylo)
library(stringr)
library(factoextra)
library(data.table)

get_feature_list = function(path){
  ### get list of features ###
  # create empty list
  features = ("")
  
  # extract all features from file
  lines = readLines(path)
  for (line in lines){
    features = append(features, line)
  }
  # save feature list in data frame
  features = features[-1]
  
  return(features)
}

feature_words = function(path, n){
  ### get list of features ###
  features = get_feature_list(path)
  result = as.data.frame(features)
  
  #### get word n grams ####
  # read all plays
  for (i in 1:length(plays)) {
    # get name of drama
    drama_name = str_sub(plays[i], 0, -5)
    drama = readLines(gsub(" ", "", paste("corpus/",plays[i])))
    # create empty list
    all_n_grams = ("")
    for (line in drama){
      # split every line of drama into single words
      words = txt.to.words(line)
      # make n grams of words if line is longer than n-1 words
      if (length(words) > (n-1)){
        all_n_grams = append(all_n_grams, make.ngrams(words, n))
      } 
    }
    count = all_n_grams[-1]
    
    ### get feature frequencies ###
    # calculate n gram frequencies and add them to result
    frequencies = make.table.of.frequencies(count, features, absent.sensitive = FALSE,  relative = TRUE)
    class(frequencies)="vector"
    result[[drama_name]] <- t(frequencies)
  }
  # return result
  return(result)
}

feature_chars = function(path, n){
  ### get list of features ###
  features = get_feature_list(path)
  result = as.data.frame(features)
  result = as.data.frame(features)
  
  #### get character n grams ####
  # read all plays
  for (i in 1:length(plays)) {
    # get name of drama
    drama_name = str_sub(plays[i], 0, -5)
    drama = readLines(gsub(" ", "", paste("corpus/",plays[i])))
    # create empty list
    all_n_grams = ("")
    # divide drama text into words
    drama_words = txt.to.words(drama)
    # split each word into its characters
    # and create n grams if word length is greater than n-1
    for (word in drama_words){
      drama_chars = txt.to.features(word, features = "c")
      if (nchar(word)>(n-1)) {
        char_n_grams = make.ngrams(drama_chars,n)
        all_n_grams = append(all_n_grams, char_n_grams)
      }
    }
    count = all_n_grams[-1]
    
    ### get feature frequencies ###
    # calculate n gram frequencies and add them to result
    frequencies = make.table.of.frequencies(count, features, absent.sensitive = FALSE,  relative = TRUE)
    class(frequencies)="vector"
    result[[drama_name]] <- t(frequencies)
  }
  # return result
  return(result)
}

feature_punct = function(path){
  ### get list of features ###
  features = get_feature_list(path)
  result = as.data.frame(features)
  
  #### get punctuation marks ####
  # read all plays
  for (i in 1:length(plays)) {
    # get name of drama
    drama_name = str_sub(plays[i], 0, -5)
    drama = readLines(gsub(" ", "", paste("corpus/",plays[i])))
    # create 'empty' list (list with one entry)
    punctuation = ("")
    # for each (not empty) text line in drama extract all punctuation marks 
    for (line in drama){
      if (line != ""){
        punctuation = append(punctuation, str_extract_all(line, "[:punct:]")[[1]])
      } 
    }
    count = punctuation[-1]
    
    ### get feature frequencies ###
    # calculate n gram frequencies and add them to result
    frequencies = make.table.of.frequencies(count, features, absent.sensitive = FALSE,  relative = TRUE)
    class(frequencies)="vector"
    result[[drama_name]] <- t(frequencies)
  }
  # return result
  return(result)
}

feature_line_length = function(){
  ### create data frame ###
  features = ("average line length")
  result = as.data.frame(features)
  #### get line length ####
  # read all plays
  for (i in 1:length(plays)) {
    # get name of drama
    drama_name = str_sub(plays[i], 0, -5)
    drama = readLines(gsub(" ", "", paste("corpus/",plays[i])))
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
    count = as.data.frame(word_counter / line_counter)
    
    result[[drama_name]] <- t(count)
  }
  # return result
  return(result)
}

feature_word_lentgh = function(){
  ### create data frame ###
  features = ("average word length")
  result = as.data.frame(features)
  #### get line length ####
  # read all plays
  for (i in 1:length(plays)) {
    # get name of drama
    drama_name = str_sub(plays[i], 0, -5)
    drama = readLines(gsub(" ", "", paste("corpus/",plays[i])))
    # create list of words and characters
    words = txt.to.words(drama)
    char_count = nchar(words)
    # calculate average char count and save result in dataframe
    count = as.data.frame(sum(char_count) / length(char_count))
    
    result[[drama_name]] <- t(count)
  }
  # return result
  return(result)
}


# location of plays
plays = list.files("corpus/")

# run calculations
result1 = feature_words("wordlist-1-grams.txt", 1)
result2 = feature_words("wordlist-2-grams.txt", 2)
result3 = feature_words("wordlist-3-grams.txt", 3)
result4 = feature_chars("wordlist-chars-2.txt", 2)
result5 = feature_chars("wordlist-chars-3.txt", 3)
result6 = feature_chars("wordlist-chars-4.txt", 4)
result7 = feature_punct("wordlist-punctuation.txt")
result8 = feature_word_lentgh()
result9 = feature_line_length()

# combine results to single data frame 
final = rbind(result1, result2)
final = rbind(final, result3)
final = rbind(final, result4)
final = rbind(final, result5)
final = rbind(final, result6)
final = rbind(final, result7)
final = rbind(final, result8)
final = rbind(final, result9)

# save as csv
write.table(final, sep="\t",  "results.csv", row.names = FALSE, col.names=TRUE)

# get list of genres
genres = ("")
lines = readLines("genres.txt")
for (line in lines){
  genres = append(genres, line)
}
genres = genres[-1]

### transpose final results for pca analysis ###
# remember the names
temp <- final$features
# transpose all but the first column
final <- as.data.frame(t(final[,-1]))
# add names again
colnames(final) <- temp
final$myfactor <- factor(row.names(final))

# run pca analysis
res.pca <- prcomp(final[1:30,1:609], scale = TRUE)
# create plot
fviz_pca_ind(res.pca,
             show.clust.cent = FALSE,
             repel = TRUE,
             habillage=genres,
             addEllipses = TRUE,
             title = "The Style of Shakespeare - Genre Analysis - pca"
)

# run k means analysis
final_k3 <- na.omit(final[1:30,1:609])
final_k3 <- scale(final_k3)
k3 <- kmeans(final_k3, centers = 3)
# create plot
fviz_cluster(k3, 
             data = final_k3,
             show.clust.cent = FALSE,
             repel = TRUE,
             title = "The Style of Shakespeare - Genre Analysis - k means")

