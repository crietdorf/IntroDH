# used libraries
library("stylo")

# path to drama file and for output file
# !!! have to be changed !!!
path_to_drama = "/home/clemens/Dokumente/Uni/22-WiSe/IntroDH_Shakespeare/small-sample.txt"
output_path = "/home/clemens/Dokumente/Uni/22-WiSe/IntroDH_Shakespeare/tri-grams.csv"

# read lines of drama file
drama = readLines(path)

# divide drama text into words
drama_words = txt.to.words(drama)

# create 'empty' list (list with one entry)
all_3_grams = ("")

# split each word into its characters
# and create tri-grams if word length is greater than 2
for (word in drama_words){
  drama_chars = txt.to.features(word, features = "c")

  if (nchar(word)>2) {
    char3grams = make.ngrams(drama_chars,3)
    all_3_grams = append(all_3_grams, char3grams)
  }
}

# convert list into dataframe and count frequencies (remove first entry)
all_3_grams = as.data.frame((table(all_3_grams[-1])))

# order dataframe by frequency (decreasing)
all_3_grams = all_3_grams[order(all_3_grams[,2], decreasing = TRUE), ]

# rename columns
colnames(all_3_grams) = c("tri-gram", "frequency")

# save dataframe to csv file
write.csv(all_3_grams, output_path, row.names = FALSE)
