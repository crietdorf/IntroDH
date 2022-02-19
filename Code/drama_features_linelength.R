# used libraries
library("stylo")
library("stringr")

# path to drama file and for output file
# !!! have to be changed !!!
path_to_drama = "/home/clemens/Dokumente/Uni/22-WiSe/IntroDH_Shakespeare/small-sample.txt"
output_path = "/home/clemens/Dokumente/Uni/22-WiSe/IntroDH_Shakespeare/average-line-length.csv"

# read lines of drama file
drama = readLines(path)

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
write.csv(line_length, output_path, row.names = FALSE)
