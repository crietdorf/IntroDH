# used libraries
library("stylo")
library("stringr")

# path to drama file and for output file
# !!! have to be changed !!!
path_to_drama = "/home/clemens/Dokumente/Uni/22-WiSe/IntroDH_Shakespeare/small-sample.txt"
output_path = "/home/clemens/Dokumente/Uni/22-WiSe/IntroDH_Shakespeare/punctuation-marks.csv"

# read lines of drama file
drama = readLines(path)

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

# save dataframe to csv file
write.csv(punctuation, output_path, row.names = FALSE)
