library("httr")
library("readr")

# get metadata from the dracor corpus for shakespeare plays 
metadata <- content(GET("https://dracor.org/api/corpora/shake/metadata/csv"))
write_csv(metadata, file = 'metadata.csv')
