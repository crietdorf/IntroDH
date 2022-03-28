# activating the package
library("stylo")
library("readr")


function_words <- read_file("function-words.txt")
vector_of_features <- txt.to.words(function_words)


#### reading the txts ####

# assigns the file contents to a string, uses readr package
text_co <- read_file("to_be_analyzed/Shakespeare_CO_ComedyOfErrors.txt")
text_tr <- read_file("to_be_analyzed/Shakespeare_TR_Othello.txt")
text_hi <- read_file("to_be_analyzed/Shakespeare_HI_KingJohn.txt")
vector_co <- txt.to.words(text_co)
vector_tr <- txt.to.words(text_tr)
vector_hi <- txt.to.words(text_hi)


# makes the table of frequencies (uses one sample text and funtion word text)
test_co <- make.table.of.frequencies(vector_co, complete.word.list, absent.sensitive = FALSE, relative = TRUE)
test_tr <- make.table.of.frequencies(vector_tr, complete.word.list, absent.sensitive = FALSE, relative = TRUE)
test_hi <- make.table.of.frequencies(vector_hi, complete.word.list, absent.sensitive = FALSE, relative = TRUE)



corp_all <- load.corpus(
    files = "all",
    corpus.dir = "corpus",
    encoding = "UTF-8"
)
corp_all = lapply(corp_all, txt.to.words)

corp_reference <- load.corpus(
    files = "all",
    corpus.dir = "corpus_ref",
    encoding = "UTF-8"
)
corp_reference = lapply(corp_reference, txt.to.words)

corp_candidate <- load.corpus(
    files = "all",
    corpus.dir = "corpus_candidates",
    encoding = "UTF-8"
)
corp_candidate = lapply(corp_candidate, txt.to.words)

corp_test <- load.corpus(
    files = "all",
    corpus.dir = "to_be_analyzed",
    encoding = "UTF-8"
)


my.corpus.clean = lapply(corp_all, txt.to.words)
complete.word.list = make.frequency.list(my.corpus.clean)
#make.table.of.frequencies(my.corpus.clean, complete.word.list)


freq_table_all <- make.table.of.frequencies(
    corp_all,
    complete.word.list,
    absent.sensitive = FALSE,
    relative = FALSE
)
freq_table_ref <- make.table.of.frequencies(
    corp_reference,
    complete.word.list,
    absent.sensitive = FALSE,
    relative = FALSE
)
freq_table_candidate <- make.table.of.frequencies(
    corp_candidate,
    complete.word.list,
    absent.sensitive = FALSE,
    relative = FALSE
)
freq_table_test <- make.table.of.frequencies(
    corp_test,
    complete.word.list,
    absent.sensitive = FALSE,
    relative = FALSE
)

imposters(reference.set = freq_table_ref, test = test_tr, candidate.set = freq_table_candidate, iterations = 100)

#imposters(
#    freq_table_ref,
#    test = test_hi,
#    freq_table_candidate,
#    iterations = 100,
#    features = 0.4,
#    imposters = 0.7,
#    classes.reference.set = NULL,
#    classes.candidate.set = NULL
#)

