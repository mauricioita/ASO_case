require(dplyr)
require(stringr)
require(tm)
require(tibble)


# keyword in context function

kwic <- function(text, keyword, reach){
  
  output <- str_extract(text, paste0("([^\\s]+\\s){",reach,"}",keyword,"(\\s[^\\s]+){",reach,"}"))
  
  output <- output[!is.na(output)]
  
  return(output)
  
}


# corpus builder function

corpus_function <- function(text) {
  
  corpus = VCorpus(VectorSource(text),readerControl = list(language="pt"))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords(kind = "portuguese"))
  corpus = tm_map(corpus, stripWhitespace)
  
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, 0.999)
  dataset = as.data.frame(as.matrix(dtm))
  
  dataset <- data.frame(apply(dataset, 2, sum))
  dataset <- tibble::rownames_to_column(dataset, "keyword")
  
  return(dataset)
  
}



