#Loading required packages
library(tidytext) 
library(textdata)
library(janeaustenr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)

#getting original_books from austen_books() grouping by book name
#and introducing new column linenumber by mutate()
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() # Ungroup at the end of a definition!!!

#view the object original_books
View(original_books)

#Tidying text - one word per line
tidy_books <- original_books %>%
  unnest_tokens(word, text)

#view the object tidy_books
View(tidy_books)

#filtering the book Pride & Prejudice from tidy_books
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

#view the object pride_prejudice
View(pride_prejudice)

#getting the polarity of word column using qdap's polarity().It will take some time to execute.
pride_prejudice_pol <- polarity(pride_prejudice$word)

#view the object pride_prejudice_pol
View(pride_prejudice_pol)

#Created a custom function called pol_subsections() which will divide the corpus by polarity score. 
#First, the function accepts a data frame with each row being a sentence or document of the corpus. The data frame is subset anywhere the polarity values are greater than or less than 0. 
#Finally, the positive and negative sentences, non-zero polarities, are pasted with parameter collapse so that the terms are grouped into a single corpus. 
#Lastly, the two documents are concatenated into a single vector of two distinct documents.
pol_subsections <- function(df) {
  x.pos <- subset(df$text, df$polarity > 0)
  x.neg <- subset(df$text, df$polarity < 0)
  x.pos <- paste(x.pos, collapse = " ")
  x.neg <- paste(x.neg, collapse = " ")
  all.terms <- c(x.pos, x.neg)
  return(all.terms)
}

#calling the custom function pol_subsections() to combine positive and negative terms from all column of pride_prejudice_pol
#converting it to Vectorsource and Vcorpus
#creating a Term Document Matrix with removing punctuation and stopwords
#converting the TDM to matrix and set the column names
all_tdm <- pride_prejudice_pol$all %>%
  select(text = text.var, polarity = polarity) %>% 
  pol_subsections() %>%
  VectorSource() %>% 
  VCorpus() %>% 
  TermDocumentMatrix(
    control = list(
      removePunctuation = TRUE,
      stopwords = stopwords(kind = "en")
    )
  ) %>%
  as.matrix() %>%
  set_colnames(c("positive", "negative"))

#creating the comparison cloud from the matrix
comparison.cloud(
  all_tdm,
  # Limiting to 50 words
  max.words = 50,
  # Using darkgreen and darkred colors
  colors = c("darkgreen", "darkred")
)
