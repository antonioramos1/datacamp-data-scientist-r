# Load qdap
library(qdap)

# Print new_text to the console
print(new_text)

# Find the 10 most frequent terms: term_count
term_count <- freq_terms(new_text, 10)

# Plot term_count
plot(term_count)


# Import text data
tweets <- read.csv("coffee.csv", stringsAsFactors=FALSE)

# View the structure of tweets
str(tweets)

# Print out the number of rows in tweets
print(nrow(tweets))

# Isolate text from tweets: coffee_tweets
coffee_tweets <- tweets$text


# Load tm
library(tm)

# Make a vector source: coffee_source
coffee_source <- VectorSource(coffee_tweets)


## coffee_source is already in your workspace

# Make a volatile corpus: coffee_corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
print(coffee_corpus)

# Print data on the 15th tweet in coffee_corpus
print(coffee_corpus[[15]])

# Print the content of the 15th tweet in coffee_corpus
print(coffee_corpus[[15]][1])


# Print example_text to the console
example_text

# Create a DataframeSource: df_source
df_source <- DataframeSource(example_text)

# Convert df_source to a corpus: df_corpus
df_corpus <- VCorpus(df_source)

# Examine df_corpus
df_corpus

# Examine the first df_corpus document
df_corpus[1]

# Create a VectorSource on column 3: vec_source
vec_source <- VectorSource(example_text[,2])

# Convert vec_source to a corpus: vec_corpus
vec_corpus <- VCorpus(vec_source)

# Examine vec_corpus
print(vec_corpus)

# Examine the first vec_corpus document
vec_corpus[[1]][1]


# Create the object: text
text <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% awake and began drinking coffee in front of her computer."

# All lowercase
tolower(text)

# Remove punctuation
removePunctuation(text)

# Remove numbers
removeNumbers(text)

# Remove whitespace
stripWhitespace(text)


## text is still loaded in your workspace

# Remove text within brackets
bracketX(text)

# Replace numbers with words
replace_number(text)

# Replace abbreviatrions
replace_abbreviation(text)

# Replace contractions
replace_contraction(text)

# Replace symbols with words
replace_symbol(text)


## text is preloaded into your workspace

# List standard English stop words
stopwords("en")

# Print text without standard stop words
print(removeWords(text, stopwords("en")))

# Add "coffee" and "bean" to the list: new_stops
new_stops <- c("coffee", "bean", stopwords("en"))

# Remove stop words from text
removeWords(text, new_stops)


# Create complicate
complicate <- c("complicated", "complication", "complicatedly")

# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)

# Create the completion dictionary: comp_dict
comp_dict <- "complicate"

# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc, comp_dict)

# Print complete_text
print(complete_text)


# Remove punctuation: rm_punc
rm_punc <- removePunctuation(text_data)

# Create character vector: n_char_vec
n_char_vec <- unlist(strsplit(rm_punc, split = ' '))

# Perform word stemming: stem_doc
stem_doc <- stemDocument(n_char_vec)

# Print stem_doc
print(stem_doc)

# Re-complete stemmed document: complete_doc
complete_doc <- stemCompletion(stem_doc, comp_dict)

# Print complete_doc
print(complete_doc)


# Alter the function code to match the instructions
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}

# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(tweet_corp)

# Print out a cleaned up tweet
print(clean_corp[[227]][1])

# Print out the same tweet in original form
print(tweets$text[227])


# Create the dtm from the corpus: coffee_dtm
coffee_dtm <- DocumentTermMatrix(clean_corp)

# Print out coffee_dtm data
print(coffee_dtm)

# Convert coffee_dtm to a matrix: coffee_m
coffee_m <-  as.matrix(coffee_dtm)

# Print the dimensions of coffee_m
print(dim(coffee_m))

# Review a portion of the matrix
coffee_m[148:150, 2587:2590]


# Create a TDM from clean_corp: coffee_tdm
coffee_tdm <- TermDocumentMatrix(clean_corp)

# Print coffee_tdm data
print(coffee_tdm)

# Convert coffee_tdm to a matrix: coffee_m
coffee_m <- as.matrix(coffee_tdm)

# Print the dimensions of the matrix
print(dim(coffee_m))

# Review a portion of the matrix
coffee_m[2587:2590, 148:150]
