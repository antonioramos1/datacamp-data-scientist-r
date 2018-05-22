# Create dist_rain
dist_rain <- dist(rain[,2])

# View the distance matrix
print(dist_rain)

# Create hc
hc <- hclust(dist_rain)

# Plot hc
plot(hc, labels = rain$city)


# Print the dimensions of tweets_tdm
print(dim(tweets_tdm))

# Create tdm1
tdm1 <- removeSparseTerms(tweets_tdm, sparse = 0.95)

# Create tdm2
tdm2 <- removeSparseTerms(tweets_tdm, sparse = 0.975)

# Print tdm1
print(tdm1)

# Print tdm2
print(tdm2)



# Create tweets_tdm2
tweets_tdm2  <- removeSparseTerms(tweets_tdm, sparse=0.975)

# Create tdm_m
tdm_m <- as.matrix(tweets_tdm2)

# Create tdm_df
tdm_df <- as.data.frame(tdm_m)

# Create tweets_dist
tweets_dist <- dist(tdm_df)

# Create hc
hc <- hclust(tweets_dist)

# Plot the dendrogram
plot(hc)


# Load dendextend
library(dendextend)

# Create hc
hc <- hclust(tweets_dist)

# Create hcd
hcd <- as.dendrogram(hc)

# Print the labels in hcd
print(labels(hcd))

# Change the branch color to red for "marvin" and "gaye"
hcd <- branches_attr_by_labels(hcd, c("marvin", "gaye"), color="red")

# Plot hcd
plot(hcd, main = "Better Dendrogram")

# Add cluster rectangles 
rect.dendrogram(hcd, k=2,border="grey50")


# Create associations
associations <- findAssocs(tweets_tdm, "venti", 0.2)

# View the venti associations
print(associations)

# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]

# Plot the associations_df values (don't change this)
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  theme_gdocs()


# Make tokenizer function 
tokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

# Create unigram_dtm
unigram_dtm <- DocumentTermMatrix(text_corp)

# Create bigram_dtm
bigram_dtm <- DocumentTermMatrix(text_corp, control = list(tokenize = tokenizer))

# Examine unigram_dtm
unigram_dtm

# Examine bigram_dtm
bigram_dtm


# Create bigram_dtm_m
bigram_dtm_m <- as.matrix(bigram_dtm)

# Create freq
freq <- colSums(bigram_dtm_m)

# Create bi_words
bi_words <- names(freq)

# Examine part of bi_words
bi_words[2577:2587]

# Plot a wordcloud
wordcloud(bi_words, freq, max.words=15)


# Create tf_tdm
tf_tdm <- TermDocumentMatrix(text_corp)

# Create tfidf_tdm
tfidf_tdm <- TermDocumentMatrix(text_corp, control = list(weighting = weightTfIdf))

# Create tf_tdm_m
tf_tdm_m <- as.matrix(tf_tdm)

# Create tfidf_tdm_m 
tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Examine part of tf_tdm_m
tf_tdm_m[508:509, 5:10]

# Examine part of tfidf_tdm_m
tfidf_tdm_m[508:509, 5:10]


# Select specific columns
tweets <- tweets[, c("num", "text", "screenName", "created" )]

# Rename columns
names(tweets)[1] <- "doc_id"

# Set the data frame source schema: docs
docs <- DataframeSource(tweets)

# Make a clean volatile corpus: text_corpus
text_corpus <- clean_corpus(VCorpus(docs))

# Print the first doc's doc_id  
text_corpus[[1]]$meta$id

# Examine the first doc content
text_corpus[[1]]$content

# Access the first doc metadata
meta(text_corpus[1])
