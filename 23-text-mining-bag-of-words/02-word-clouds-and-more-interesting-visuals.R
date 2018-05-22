## coffee_tdm is still loaded in your workspace

# Create a matrix: coffee_m
coffee_m <- as.matrix(coffee_tdm)

# Calculate the rowSums: term_frequency
term_frequency <- rowSums(coffee_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)

# View the top 10 most common words
print(term_frequency[1:10])

# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col="tan", las=2)


# Create frequency
frequency <- freq_terms(tweets$text, top = 10, at.least = 3, 
                        stopwords = "Top200Words")

# Make a frequency barchart
plot(frequency)

# Create frequency2
frequency2 <- freq_terms(tweets$text, top = 10, at.least = 3, 
                         stopwords = tm::stopwords("english"))

# Make a frequency2 barchart
plot(frequency2)


## term_frequency is loaded into your workspace

# Load wordcloud package
library(wordcloud)

# Print the first 10 entries in term_frequency
print(term_frequency[1:10])

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors="red")


# Add new stop words to clean_corpus()
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "amp", "chardonnay", "wine", "glass"))
  return(corpus)
}

# Create clean_chardonnay
clean_chardonnay <- clean_corpus(chardonnay_corp)

# Create chardonnay_tdm
chardonnay_tdm <- TermDocumentMatrix(clean_chardonnay)

# Create chardonnay_m
chardonnay_m <- as.matrix(chardonnay_tdm)

# Create chardonnay_words
chardonnay_words <- rowSums(chardonnay_m)


# Sort the chardonnay_words in descending order
chardonnay_words <- sort(chardonnay_words, decreasing = TRUE)

# Print the 6 most frequent chardonnay terms
print(chardonnay_words[1:6])

# Create chardonnay_freqs
chardonnay_freqs <- data.frame(term=names(chardonnay_words), num=chardonnay_words)

# Create a wordcloud for the values in word_freqs
wordcloud(chardonnay_freqs$term, chardonnay_freqs$num, max.word = 50, colors = "red")


# Print the list of colors
print(colors())

# Print the wordcloud with the specified colors
wordcloud(chardonnay_freqs$term, chardonnay_freqs$num, max.words=100, colors=c("grey80", "darkgoldenrod1", "tomato"))


# List the available colors
display.brewer.all()

# Create purple_orange
purple_orange <- brewer.pal(10, "PuOr")

# Drop 2 faintest colors
purple_orange <- purple_orange[-(1:2)]

# Create a wordcloud with purple_orange palette
wordcloud(chardonnay_freqs$term, chardonnay_freqs$num, max.words = 100, colors = purple_orange)


# Create all_coffee
all_coffee <- paste(coffee_tweets$text, collapse=" ")

# Create all_chardonnay
all_chardonnay <- paste(chardonnay_tweets$text, collapse= " ")

# Create all_tweets
all_tweets <- c(all_coffee, all_chardonnay)

# Convert to a vector source
all_tweets <- VectorSource(all_tweets)

# Create all_corpus
all_corpus <- VCorpus(all_tweets)


# Clean the corpus
all_clean <- clean_corpus(all_corpus)

# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)

# Create all_m
all_m <- as.matrix(all_tdm)

# Print a commonality cloud
commonality.cloud(all_m, colors = "steelblue1", max.words = 100)


# Clean the corpus
all_clean <- clean_corpus(all_corpus)

# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)

# Give the columns distinct names
colnames(all_tdm) <-  c("coffee", "chardonnay")

# Create all_m
all_m <- as.matrix(all_tdm)

# Create comparison cloud
comparison.cloud(all_m, colors=c("orange", "blue"), max.words=50)


# Create common_words
common_words <- subset(all_tdm_m, all_tdm_m[, 1] > 0 & all_tdm_m[, 2] > 0)

# Create difference
difference <- abs(common_words[, 1] - common_words[, 2])

# Combine common_words and difference
common_words <- cbind(common_words, difference)

# Order the data frame from most differences to least
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

# Create top25_df
top25_df <- data.frame(x = common_words[1:25, 1], 
                       y = common_words[1:25, 2], 
                       labels = rownames(common_words[1:25, ]))

# Create the pyramid plot
pyramid.plot(top25_df$x, top25_df$y, labels = top25_df$labels, 
             gap = 8, top.labels = c("Chardonnay", "Words", "Coffee"), 
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)


# Word association
word_associate(chardonnay_tweets$text, match.string = c("marvin"), 
               stopwords = c(Top200Words, "chardonnay", "amp"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

# Add title
title(main = "Chardonnay Tweets Associated with Marvin")


# Plot a dendrogram
plot(hc)
