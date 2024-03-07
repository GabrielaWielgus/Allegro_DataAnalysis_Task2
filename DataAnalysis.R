questions <- read.csv("questions.csv", header = TRUE, sep = ";")
data_from_users <- read.csv("Data-1.csv", header = TRUE, sep = ";")

# cleaning data

# "q8" to numeric
data_from_users$q8 <- as.numeric(data_from_users$q8)

# delete if "q8" is Na (so it was string before conversion)
data_from_users <- data_from_users[!is.na(data_from_users$q8), ]

#change age to year
data_from_users$q8 <- ifelse(nchar(data_from_users$q8) != 4,
                             2024 - data_from_users$q8, 
                             data_from_users$q8)

# add age column
questions <- rbind(questions, c("q9", "Age"))
data_from_users$q9 <- 2024 - data_from_users$q8

# delete if younger than 18
data_from_users <- data_from_users[!(data_from_users$q9 < 18), ]

# delete if older than 100
data_from_users <- data_from_users[!(data_from_users$q9 > 100), ]

# split the answers in q7
data_from_users$q7[data_from_users$q7 == "-"] <- ""
data_from_users$q7 <- lapply(strsplit(data_from_users$q7, ","), function(x) {
  if (length(x) == 0 || (length(x) == 1 && x == "")) {
    "not answered"
  } else {
    x
  }
})


# DATA ANALYSIS

# question one

# global rating
mean_q2 <- mean(data_from_users$q2, na.rm = TRUE)

# age groups
age_groups <- cut(data_from_users$q9, breaks = c(18, 29, 39, 49, 59, Inf), labels = c("18-29", "30-39", "40-49", "50-59", "60+"))
mean_q2_by_age_groups <- tapply(data_from_users$q2, age_groups, mean, na.rm = TRUE)

# satisfaction level groups
data_from_users$satisfaction_group <- cut(data_from_users$q2, breaks = c(0, 6, 8, Inf), labels = c("0-6", "7-8", "9-10"))
age_by_satisfaction_mean <- aggregate(q9 ~ satisfaction_group, data = data_from_users, FUN = mean)

calculate_mode <- function(x) {
  ux <- unique(x)
  counts <- tabulate(match(x, ux))
  ux[which.max(counts)]
}
print(age_by_satisfaction_mean)

device_by_satisfaction <- aggregate(q1 ~ satisfaction_group, data = data_from_users, FUN = calculate_mode)
print(device_by_satisfaction)

# mode device by age
device_by_age <- aggregate(q1 ~ age_group, data = data_from_users, FUN = calculate_mode)
print(device_by_age)

# Analyze the distribution of responses from questions q3 to q5
mean_q3 <- mean(data_from_users$q3, na.rm = TRUE)
mean_q4 <- mean(data_from_users$q4, na.rm = TRUE)
mean_q5 <- mean(data_from_users$q5, na.rm = TRUE)

# Output the results
cat("Global Rating (NPS):", mean_q2, "\n")
cat("Mean Rating for Pricing (q3):", mean_q3, "\n")
cat("Mean Rating for Product Availability (q4):", mean_q4, "\n")
cat("Mean Rating for Website Usability (q5):", mean_q5, "\n")

# question two
buying_interest_groups <- split(data_from_users, data_from_users$q6)

# Calculate mean NPS and ratings for each buying intention group
mean_q2_by_buying_groups <- lapply(buying_interest_groups, function(group) {
  mean(group$q2, na.rm = TRUE)
})

# Calculate mean ratings for pricing (q3), product availability (q4), and website usability (q5) for each buying intention group
mean_q3_by_buying_groups <- lapply(buying_interest_groups, function(group) {
  mean(group$q3, na.rm = TRUE)
})

mean_q4_by_buying_groups <- lapply(buying_interest_groups, function(group) {
  mean(group$q4, na.rm = TRUE)
})

mean_q5_by_buying_groups <- lapply(buying_interest_groups, function(group) {
  mean(group$q5, na.rm = TRUE)
})

# Output the results
print("Mean NPS by Buying Interest Groups:")
print(mean_q2_by_buying_groups)

print("Mean Rating for Pricing (q3) by Buying Interest Groups:")
print(mean_q3_by_buying_groups)

print("Mean Rating for Product Availability (q4) by Buying Interest Groups:")
print(mean_q4_by_buying_groups)

print("Mean Rating for Website Usability (q5) by Buying Interest Groups:")
print(mean_q5_by_buying_groups)

# question three
data_from_users$age_group <- cut(data_from_users$q9, breaks = c(18, 29, 39, 49, 59, Inf), labels = c("18-29", "30-39", "40-49", "50-59", "60+"))
intentions_by_age <- table(data_from_users$age_group, data_from_users$q6)
intentions_proportions <- prop.table(intentions_by_age, margin = 1) * 100

intentions_proportions <- as.data.frame(intentions_proportions)
colnames(intentions_proportions) <- c("age_group", "answer", "per-cent")
intentions_proportions <- intentions_proportions[order(intentions_proportions$age_group), ]

#Output the results
print(intentions_proportions)

# question four
q7_values <- unlist(data_from_users$q7)
q7_values <- na.omit(q7_values)
q7_counts <- table(q7_values)
total_respondents <- length(q7_values)
q7_percentages <- prop.table(q7_counts) * 100

q7_percentages <- as.data.frame(q7_percentages)
colnames(q7_percentages) <- c("answer", "per-cent")

#Output the results
print(q7_percentages)

# Text analysis on responses to question q7 -- error to fix in future ________
library(tm)
library(SnowballC)
library(wordcloud)
library(sentimentr)

corpus <- Corpus(VectorSource(q7_values))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

corpus <- corpus[!sapply(corpus, function(x) length(x) == 0)]

dtm <- DocumentTermMatrix(corpus)
word_freq <- colSums(as.matrix(dtm))

set.seed(123)
wordcloud(names(word_freq), word_freq, max.words = 50, colors = brewer.pal(8, "Dark2"))
unlisted_responses <- unlist(q7_values)

sentiment <- sentiment_by(unlisted_responses)
sentiment <- sentiment[complete.cases(sentiment),]
summary(sentiment)
#_________________________________________________________________________________________

# question five
hist(data_from_users$q9, xlab = "Age", main = "Age distribution")
age_table <- table(age_groups)
correlation <- cor(data_from_users$q9, data_from_users$q2, method = "pearson")
correlation_test <- cor.test(data_from_users$q9, data_from_users$q2, method = "pearson")

print(paste("Correlation:", correlation))
print(paste("p-value:", correlation_test$p.value))

print(correlation)

#Correlation plot
plot(data_from_users$q9, data_from_users$q2, 
     xlab = "Age", ylab = "NPS",
     main = "Pearson Correlation age with NPS")

text(x = min(data_from_users$q9), y = max(data_from_users$q2), 
     paste("Correaltion:", round(correlation, 2)), pos = 4)

# Analysis of the correlation between age groups and NPS scores as well as ratings from questions q3 to q5.
correlation_age_ratings <- cor.test(data_from_users$q9, data_from_users$q2, method = "pearson")
correlation_q3 <- cor.test(data_from_users$q9, data_from_users$q3, method = "pearson")
correlation_q4 <- cor.test(data_from_users$q9, data_from_users$q4, method = "pearson")
correlation_q5 <- cor.test(data_from_users$q9, data_from_users$q5, method = "pearson")

# Output the results
print("Correlation between age and NPS:")
print(correlation_age_ratings)
print("Correlation between age and Pricing (q3):")
print(correlation_q3)
print("Correlation between age and Product Availability (q4):")
print(correlation_q4)
print("Correlation between age and Website Usability (q5):")
print(correlation_q5)
