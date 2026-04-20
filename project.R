#Generative AI was assistive in creating this code in discovering how to normalize the data.
#Additional GenAI was used to fix errors in the normalization of the code 

install.packages("readxl")
library(readxl)
df <- read_excel("survey_R.xlsx")

grey_cols <- function(n) grey(seq(0.2, 0.9, length.out = n))

# Plot 1: Classification (column 6)
df[[6]][grepl("sophomore", df[[6]], ignore.case = TRUE)] <- "Sophomore"
classif <- table(df[[6]])
pct <- round(100 * prop.table(classif), 1)
labels <- paste0(names(classif), "\n", pct, "%")
pie(classif, labels = labels, main = "Classification", col = grey_cols(length(classif)))

# Plot 2: School (column 7)
df[[7]][grepl("journalism", df[[7]], ignore.case = TRUE)] <- "School of Journalism & Communications"
science <- table(df[[7]])
pct <- round(100 * prop.table(science), 1)
labels <- paste0(names(science), "\n", pct, "%")
pie(science, labels = labels, main = "School of?", col = grey_cols(length(science)))

# Plot 3: Satisfied with quality of instruction (column 8)
df[[8]] <- trimws(df[[8]])
df[[8]][grepl("^very satisfied", df[[8]], ignore.case = TRUE)] <- "Very Satisfied"
df[[8]][grepl("^very dissatisfied", df[[8]], ignore.case = TRUE)] <- "Very Dissatisfied"
df[[8]][grepl("^somewhat satisfied", df[[8]], ignore.case = TRUE)] <- "Somewhat Satisfied"
df[[8]][grepl("^somewhat dissatisfied", df[[8]], ignore.case = TRUE)] <- "Somewhat Dissatisfied"
df[[8]][grepl("^neither", df[[8]], ignore.case = TRUE)] <- "Neither Satisfied nor Dissatisfied"
satisfied <- table(df[[8]])
pct <- round(100 * prop.table(satisfied), 1)
labels <- paste0(names(satisfied), "\n", pct, "%")
pie(satisfied, labels = labels, main = "Are students satisfied with the quality of instruction in your major?", col = grey_cols(length(satisfied)))

# Plot 4: Is your degree relevant for your career goals? (column 9)
df[[9]] <- gsub("\u00a0", " ", df[[9]])
df[[9]] <- gsub("\\s+", " ", trimws(df[[9]]))
df[[9]] <- trimws(df[[9]])
df[[9]][grepl("^very relevant", df[[9]], ignore.case = TRUE)] <- "Very Relevant"
df[[9]][grepl("^very irrelevant", df[[9]], ignore.case = TRUE)] <- "Very Irrelevant"
df[[9]][grepl("^somewhat relevant", df[[9]], ignore.case = TRUE)] <- "Somewhat Relevant"
df[[9]][grepl("^somewhat irrelevant", df[[9]], ignore.case = TRUE)] <- "Somewhat Irrelevant"
df[[9]][grepl("^neither", df[[9]], ignore.case = TRUE)] <- "Neither Relevant nor Irrelevant"
relevance <- table(df[[9]])
pct <- round(100 * prop.table(relevance), 1)
labels <- paste0(names(relevance), "\n", pct, "%")
pie(relevance, labels = labels, main = "Is your degree relevant for your career goals?", col = grey_cols(length(relevance)))

# Plot 5: How helpful is your academic advisor (column 10)
df[[10]] <- trimws(df[[10]])
df[[10]][grepl("^very helpful", df[[10]], ignore.case = TRUE)] <- "Very Helpful"
df[[10]][grepl("^not helpful", df[[10]], ignore.case = TRUE)] <- "Not Helpful"
df[[10]][grepl("^somewhat helpful", df[[10]], ignore.case = TRUE)] <- "Somewhat Helpful"
df[[10]][grepl("^not very helpful", df[[10]], ignore.case = TRUE)] <- "Not Very Helpful"
df[[10]][grepl("^neither", df[[10]], ignore.case = TRUE)] <- "Neither Helpful nor Unhelpful"
advisor <- table(df[[10]])
pct <- round(100 * prop.table(advisor), 1)
labels <- paste0(names(advisor), "\n", pct, "%")
pie(advisor, labels = labels, main = "How helpful is your academic advisor?", col = grey_cols(length(advisor)))

# Plot 6: How manageable is your current academic workload? (column 11)
df_raw <- read_excel("survey_R.xlsx")
df[[11]] <- trimws(as.character(df_raw[[11]]))
df[[11]] <- factor(df[[11]],
                   levels = c("1", "2", "3", "4", "5"),
                   labels = c("1 - Manageable",
                              "2 - Somewhat Manageable",
                              "3 - Neutral",
                              "4 - Somewhat Unmanageable",
                              "5 - Not Manageable"))
manage <- table(df[[11]])
manage <- manage[manage > 0]
pct <- round(100 * prop.table(manage), 1)
labels <- paste0(names(manage), "\n", pct, "%")
pie(manage, labels = labels, main = "How manageable is your current academic workload? (1-5)", col = grey_cols(length(manage)))

# Plot 7: Recommendation (column 12)
recommend <- table(df[[12]])
pct <- round(100 * prop.table(recommend), 1)
labels <- paste0(names(recommend), "\n", pct, "%")
pie(recommend, labels = labels, main = "Would you recommend this institution?", col = grey_cols(length(recommend)))

