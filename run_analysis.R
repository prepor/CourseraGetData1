library(data.table)

load <- function(name) {
  list(set = read.table(file.path(name, paste("X_", name, ".txt", sep = "")), quote = ""), 
       labels = readLines(file.path(name, paste("y_", name, ".txt", sep = ""))),
       subjects = readLines(file.path(name, paste("subject_", name, ".txt", sep = ""))))
}

merge <- function(x, y) {
  list(set = rbind(x$set, y$set), 
       labels = c(x$labels, y$labels),
       subjects = c(x$subjects, y$subjects))
}

extractInfo <- function(data) {
  features <- read.table("features.txt", col.names = c("num", "name"))
  filteredFeatures <- features[grepl("mean|std", features$name) ,]
  filtered <- data$set[, filteredFeatures$num]
  names(filtered) <- gsub("-", ".", gsub("[()]", "", filteredFeatures$name))
  list(set = filtered,
       labels = data$labels,
       subjects = data$subjects)
}

labelsToActivity <- function() {
  activities <- read.table("activity_labels.txt", col.names = c("label", "activity"))
  res <- activities$activity
  names(res) <- activities$label
  res
}

combine <- function(data, activities) {
  data$set$activity <- sapply(data$labels, function(label) activities[[label]])
  data$set$subject <- data$subjects
  data$set
}

calcAverage <- function(combined) {
  aggregate(subset(combined, select = -c(activity, subject)), 
            list(activity = combined$activity, subject = combined$subject), 
            mean)
}

test <- load("test")
train <- load("train")

merged <- merge(test, train)

extracted <- extractInfo(merged)
activities <- labelsToActivity()
combined <- combine(extracted, activities)
result <- calcAverage(combined)
write.table(result, "result.txt")


