library(magrittr)
library(readr)
library(tidyr)
# library(plyr)
library(dplyr)
# library(lubridate)

library(caret)

library(ggplot2)

set.seed(2016-10-14)

# Local data files
train.file <- 'data/pml-training.csv'
valid.file <- 'data/pml-testing.csv'

# Get the data if not yet downloaded
if (!dir.exists('data/')) {
    dir.create('data/')
}
if (!file.exists(train.file)) {
    message("Download training set")
    train.url = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
    download.file(url = train.url, destfile = train.file)
    rm(train.url)
}
if (!file.exists(valid.file)) {
    message("Download testing set")
    test.url = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
    download.file(url = test.url, destfile = valid.file)
    rm(test.url)
}

# Import the datafiles
message("Load datasets")
col_types <- paste0('_ciicci', paste0(rep("c", 152), collapse = ""), collapse = "")
train.data <- suppressMessages(read_csv(train.file,
                                        progress = FALSE,
                                        na = c('', 'NA', '#DIV/0!'),
                                        col_types = paste0(col_types, "c", collapse = "")))
valid.data <- suppressMessages(read_csv(valid.file,
                                        progress = FALSE,
                                        na = c('', 'NA', '#DIV/0!'),
                                        col_types = paste0(col_types, "i", collapse = "")))

# Prepare the data for further analysis
message("Convert datatypes")
train.data %<>%
    mutate(
        user_name = factor(user_name),
        new_window = new_window == "yes",
        classe = factor(classe)
    )
valid.data %<>%
    mutate(
        user_name = factor(user_name),
        new_window = new_window == "yes"
    )

train.classe <- train.data$classe
train.data %<>% select(-classe)
valid.id <- valid.data$problem_id
valid.data %<>% select(-problem_id)

message("Remove mostly empty variables")
missing <- sapply(train.data, function(x)mean(is.na(x)))
# plot(sapply(seq(1, 0, by = -0.01), function(x)mean(missing <= x)), seq(0, 1, by = 0.01), type = "l", xlim = c(0,1))
train.data <- train.data[,missing < 0.1] %>% select(-num_window)
valid.data <- valid.data[,missing < 0.1] %>% select(-num_window)

# Split data to training and testing data, so we can use the original testing
# set as validation set
message("Split training data to training and testing datasets")
intrain <- createDataPartition(train.classe, p = 0.65, list = FALSE)
test.data <- train.data[-intrain,]
test.classe <- train.classe[-intrain]
train.data <- train.data[intrain,]
train.classe <- train.classe[intrain]

# message("Preprocess datasets")
# prepObj <- preProcess(train.data,
#                       method = c("nzv", "pca"),
#                       thresh = .99)
# train.data %<>% predict(prepObj, .)
# test.data %<>% predict(prepObj, .)
# valid.data %<>% predict(prepObj, .)

message("Train random forest")
fit <- train(y = train.classe, x = train.data, method = "rpart")
