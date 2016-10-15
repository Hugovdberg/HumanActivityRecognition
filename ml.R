library(magrittr)
library(readr)
library(tidyr)
# library(plyr)
library(dplyr)
# library(lubridate)

library(log4r)

library(caret)

library(ggplot2)

set.seed(2016-10-14)

logger <- create.logger(logfile = "", level = "DEBUG")
info <- function(message){log4r::info(logger, message)}

# Local data files
train.file <- 'data/pml-training.csv'
valid.file <- 'data/pml-testing.csv'

# Get the data if not yet downloaded
if (!dir.exists('data/')) {
    dir.create('data/')
}
if (!file.exists(train.file)) {
    info("Download training set")
    train.url = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
    download.file(url = train.url, destfile = train.file)
    rm(train.url)
}
if (!file.exists(valid.file)) {
    info("Download testing set")
    test.url = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
    download.file(url = test.url, destfile = valid.file)
    rm(test.url)
}

# Import the datafiles
info("Load datasets")
col_types <- paste0('_ciicci', paste0(rep("d", 152), collapse = ""), collapse = "")
train.data <- read_csv(train.file,
                       progress = FALSE,
                       na = c('', 'NA', '#DIV/0!'),
                       col_types = paste0(col_types, "c", collapse = ""))
valid.data <- read_csv(valid.file,
                       progress = FALSE,
                       na = c('', 'NA', '#DIV/0!'),
                       col_types = paste0(col_types, "i", collapse = ""))

# Prepare the data for further analysis
info("Convert datatypes")
train.data %<>%
    mutate(
        user_name = factor(user_name),
        new_window = new_window == "yes",
        classe = factor(classe)
    )# %>% sample_n(3000)
valid.data %<>%
    mutate(
        user_name = factor(user_name),
        new_window = new_window == "yes"
    )

train.classe <- train.data$classe
train.data %<>% select(-classe)
valid.id <- valid.data$problem_id
valid.data %<>% select(-problem_id)

info("Remove mostly empty variables")
missing <- sapply(train.data, function(x)mean(is.na(x)))
# plot(sapply(seq(1, 0, by = -0.01), function(x)mean(missing <= x)), seq(0, 1, by = 0.01), type = "l", xlim = c(0,1))
train.data <- train.data[,missing < 0.1] %>% select(-num_window, -cvtd_timestamp)
valid.data <- valid.data[,missing < 0.1] %>% select(-num_window, -cvtd_timestamp)

# Split data to training and testing data, so we can use the original testing
# set as validation set
info("Split training data to training and testing datasets")
intrain <- createDataPartition(train.classe, p = 0.65, list = FALSE)
test.data <- train.data[-intrain,]
test.classe <- train.classe[-intrain]
train.data <- train.data[intrain,]
train.classe <- train.classe[intrain]

info("Preprocess datasets")
prepObj <- preProcess(train.data,
                      method = c("nzv", "pca"),
                      thresh = .9)
train.data %<>% predict(prepObj, .)
test.data %<>% predict(prepObj, .)
valid.data %<>% predict(prepObj, .)

info("Train random forest")
fit <- train(y = train.classe, x = train.data, method = "rf")
info("Training finished")

train.tab <- table(train.classe, predict(fit))
test.tab <- table(test.classe, predict(fit, test.data))

print(sum(diag(train.tab))/sum(train.tab))
print(sum(diag(test.tab))/sum(test.tab))
