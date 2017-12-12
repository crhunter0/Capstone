library(quanteda)
library(stringr)
library(data.table)


set.seed(128)
samp_perc <- .10
con <- file("/home/chris/Downloads/en_US/en_US.blogs.txt", "r") 
curline <- (readLines(con,1))
samp_data <- curline
while (length(curline <- readLines(con,1)) != 0) {
    ran_uni <- runif(1)
    if (ran_uni <= samp_perc) {
        samp_data[length(samp_data) + 1]  <- curline
    }
}
close(con) 

con <- file("/home/chris/Downloads/en_US/en_US.news.txt", "r") 
curline <- (readLines(con,1))
#samp_data <- curline
while (length(curline <- readLines(con,1)) != 0) {
    ran_uni <- runif(1)
    if (ran_uni <= samp_perc) {
        samp_data[length(samp_data) + 1]  <- curline
    }
}
close(con)

con <- file("/home/chris/Downloads/en_US/en_US.twitter.txt", "r") 
curline <- (readLines(con,1))
#samp_data <- curline
while (length(curline <- readLines(con,1)) != 0) {
    ran_uni <- runif(1)
    if (ran_uni <= samp_perc) {
        samp_data[length(samp_data) + 1]  <- curline
    }
}
close(con)

samp_data <- gsub("-", " ", samp_data)
samp_data <- gsub("n't", " not", samp_data)
samp_data <- gsub("'ve", " have", samp_data)
samp_data <- gsub("'d ", " had ", samp_data)
samp_data <- gsub("'ll", " will", samp_data)
samp_data <- gsub("'re ", " are ", samp_data)
samp_data <- gsub(" u ", " you ", samp_data)
samp_data <- gsub("'m ", " am ", samp_data)
samp_data <- gsub("c'mon", "come on", samp_data)
samp_data <- gsub("gonna", "going to", samp_data)
samp_data <- gsub("gotta", "got to", samp_data)
samp_data <- gsub("[’']","", samp_data)


training <- as.character()
testing <- as.character()
for(i in 1:length(samp_data)) {
    ran_uni <- runif(1)
    if (ran_uni <= .90) {
        training[length(training) + 1] <- samp_data[i]
    } else
        testing[length(testing) + 1] <- samp_data[i]
}
save(training, file = "/home/chris/Capstone/training.Rdata")
save(testing, file = "/home/chris/Capstone/testing.Rdata")
rm(samp_data)
rm(training)
rm(testing)
gc()
