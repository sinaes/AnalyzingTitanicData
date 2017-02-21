# reading the csv files
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv",header = TRUE)

# adding survived column to test data
test.survived <- data.frame(Survived = rep("None" , nrow(test)), test[,])

# survived column added as a first column! compared with train data it should be in the second column
test.survived <- test.survived[c(2,1,3,4:12)]

# combining two data frame into single one
data.combined <- rbind(train, test.survived)

# changing the type of pclass column from int into factor
data.combined$Pclass <- as.factor(data.combined$Pclass)

# changing the type of survived column from chr into factor
data.combined$Survived <- as.factor(data.combined$Survived)

# 
table(data.combined$Survived)
table(data.combined$Pclass)

library(ggplot2)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x =Pclass , fill = factor(Survived))) + 
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total count") +
  labs(fill = "Survived")

head(as.character(data.combined$Name))

length(as.character(data.combined$Name))
length(unique(as.character(data.combined$Name)))

# finding the duplicate names
dup_name <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])
data.combined[which(data.combined$Name %in% dup_name),]

library(stringr)
# finding which of the names has Miss, Mrs prefix
Miss_names <- data.combined[which(str_detect(data.combined$Name,"Miss")),]
Mrs_names <- data.combined[which(str_detect(data.combined$Name,"Mrs")),]
Males <- data.combined[which(data.combined$Sex == "male"),]

# define a function to return the title of peaple to a variable
find_title <- function(name){
  name <- as.character(name)
  if (length(grep("Miss.",name)) > 0) {
    return("Miss.")
  } else if (length(grep("Mrs." , name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr." , name)) > 0) {
    return("Mr.")
  } else if (length(grep("Master." , name)) > 0) {
    return("Master.")
  } else {
    return("Other.")
  }
}

# creating the title vector based on the title of each pearson in titanic dataset
title <- NULL
for(i in 1:nrow(data.combined)) {
  title <- c(title,find_title(data.combined[i,"Name"]))
}

# Adding this vector as a factor to data.combined 
data.combined$title <- as.factor(title)

# Plotting data based on title 
ggplot(data.combined[1:891,], aes(x=title, fill = Survived)) +
  geom_bar(width=0.5) + 
  facet_wrap(~Pclass) +
  ggtitle("Pclass") + 
  xlab("Title") + 
  ylab("Total count") +
  labs(fill="Survived")

# Plotting data based on sex
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total count") +
  labs(fill="Survived")

boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)
# finding thoes misses that travelling alone which means sibsp
# and parch are equal to zero for thos misses
misses_alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses_alone$Age)
# how many of these alone misses have age less than 14.5
length(which(misses_alone$Age <= 14.5))

# let's look at the sibSp distribution
ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("SibSp") +
  ylab("Total count") +
  labs(fill="Survived")

# let's look at the Parch distribution
ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("Parch") +
  ylab("Total count") +
  labs(fill="Survived")

# calculating the family size variable
SibSp.temp <- c(train$SibSp,test$SibSp)
Parch.temp <- c(train$Parch,test$Parch)
data.combined$FamilySize <- Parch.temp+SibSp.temp+1

ggplot(data.combined[1:891,], aes(x=FamilySize, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Family size") +
  ylab("Total count") +
  labs(fill="Survived")

# Analysing data based on ticket number
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]
# taking the first character of ticket variable
ticket_1_char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket,1,1))
unique(tichet_1_char)
data.combined$ticket_1_char <- as.factor(ticket_1_char)
ggplot(data.combined[1:891,],aes(x=ticket_1_char, fill=Survived)) +
  geom_bar(width = .5) +
  ggtitle("Survivablity based on ticket's first char") +
  xlab("Ticket's first character") +
  ylab("Total counts") +
  labs(fill="Survived")
ggplot(data.combined[1:891,],aes(x=ticket_1_char, fill=Survived)) +
  geom_bar(width = .5) +
  facet_wrap(~Pclass) +
  ggtitle("Survivablity based on ticket's first char") +
  xlab("Ticket's first character") +
  ylab("Total counts") +
  labs(fill="Survived")

# Analysing data based on ticket fare 
summary(data.combined$Fare)
length(unique(data.combined$Fare))
ggplot(data.combined[1:891,],aes(x=Fare)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Ticket fare distribution") +
  xlab("Ticket fare rate") +
  ylab("Total count")

ggplot(data.combined[1:891,],aes(x=Fare, fill=Survived)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Survivablity based on ticket fare") +
  xlab("Ticket fare rate") +
  ylab("Total count") +
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Fare, fill=Survived)) +
  geom_histogram(binwidth = 3) +
  facet_wrap(~title) +
  ggtitle("Survivablity based on ticket fare and title") +
  xlab("Ticket fare rate") +
  ylab("Total count") +
  labs(fill="Survived")

# Analysing data based on cabin
str(data.combined$Cabin)
data.combined$Cabin[1:50]
data.combined$Cabin <- as.character(data.combined$Cabin)
cabin_1_char <- substr(data.combined$Cabin,1,1)
data.combined$Cabin_1_char <- cabin_1_char

ggplot(data.combined[1:891,], aes(Cabin_1_char, fill = Survived)) +
  geom_bar(width = .5) +
  ggtitle("Survivablity based on Cabin's 1st character") +
  xlab("Cabin 1st character") +
  ylab("Total counts") +
  labs(fill="Survived")

ggplot(data.combined[1:891,], aes(Cabin_1_char, fill = Survived)) +
  geom_bar(width = .5) +
  facet_wrap(~Pclass) +
  ggtitle("Survivablity based on Cabin's 1st character and Pclass") +
  xlab("Cabin 1st character") +
  ylab("Total counts") +
  labs(fill="Survived")

# Let's look at the embarked variable
# This shows th port of embarkation: (C = Cherbourg; Q = Queenstown; S = Southampton)
levels(data.combined$Embarked)
table(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(Embarked, fill = Survived))+
  geom_bar(width = 0.5)+
  ggtitle("Survivablity based on the port of embarkation")+
  xlab("The port of embarkation")+
  ylab("Total counts")+
  labs(fill="Survived")

###################################################
# MODELING
# Using RandomForest package
library(randomForest)

rf.label <- as.factor(train$Survived)

# Creating train dataset for randomForest using Pclass and title
rf.train.1 <- data.combined[1:891,c("Pclass","title")]
set.seed(123)
rf1 <- randomForest(x=rf.train.1,y=rf.label,importance = TRUE,ntree = 1000)
rf1
varImpPlot(rf1)

# Creating train dataset for randomForest using Pclass, title and sex
rf.train.2 <- data.combined[1:891,c("Pclass","title","Sex")]
set.seed(123)
rf2 <- randomForest(x=rf.train.2,y=rf.label,importance = TRUE,ntree = 1000)
rf2
varImpPlot(rf2)

# Creating train dataset for randomForest using Pclass, title and fare
rf.train.3 <- data.combined[1:891,c("Pclass","title","Fare")]
set.seed(123)
rf3 <- randomForest(x=rf.train.3,y=rf.label,importance = TRUE,ntree = 1000)
rf3
varImpPlot(rf3)

# Creating train dataset for randomForest using Pclass, title and familysize
rf.train.4 <- data.combined[1:891,c("Pclass","title","FamilySize")]
set.seed(123)
rf4 <- randomForest(x=rf.train.4,y=rf.label,importance = TRUE,ntree = 1000)
rf4
varImpPlot(rf4)

# Creating train dataset for randomForest using Pclass, title, fare and familysize
rf.train.5 <- data.combined[1:891,c("Pclass","title","Fare","FamilySize")]
set.seed(123)
rf5 <- randomForest(x=rf.train.5,y=rf.label,importance = TRUE,ntree = 1000)
rf5
varImpPlot(rf5)

# Creating train dataset for randomForest using Pclass, title, embarked and familysize
rf.train.6 <- data.combined[1:891,c("Pclass","title","Embarked","FamilySize")]
set.seed(123)
rf6 <- randomForest(x=rf.train.6,y=rf.label,importance = TRUE,ntree = 1000)
rf6
varImpPlot(rf6)
(rf7)


