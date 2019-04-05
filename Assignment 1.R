library(tidyverse)
library(scatterplot3d)
library(arules)

# Read data file
mydata <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"), header = FALSE)
# Apply headers
names(mydata) <- c("Age", "Workclass", "Fnlwgt", "Education", "Education_num", "Marital_status", "Occupation", "Relationship", "Race", "Sex", "Capital_gain", "Capital_loss", "Hours_per_week", "Native_country", "Income")

# Prompt 2: Data Cleaning

# find elements which has ? in place of NA
null_positions <- mydata == " ?"
# replace elements with NA
is.na(mydata) <- null_positions
# Now dropping the null values
mydata_clean <- na.omit(mydata)
# Now dropping the duplicates
mydata_clean_no_dups <- mydata_clean %>% distinct()
# Removing factor levels which doesn't occur (i.e., zero frequency)
mydata_clean_no_dups <- droplevels(mydata_clean_no_dups)

# Prompt 1: Data Exploration

# Type of Attributes in the data
names(mydata_clean_no_dups)

# Number of categorical attributes in the data
info <- sapply(mydata_clean_no_dups, is.factor)
which(info)

# Printing levels of each categorical attribute
for(i in 1:ncol(mydata_clean_no_dups)){
  if(info[i] == TRUE){
    print(colnames(mydata_clean_no_dups)[i])
    temp <- as.data.frame(table(mydata_clean_no_dups[ , i]))
    print(temp)
    cat ("\n\n\n")
  }
}

# Summary of all the attributes in the data
summary(mydata_clean_no_dups)

# Relationship between Education and Income
dat <- data.frame(prop.table(table(mydata_clean_no_dups$Education,mydata_clean_no_dups$Income), 1))
dat[,3] <- dat[,3]*100
names(dat) <- c("Education","Income","Percentage")
ggplot(data=dat, aes(x=Education, y=Percentage, fill=Income)) + geom_bar(stat="identity")

# Relationship between Education years and Income

dat <- data.frame(table(mydata_clean_no_dups$Income, mydata_clean_no_dups$Education_num))
names(dat) <- c('Income', 'Education_num', 'Count')
ggplot(data=dat, aes(x=Education_num, y=Count, fill=Income, order = as.numeric(Income))) + geom_bar(stat="identity")

# Relationship between Hours per week and Income
myvars <- c("Hours_per_week", "Income")
dat <- data.frame(mydata_clean_no_dups[myvars])

data_by_hrs <- dat %>%
  group_by(Income) %>%
  summarise(n = n(),
            mean_hours = mean(as.numeric(Hours_per_week)))

ggplot(data_by_hrs, aes(x = Income, y = n)) +
  geom_col() +
  labs(x = "Income", y = "N")

ggplot(data_by_hrs, aes(x = Income, y = mean_hours)) +
  geom_col() +
  labs(x = "Income", y = "Mean Hours Per week")

# Relationship between Sex and Income
dat <- data.frame(table(mydata_clean_no_dups$Sex,mydata_clean_no_dups$Income))
names(dat) <- c("Sex","Income","Count")
ggplot(data=dat, aes(x=Income, y=Count, fill=Sex)) + geom_bar(stat="identity")

# Relationship between Age and Sex
ggplot(data = mydata_clean_no_dups) + aes(x=as.numeric(Age), group=Sex, fill=Sex) + 
  geom_histogram(binwidth=1, color='black')

# Relationship between Occupation and Income
dat <- data.frame(table(mydata_clean_no_dups$Occupation,mydata_clean_no_dups$Income))
names(dat) <- c("Occupation","Income","Count")
ggplot(data=dat, aes(x=Occupation, y=Count, fill=Income)) + geom_bar(stat="identity")


# Prompt 3: Data Pre processing

# Aggregation:
aggregate(. ~Income, data = mydata_clean_no_dups, FUN = mean)
aggregate(. ~Income, data = mydata_clean_no_dups, FUN = median)

# Feature Creation

mydata_clean_no_dups[,"WorkclassCategorized"] <- NA
mydata_clean_no_dups$WorkclassCategorized <- mydata_clean_no_dups$Workclass
mydata_clean_no_dups$WorkclassCategorized <- gsub("Federal-gov", "Government", mydata_clean_no_dups$WorkclassCategorized)
mydata_clean_no_dups$WorkclassCategorized <- gsub("Local-gov", "Government", mydata_clean_no_dups$WorkclassCategorized)
mydata_clean_no_dups$WorkclassCategorized <- gsub("State-gov", "Government", mydata_clean_no_dups$WorkclassCategorized)
mydata_clean_no_dups$WorkclassCategorized <- gsub("Self-emp-inc", "Self-Employed", mydata_clean_no_dups$WorkclassCategorized)
mydata_clean_no_dups$WorkclassCategorized <- gsub("Self-emp-not-inc", "Self-Employed", mydata_clean_no_dups$WorkclassCategorized)
mydata_clean_no_dups$WorkclassCategorized <- gsub("Never-worked", "Unemployed", mydata_clean_no_dups$WorkclassCategorized)
mydata_clean_no_dups$WorkclassCategorized <- gsub("Without-pay", "Unemployed", mydata_clean_no_dups$WorkclassCategorized)

# Subset Selection
mydata_subset <- as.data.frame(cbind(mydata_clean_no_dups$Age, mydata_clean_no_dups$WorkclassCategorized, mydata_clean_no_dups$Income))
names(mydata_subset) <- c("Age", "Workclass", "Income")
mydata_subset$Income <- gsub("1", "<=50K",mydata_subset$Income)
mydata_subset$Income <- gsub("2", ">50K",mydata_subset$Income)

dat <- data.frame(table(mydata_subset$Workclass,mydata_subset$Income))
names(dat) <- c("Workclass","Income","Count")
ggplot(data=dat, aes(x=Workclass, y=Count, fill= Income)) + geom_bar(stat="identity")

# Sampling

sampleids <- sample(x = 1:nrow(mydata_clean_no_dups), size = 200)
mydata_sample <- mydata_clean_no_dups[sampleids, ]

# Dimensionality Reduction
scatterplot3d(x = mydata_sample$Age, y = mydata_sample$Education_num, z = mydata_sample$Hours_per_week,color=as.integer(mydata_sample$Income))

# Discretization

# Discretizing Age
plot(mydata_sample$Age, 1:nrow(mydata_sample), ylab="index")
hist(mydata_sample$Age)
cut(mydata_sample$Age, breaks=3)
discretize(mydata_sample$Age, method="interval", breaks=3)
discretize(mydata_sample$Age, method="frequency", breaks=3)

hist(mydata_sample$Age,
     main = "Discretization: interval", sub = "Blue lines are boundaries")
abline(v=discretize(mydata_sample$Age, method="interval",
                    breaks =3,onlycuts=TRUE), col="blue")
hist(mydata_sample$Age,
     main = "Discretization: frequency", sub = "Blue lines are boundaries")
abline(v=discretize(mydata_sample$Age, method="frequency",
                    breaks =3,onlycuts=TRUE), col="blue")


# Discretizing Hours_Per_Week
discretize(mydata_sample$Hours_per_week, method="cluster", breaks=3)
hist(mydata_sample$Hours_per_week,
     main = "Discretization: Cluster", sub = "Blue lines are boundaries")
abline(v=discretize(mydata_sample$Hours_per_week, method="cluster",
                    breaks =3,onlycuts=TRUE), col="blue")


