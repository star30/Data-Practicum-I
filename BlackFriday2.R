BlackFriday2 <- read_excel("~/Documents/Data Practicum/BlackFriday2.xlsm")
View(BlackFriday2)
str(BlackFriday)
BlackFriday$Gender[BlackFriday$Gender == "F"] <- 0
BlackFriday$Gender[BlackFriday$Gender == "M"] <- 1
BlackFriday$Age[BlackFriday$Age == "0-17"] <- 0
BlackFriday$Age[BlackFriday$Age == "18-25"] <- 1
BlackFriday$Age[BlackFriday$Age == "26-35"] <- 2
BlackFriday$Age[BlackFriday$Age == "36-45"] <- 3
BlackFriday$Age[BlackFriday$Age == "46-50"] <- 4
BlackFriday$Age[BlackFriday$Age == "51-55"] <- 5
BlackFriday$Age[BlackFriday$Age == "55+"] <- 6
BlackFriday$City_Category[BlackFriday$City_Category == "A"] <- 1
BlackFriday$City_Category[BlackFriday$City_Category == "B"] <- 2
BlackFriday$City_Category[BlackFriday$City_Category == "C"] <- 3
BlackFriday$Stay_In_Current_City_Years[BlackFriday$Stay_In_Current_City_Years == "4+"] <- 4
BlackFriday$User_ID <- NULL
BlackFriday$Product_ID <- NULL
BlackFriday$'Age' <-as.numeric(BlackFriday$'Age')
BlackFriday$'City_Category' <-as.numeric(BlackFriday$'City_Category')
BlackFriday$'Stay_In_Current_City_Years' <-as.numeric(BlackFriday$'Stay_In_Current_City_Years')
BlackFriday$'Gender' <-as.numeric(BlackFriday$'Gender')
str(BlackFriday)
corrmatrix <- round(cor(BlackFriday),2)
head(corrmatrix)
library(reshape2)
melted_BlackFriday <- melt(corrmatrix)
head(melted_BlackFriday)
library(ggplot2)
ggplot(data = melted_BlackFriday, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
corr(BlackFriday$Purchase, BlackFriday$Product_Category_3)
linearMod <- lm(Purchase ~ Product_Category_3, data = BlackFriday)
print(linearMod)
summary(linearMod)
set.seed(100000)
trainingRowIndex <- sample(1:nrow(BlackFriday), 0.8*nrow(BlackFriday))
trainingData <- BlackFriday[trainingRowIndex, ]
testData <- BlackFriday[-trainingRowIndex, ]
lmMod <- lm(Purchase ~ Product_Category_3, data = trainingData)
purchasePred <- predict(lmMod, testData)
summary(lmMod)










