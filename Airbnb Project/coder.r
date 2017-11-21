# Project to Detect the best Location for a user to live in New York City depending on the parameters stated in the dataset.

library(readr)
airbnb <- read_csv("C:/Users/wyadavh/Desktop/tomslee_airbnb_new_york_1196_2017-05-06.csv")
View(airbnb)
rapply(airbnb,function(x)length(unique(x)))

airbnb["borough"][airbnb["borough"] == "Manhattan"] <- 1
View(airbnb)
airbnb["borough"][airbnb["borough"] == "Bronx"] <- 5
View(airbnb)
airbnb["borough"][airbnb["borough"] == "Brooklyn"] <- 2
airbnb["borough"][airbnb["borough"] == "Queens"] <- 3
airbnb["borough"][airbnb["borough"] == "Staten Island"] <- 4
View(airbnb)

boros <- c("Private room", "Entire home/apt", "Shared room")
for (i in 1:nrow(airbnb)) {
airbnb$room_type[i] <- which(boros == airbnb$room_type[i])
}
airbnb["room_type"][airbnb["room_type"] == "Entire home/apt"] <- 1
airbnb["room_type"][airbnb["room_type"] == "Private room"] <- 3
View(airbnb)
library(randomForest)

sample.ind <- sample(2, nrow(df), replace = T, prob = c(0.8,0.2))
cross.sell.val <- df[sample.ind==2,]
cross.sell.dev <- df[sample.ind==1,]
View(cross.sell.dev)
View(cross.sell.val)
class(cross.sell.dev$borough)

cross.sell.dev$borough = factor(cross.sell.dev$borough)
class(cross.sell.dev$borough)
cross.sell.rf <- randomForest(cross.sell.dev, cross.sell.dev$borough, ntree=200, importance=T)
plot(cross.sell.dev$borough)
cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)
library(e1071)
library(caret)
confusionMatrix(data=cross.sell.dev$predicted.response,
reference=cross.sell.dev$borough,
positive='yes')

