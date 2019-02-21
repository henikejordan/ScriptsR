#load data
data(iris)
head(iris)
summary(iris)

#pca
myPr <- prcomp(iris[, -5], scale=TRUE)
plot(iris$Sepal.Length, iris$Sepal.Width)
plot(scale(iris$Sepal.Length), scale(iris$Sepal.Width))
myPr
summary(myPr)
plot(myPr, type="l")
biplot(myPr, scale=0)

#extract pc scores
str(myPr)
myPr$x
iris2 <- cbind(iris, myPr$x[,1:2])
head(iris2)

#plot with ggplot
install.packages("ggplot2")
library(ggplot2)

ggplot(iris2, aes(PC1, PC2, col=Species, fill=Species)) +
	stat_ellipse(geom="polygon", col="black", alpha=0.5) +
	geom_point(shape=21, col="black")
	
#correlation between vars and pcs
cor(iris[, -5], iris2[,6:7])