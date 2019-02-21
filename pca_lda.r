require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

pca <- prcomp(pessego_tensao[,-1],
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca$sdev^2/sum(pca$sdev^2)

lda <- lda(classe ~ ., 
           pessego_tensao, 
           prior = c(1,1,1,1)/4)

prop.lda = lda$svd^2/sum(lda$svd^2)

plda <- predict(object = lda,
                newdata = pessego_tensao)

dataset = data.frame(classe = pessego_tensao$classe, pca = pca$x, lda = plda$x)

p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = classe, shape = classe), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = classe, shape = classe), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)

library("car")

scatter3d(x = dataset$pca.PC1, y = dataset$pca.PC2, z = dataset$pca.PC3, 
          groups = dataset$classe, surface=FALSE, ellipsoid = TRUE, 
          xlab = paste("PC1 (", percent(prop.pca[1]), ")", sep=""), 
          ylab = paste("PC2 (", percent(prop.pca[2]), ")", sep=""), 
          zlab = paste("PC3 (", percent(prop.pca[3]), ")", sep=""))

scatter3d(x = dataset$lda.LD1, y = dataset$lda.LD2, z = dataset$lda.LD3, 
          groups = dataset$classe, surface=FALSE, ellipsoid = TRUE, 
          xlab = paste("LD1 (", percent(prop.lda[1]), ")", sep=""), 
          ylab = paste("LD2 (", percent(prop.lda[2]), ")", sep=""), 
          zlab = paste("LD3 (", percent(prop.lda[3]), ")", sep=""))

