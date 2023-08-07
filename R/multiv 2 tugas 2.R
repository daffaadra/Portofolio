#### TUGAS 2 ####
library(tidyverse)
data("swiss")
bartlett.test(swiss)

mtcars

library(regclass)
VIF(swiss)

data_new <- cbind(swiss$Fertility, swiss$Agriculture, swiss$Examination,
                  swiss$Education, swiss$Catholic, swiss$Infant.Mortality)

mean(swiss$Fertility)
mean(swiss$Agriculture)
mean(swiss$Examination)
mean(swiss$Education)
mean(swiss$Catholic)
mean(swiss$Infant.Mortality)

S <- cov(data_new); S

eig_val <- eigen(S)$values; eig_val
eig_vec <- eigen(S)$vector; eig_vec

pca <- prcomp(swiss)
summary(pca)
pca$rotation
pca$x

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca)

ggbiplot(pca,ellipse=TRUE,circle=TRUE, labels=rownames(swiss),groups = )

plot(pca$x[,1], pca$x[,2],
     xlab = "1st Principal Component",
     ylab = "2nd Principal Component",
     main = "First Two Principal Components",
     pch = 20)

library(ggfortify)
autoplot(pca,
         main = "Plot Data 2 Dimensi melalui PCA")
plot(pca,
     type = "l",
     main = "Scree Plot")

biplot(swiss,scale=0)

fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")

sum(eig_val)
prop_var <- eig_val/sum(eig_val); prop_var #Proporion of Variance

#Scree Plot (CARA 1)
pca.var <- pca$sdev^2
pca.var.per <- pca.var/sum(pca.var)*100
barplot(pca.var.per,
        main="Scree Plot",
        xlab="Principal Component",
        ylab="Percent Variation")

#Scree Plot (CARA 2)
library(factoextra)
fviz_eig(pca)

## rank 6 measurements variables that contribute most to pc1
loading_scores <- pca$rotation[,1]
var_scores <- abs(loading_scores)
var_score_ranked <- sort(var_scores, decreasing=TRUE)
top_6_var <- names(var_score_ranked[1:6])
top_6_var
pca$rotation[top_6_genes,1]

#### EXAMPLE 1 ####
library(tidyverse)
#load data
data("USArrests")
S <- cov(USArrests)

eig_val <- eigen(S)$values; eig_val
eig_vec <- eigen(S)$vector; eig_vec
sum(eig_val)
prop_var <- eig_val/sum(eig_val) #proporion of variance
prop_var/sum(prop_var)*100

#view first six rows of data
head(USArrests)

#calculate principal components
results <- prcomp(USArrests, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

#reverse the signs of the scores
results$x <- -1*results$x

#display the first six scores
head(results$x)

#plot
biplot(results, scale = 0)

#display states with highest murder rates in original dataset
head(USArrests[order(-USArrests$Murder),])

#calculate total variance explained by each principal component
results$sdev^2 / sum(results$sdev^2)

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:4), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#### EXAMPLE 2 ####
data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep=""),
  paste("ko", 1:5, sep=""))
rownames(data.matrix) <- paste("gene", 1:100, sep="")
for (i in 1:100) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  data.matrix[i,] <- c(wt.values, ko.values)
}
head(data.matrix)
dim(data.matrix)

pca <- prcomp(t(data.matrix), scale=TRUE) 

## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])

## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per,
        main="Scree Plot",
        xlab="Principal Component",
        ylab="Percent Variation")

## now make a fancy looking plot that shows the PCs and the variation:
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

## get the name of the top 10 measurements (genes) that contribute
## most to pc1.
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:2])

top_10_genes ## show the names of the top 10 genes

pca$rotation[top_10_genes,1] ## show the scores (and +/- sign)

#######
##
## NOTE: Everything that follow is just bonus stuff.
## It simply demonstrates how to get the same
## results using "svd()" (Singular Value Decomposition) or using "eigen()"
## (Eigen Decomposition).
##
#######

############################################
##
## Now let's do the same thing with svd()
##
## svd() returns three things
## v = the "rotation" that prcomp() returns, this is a matrix of eigenvectors
##     in other words, a matrix of loading scores
## u = this is similar to the "x" that prcomp() returns. In other words,
##     sum(the rotation * the original data), but compressed to the unit vector
##     You can spread it out by multiplying by "d"
## d = this is similar to the "sdev" value that prcomp() returns (and thus
##     related to the eigen values), but not
##     scaled by sample size in an unbiased way (ie. 1/(n-1)).
##     For prcomp(), sdev = sqrt(var) = sqrt(ss(fit)/(n-1))
##     For svd(), d = sqrt(ss(fit))
##
############################################

svd.stuff <- svd(scale(t(data.matrix), center=TRUE))

## calculate the PCs
svd.data <- data.frame(Sample=colnames(data.matrix),
                       X=(svd.stuff$u[,1] * svd.stuff$d[1]),
                       Y=(svd.stuff$u[,2] * svd.stuff$d[2]))
svd.data

## alternatively, we could compute the PCs with the eigen vectors and the
## original data
svd.pcs <- t(t(svd.stuff$v) %*% t(scale(t(data.matrix), center=TRUE)))
svd.pcs[,1:2] ## the first to principal components

svd.df <- ncol(data.matrix) - 1
svd.var <- svd.stuff$d^2 / svd.df
svd.var.per <- round(svd.var/sum(svd.var)*100, 1)

ggplot(data=svd.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", svd.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", svd.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("svd(scale(t(data.matrix), center=TRUE)")

#### EXAMPLE 12.2.1 ####
library(base)
library(readr)
df <- read_table2("Downloads/Data di Buku Alvin 3rd Edition/T3_8_SONS.DAT", 
                  col_names = FALSE)
mean(df$X1)
mean(df$X2)
cov(df)
df_new <- df[1:2]
S <- cov(df_new)

eig_val <- eigen(S)$values; eig_val
eig_vec <- eigen(S)$vector; eig_vec

results <- prcomp(df_new, scale = TRUE)
biplot(results, scale = 0)

results$rotation <- -1*results$rotation
results$rotation
results$x <- -1*results$x

plot(df_new, xlim=c(150,220), ylim=c(130,170))

#### EXAMPLE 12.2.2 ####
library(readr)
df <- read_table2("Downloads/Data di Buku Alvin 3rd Edition/T8_3_FOOTBALL.DAT", 
                  col_names = FALSE)
df_new <- cbind(df$X2[31:90], df$X3[31:90], df$X4[31:90],
                df$X5[31:90], df$X6[31:90], df$X7[31:90])
S <- cov(df_new)

eig_val <- eigen(S)$values; eig_val
eig_vec <- eigen(S)$vector; eig_vec
sum(eig_val)
prop_var <- eig_val/sum(eig_val) #proporion of variance
prop_var/sum(prop_var)
0.57870737 + 0.23930877 + 0.08289803

pca <- prcomp(df_new, scale = TRUE)
summary(pca)
pca$rotation
pca$x

plot(pca$x[,1], pca$x[,2],
     xlab = "1st Principal Component",
     ylab = "2nd Principal Component",
     main = "First Two Principal Components")
biplot(pca, scale = 0)

fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")

library(ggplot2)
pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

#### EXAMPLE 12.4(d) ####
13.861/19
