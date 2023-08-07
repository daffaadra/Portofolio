packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

# Example 16.1.2(b)
airline <- matrix(c(0,587,1212,701,1936,604,748,2139,2182,543,
                    587,0,920,940,1745,1188,713,1858,1737,597,
                    1212,920,0,879,831,1726,1631,949,1021,1494,
                    701,940,879,0,1374,968,1420,1645,1891,1220,
                    1936,1745,831,1374,0,2339,2451,347,959,2300,
                    604,1188,1726,968,2339,0,1092,2594,2734,923,
                    748,713,1631,1420,2451,1092,0,2571,2408,205,
                    2139,1858,949,1645,347,2594,2571,0,678,2442,
                    2182,1737,1021,1891,959,2734,2408,678,0,2329,
                    543,597,1494,1220,2300,923,205,2442,2329,0),
                  nrow = 10, ncol = 10, byrow=TRUE); airline

J <- matrix(1, nrow = 10, ncol = 10); J

A_airline <- -1/2*(airline)^2; A_airline
B_airline <- (diag(10)-1/10*J) %*% A_airline %*% (diag(10)-1/10*J); B_airline

#Nilai Eigen
eig_val <- eigen(B_airline)$values; eig_val

#Vektor Eigen (normalized)
eig_vec <- eigen(B_airline)$vectors; eig_vec

#### FIX ####
cek <- matrix(c(0,8,15,15,10,9,7,15,16,14,15,16,7,11,13,
                8,0,17,12,13,13,12,16,17,15,16,17,13,12,16,
                15,17,0,9,16,12,15,5,5,6,5,4,11,10,7,
                15,12,9,0,14,12,13,10,8,8,8,6,15,10,7,
                10,13,16,14,0,8,9,13,14,12,12,12,10,11,11,
                9,13,12,12,8,0,7,12,11,10,9,10,6,6,10,
                7,12,15,13,9,7,0,17,16,15,14,15,10,11,13,
                15,16,5,10,13,12,17,0,4,5,5,3,12,7,6,
                16,17,5,8,14,11,16,4,0,3,2,1,13,7,5,
                14,15,6,8,12,10,15,5,3,0,1,2,11,4,6,
                15,16,5,8,12,9,14,5,2,1,0,1,12,5,5,
                16,17,4,6,12,10,15,3,1,2,1,0,12,6,4,
                7,13,11,15,10,6,10,12,13,11,12,12,0,9,13,
                11,12,10,10,11,6,11,7,7,4,5,6,9,0,9,
                13,16,7,7,11,10,13,6,5,6,5,4,13,9,0),
              nrow = 15, ncol = 15, byrow=TRUE); cek

J <- matrix(1, nrow = 15, ncol = 15); J
A_cek <- -1/2*(cek)^2; A_cek
B_cek <- (diag(15)-1/15*J) %*% A_cek %*% (diag(15)-1/15*J); B_cek
rankMatrix(B_cek)
#Nilai Eigen
eig_val <- eigen(B_cek)$values; eig_val
#Vektor Eigen (normalized)
eig_vec <- eigen(B_cek)$vectors; eig_vec
plot(cbind(t(eig_val[1] %*% eig_vec[,1]), t(eig_val[2] %*% eig_vec[,2])))
text(t(eig_val[1] %*% eig_vec[,1]), t(eig_val[2] %*% eig_vec[,2]),
     labels = c('R1','R2','D1','D2','R3','R4','R5','D3','D4','D5','D6','R6','R7','R8','D8'),
     cex=.7, pos = 3)

# Multidimensional Scaling (METRIC)
fit <- cmdscale(cek,eig=TRUE, k=2) # k is the number of dim
fit
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Metric MDS", pch = 19, col = "cornflowerblue")
text(x, y,
     labels = c('R1','R2','D1','D2','R3','R4','R5','D3','D4','D5','D6','R6','R7','R8','D8'),
     cex=.7, pos = 3)

# Multidimensional Scaling (NONMETRIC)
library(MASS)
fit2 <- isoMDS(cek, k=2) # k is the number of dim
fit2 # view results
# plot solution
x2 <- fit2$points[,1]
y2 <- fit2$points[,2]
plot(x2, y2, xlab="Dimension 1", ylab="Dimension 2",
     main="Nonmetric MDS", pch = 19, col = "turquoise3")
text(x2, y2,
     labels = c('R1','R2','D1','D2','R3','R4','R5','D3','D4','D5','D6','R6','R7','R8','D8'),
     cex=.7, pos = 3)

#### Tugas (https://www.kaggle.com/datasets/uciml/glass) ####
library(readxl)
glass <- read_excel("Downloads/glass.xlsx", 
                    sheet = "Glass")

library(dplyr)
glass <- dist(t(glass.matrix), method = "euclidean") %>% as.matrix(glass)

J <- matrix(1, nrow = 9, ncol = 9); J
A_glass <- -1/2*(glass)^2; A_glass
B_glass <- (diag(9)-1/9*J) %*% A_glass %*% (diag(9)-1/9*J); B_glass
rankMatrix(B_glass)

#Nilai Eigen
eig_val <- eigen(B_glass)$values; eig_val
#Vektor Eigen (normalized)
eig_vec <- eigen(B_glass)$vectors; eig_vec

cbind(t(eig_val[1] %*% eig_vec[,1]), t(eig_val[2] %*% eig_vec[,2]),
      t(eig_val[3] %*% eig_vec[,3]), t(eig_val[4] %*% eig_vec[,4]),
      t(eig_val[5] %*% eig_vec[,5]), t(eig_val[6] %*% eig_vec[,6]),
      t(eig_val[7] %*% eig_vec[,7]), t(eig_val[8] %*% eig_vec[,8]))
plot(cbind(t(eig_val[1] %*% eig_vec[,1]), t(eig_val[2] %*% eig_vec[,2])))
text(t(eig_val[1] %*% eig_vec[,1]), t(eig_val[2] %*% eig_vec[,2]),
     labels = c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe"),
     cex=.7, pos = 3)

# Multidimensional Scaling (METRIC)
library(dplyr)
glass <- dist(t(glass.matrix), method = "euclidean") %>% as.matrix(glass)
fit <- cmdscale(glass,eig=TRUE, k=2); fit
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Metric MDS", pch = 19, col = "cornflowerblue")
text(x, y, colnames(glass), cex=.7, pos = 3)

# Multidimensional Scaling (NONMETRIC)
library(dplyr)
glass <- dist(t(glass.matrix), method = "euclidean") %>% as.matrix(glass)
library(MASS)
fit2 <- isoMDS(glass, k=2); fit2
x2 <- fit2$points[,1]
y2 <- fit2$points[,2]
plot(x2, y2, xlab="Dimension 1", ylab="Dimension 2",
     main="Nonmetric MDS", pch = 19, col = "turquoise3")
text(x2, y2, colnames(glass), cex=.7, pos = 3)

# Plot STRESS
glass <- read_excel("Downloads/glass.xlsx", 
                    sheet = "Glass")
library(goeveg)
dimcheckMDS(
  glass[,1:9],
  distance = "bray",
  k = 6,
  autotransform = TRUE
)
