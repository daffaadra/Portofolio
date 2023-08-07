library(readxl)
df <- read_excel("Downloads/Body Measurements _ original_CSV.xlsx")
df <- na.omit(df)

df1 <- df[df$Gender==1, axis = 1]
df2 <- df[df$Gender==2, axis = 1]

# Cut data to be 100 only for each
df1 <- df1[1:100, axis = 1]
df2 <- df2[1:100, axis = 1]

library("writexl")
write_xlsx(df1, "Downloads/df1.xlsx")
write_xlsx(df2, "Downloads/df2.xlsx")

y1_bar <- matrix(colMeans(df1[,-c(1)]), nrow=12, ncol=1, byrow=TRUE)
y2_bar <- matrix(colMeans(df2[,-c(1)]), nrow=12, ncol=1, byrow=TRUE)
y <- matrix(colMeans(df[,-c(1)]), nrow=12, ncol=1, byrow=TRUE)

S_pl <- ((100-1)*cov(df1[,-c(1)])+(100-1)*cov(df2[,-c(1)]))/(100+100-2)
S_pl <- matrix(S_pl, nrow = 12, ncol = 12, byrow = TRUE)
library('pracma')
inv(S_pl)

packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)
library(matlib)

a <- inv(S_pl) %*% (y1_bar-y2_bar)
z1 <- t(a) %*% y1_bar
z2 <- t(a) %*% y2_bar
z <- (z1+z2)/2

getOption("max.print")
options(max.print=1000000)
df1$z <- ((0.0526345184)*df1$Age + (-0.0436652354)*df1$HeadCircumference + (-0.0100244417)*df1$ShoulderWidth
          + (-0.0119005969)*df1$ChestWidth + (-0.0158785775)*df1$Belly + (-0.0221451197)*df1$Waist
          + (-0.0768116232)*df1$Hips + (-0.0179591931)*df1$ArmLength + (0.0324414300)*df1$ShoulderToWaist
          + (0.0603763058)*df1$WaistToKnee + (0.0133362694)*df1$LegLength + (0.0002792248)*df1$TotalHeight)
df1$classification <- ifelse(df1$z > -0.3424297, "G1", "G2")
sum(df1$classification == 'G1')
sum(df1$classification == 'G2')

df2$z <- ((0.0526345184)*df2$Age + (-0.0436652354)*df2$HeadCircumference + (-0.0100244417)*df2$ShoulderWidth
          + (-0.0119005969)*df2$ChestWidth + (-0.0158785775)*df2$Belly + (-0.0221451197)*df2$Waist
          + (-0.0768116232)*df2$Hips + (-0.0179591931)*df2$ArmLength + (0.0324414300)*df2$ShoulderToWaist
          + (0.0603763058)*df2$WaistToKnee + (0.0133362694)*df2$LegLength + (0.0002792248)*df2$TotalHeight)
df2$classification <- ifelse(df2$z > -0.3424297, "G1", "G2")
sum(df2$classification == 'G1')
sum(df2$classification == 'G2')

# Classification into several groups
L1_y <- t(y1_bar)%*%inv(S_pl)%*%y - t(y1_bar)%*%inv(S_pl)%*%y1_bar/2
L2_y <- t(y2_bar)%*%inv(S_pl)%*%y - t(y2_bar)%*%inv(S_pl)%*%y2_bar/2

fungsi1 <- t(y1_bar)%*%inv(S_pl)
fungsi2 <- t(y2_bar)%*%inv(S_pl)

df1$classification <- (df1$Age*fungsi1[1] + df1$HeadCircumference*fungsi1[2] + df1$ShoulderWidth*fungsi1[3]
                       + df1$ChestWidth*fungsi1[4] + df1$Belly*fungsi1[5] + df1$Waist*fungsi1[6] + df1$Hips*fungsi1[7]
                       + df1$ArmLength*fungsi1[8] + df1$ShoulderToWaist*fungsi1[9] + df1$WaistToKnee*fungsi1[10]
                       + df1$LegLength*fungsi1[11] + df1$TotalHeight*fungsi1[12] - 16.70681)

df2$classification <- (df2$Age*fungsi2[1] + df2$HeadCircumference*fungsi2[2] + df2$ShoulderWidth*fungsi2[3]
                       + df2$ChestWidth*fungsi2[4] + df2$Belly*fungsi2[5] + df2$Waist*fungsi2[6] + df2$Hips*fungsi2[7]
                       + df2$ArmLength*fungsi2[8] + df2$ShoulderToWaist*fungsi2[9] + df2$WaistToKnee*fungsi2[10]
                       + df2$LegLength*fungsi2[11] + df2$TotalHeight*fungsi2[12] - 17.04733)

compare <- cbind(df1$classification, df2$classification)

# Using Decision Tree
library(party)
set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
saved_ind <- ind
train_data <- df[saved_ind == 1,]
test_data <- df[saved_ind == 2,]

df_ctree <- ctree(Gender ~ ., data = train_data)
table(predict(df_ctree), train_data$Gender)
plot(df_ctree)