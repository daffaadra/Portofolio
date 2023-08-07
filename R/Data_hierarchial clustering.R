library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

#Merapikan data karena kolom pertama hanya indeks bukan data
df <- Data_Tugas_4
df1 <- df[col_types = c("skip", "text", "numeric", 
                         "numeric", "numeric", "numeric", 
                         "numeric", "numeric")]
df <- df1[,-1]
row.names(df) <- df1$Provinsi
zdf <- scale(df)
df<- zdf
head(df)

# Dissimilarity matrix
d <- dist(df, method = "euclidean")
d

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}
map_dbl(m, ac)

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )
hc5

# Cut tree into 3 groups
sub_grp <- cutree(hc5, k = 2)

# Number of members in each cluster
table(sub_grp)

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 2, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))

# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

plot(dend1)
tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)
#elbow graph
fviz_nbclust(df, FUN = hcut, method = "wss")

#average silhouette
fviz_nbclust(df, FUN = hcut, method = "silhouette")

#gap statistic
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
