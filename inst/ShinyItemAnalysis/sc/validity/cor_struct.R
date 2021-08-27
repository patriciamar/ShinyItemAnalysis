library(ggdendro)
library(ggplot2)
library(psych)
library(ShinyItemAnalysis)

# loading data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]

# polychoric correlation matrix
cor(data)
cor(data, method = "spearman")
(corP <- polychoric(data))

# correlation heat map with 3 clusters using Ward method
plot_corr(data, cor = "poly", clust_method = "ward.D2", n_clust = 3)

# dendrogram
hc <- hclust(as.dist(1 - corP$rho), method = "ward.D") # hierarchical clustering
ggdendrogram(hc) # dendrogram
