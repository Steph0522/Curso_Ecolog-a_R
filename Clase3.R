#clase 3 - Análisis Multivariado

library(vegan)
data("varespec")
head(varespec)

#distancias
eucl_dist<- dist(varespec, method = "euclidean")
eucl_dist

eucl_dist<- vegdist(varespec, method = "euclidean")
eucl_dist

#clusster
clusters<-hclust(eucl_dist, method = "complete")
plot(clusters)

library("ape")
plot(as.phylo(clusters), cex = 0.6)
plot(as.phylo(clusters), cex = 0.6, type = "unrooted")
plot(as.phylo(clusters), type = "fan")

#matriz cofenética
mat.clusters<- cophenetic(clusters)
cor(mat.clusters, eucl_dist, method = "pearson")

#PCA
data("dune")
data("dune.env")
dune_pca <- rda(dune)
sum_dune_pca <- summary(dune_pca)
head(sum_dune_pca)
biplot(dune_pca)
ordihull(dune_pca, groups = dune.env$Management, col = c("red", "green", "blue", "black"))
man_names<- levels(dune.env$Management)
legend("topright",col = c("red", "green", "blue", "black"), lty = 1, legend = man_names )

#PCoA
d <- vegdist(dune, method = "jaccard")
ord <- wcmdscale(d, eig = TRUE)
ordiplot(ord, display = 'sites', type = 'text')
barplot (ord$eig, las = 3, ylab = 'eigenvalues')

#NMDS
nmds <- metaMDS(dune, distance = "bray", k = 2)
plot(nmds)
ordiplot(nmds,type="n") 
orditorp(nmds,display="species",col="red",air=0.01) 
orditorp(nmds,display="sites",cex=1.25,air=0.01)
ordihull(nmds, groups = dune.env$Management, col = c("red", "green", "blue", "black"))

head(scores(nmds, display = "species"))
head(scores(nmds, display = "sites"))


#CCA
data("varespec") #especies
data("varechem") #fisicoquímicos
vares_cca <- cca(varespec ~ N+P+K+Ca+Mg+S+Al+Fe+Mn+Zn+Mo+Baresoil+Humdepth+pH , data=varechem)
summary(vares_cca)

plot(vares_cca)
envfit(vares_cca  ~ N+P+K+Ca+Mg+S+Al+Fe+Mn+Zn+Mo+Baresoil+Humdepth+pH ,
       data=varechem )

#perMANOVA
dune_perm <- adonis(dune ~ Management+Use+Moisture, data = dune.env, method = "euclidean")
dune_perm2 <- adonis(dist(dune) ~ Management+Use+Moisture, data = dune.env)

dune_perm;dune_perm2

