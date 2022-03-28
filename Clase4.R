#Clase 4
#curva acumulación de especies
library(vegan)
data("BCI")
head(BCI)[1:4,1:4]

#por sitios
sac <- specaccum(BCI)
plot(sac, ci.type="polygon") #ver vegan para opciones

#por individuos
sac <- specaccum(BCI, method = "rarefaction")
plot(sac, xvar = "individual", ci.type="polygon") 

#Rarefacción
Srar <- rarefy(BCI, min(rowSums(BCI)))
Srar
#para un número establecido
Srar <- rarefy(BCI, 1000)
Srar

#rango-abundancia
#install.packages("BiodiversityR")
library(BiodiversityR)
#usando BiodiversityR
RkAb <- rankabundance(BCI)
head(RkAb) #especies ordenadas según su abundancia
#gráfica de rango-abundancia
rankabunplot(RkAb, scale='abundance', addit=FALSE, specnames=c(1))

#betadiversidad
#install.packages("betapart")
#install.packages("vegan")
library(vegan)
library(betapart)
comm<- data.frame(comm =1:6,
                  sp1=c(2,2,3,0,0,1),
                  sp2=c(2,2,0,1,1,2),
                  sp3=c(1,0,1,2,3,2),
                  sp4=c(1,0,1,0,2,0),
                  sp5=c(1,2,0,0,0,1),
                  sp6=c(2,2,1,0,0,0),
                  sp7=c(0,0,0,1,0,1),
                  sp8=c(1,0,1,0,1,0), row.names = 1)


groups <- factor(c(rep(1,3), rep(2,3)), 
                 labels = c("noperturbado","perturbado"))

presabs<-ifelse(comm>0,1,0)

dist<-beta.pair(presabs, index.family="jaccard")

library(vegan)
bd<-betadisper(dist[[3]],groups)

plot(bd)

dist.multi<-beta.multi(presabs,index.family ="jaccard" )

#hillR
library(hillR)
set.seed(123)
dummy_data <- FD::dummy
comunidades<-  dummy_data$abun
funciones <- dummy_data$trait
arbol <- ape::rtree(n = ncol(comunidades), 
                    tip.label = paste0("sp", 1:ncol(comunidades)))

beta_q0<-hill_taxa_parti_pairwise(comunidades, q = 0, show_warning = FALSE, .progress = FALSE,
                                  pairs = "full")
beta_q0_mat<-hill_taxa_parti_pairwise(comunidades, q = 0, show_warning = FALSE, 
                                      .progress = FALSE, output = "matrix", pairs = "full")

#phyloseq
library(phyloseq)
data("GlobalPatterns")
gp.ch = subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")
rarecurve(t(otu_table(gp.ch)), step=50, cex=0.5)
plot_bar(gp.ch, "SampleType", fill="Genus", facet_grid=~Family)
plot_richness(gp.ch, x="SampleType", measures=c("Observed", "Shannon")) + geom_boxplot()
library(tidyverse)
wunifrac_dist = phyloseq::distance(gp.ch, method="unifrac", weighted=F) %>% replace_na(0)
ordination = ordinate(gp.ch, method="PCoA", distance=wunifrac_dist)
plot_ordination(gp.ch, ordination, color="SampleType") + theme(aspect.ratio=1)