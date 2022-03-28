#Clase 1

#muestreos
#muestreo aleatorio con reposición
sample(1:100, 15, replace = TRUE)

#muestreo aleatorio sin reposición
sample(1:100, 15, replace = FALSE)

#iris
set.seed(123)
nrow(iris)
flores_elegidas<- sample(1:150,5,replace=FALSE)
muestra_iris_flores_elegidas <- iris[flores_elegidas,] 
muestra_iris_flores_elegidas

#estratificado
set.seed(234)
bolas_rojas <- sample(1:40, 4, replace = FALSE)
bolas_verdes<- sample(41:100, 4, replace = FALSE)

rbind(bolas_rojas, bolas_verdes)

#sistemático
set.seed(15)
primera_bola <- sample(1:100, 1)
#primera_bolas<- 92
incremento <-7
bolas_elegidas <- seq(from=primera_bola,by=incremento,length.out=5)

#moran
ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv",
                    sep=",",header=T)
head(ozone)

ozone_coords<- cbind(ozone$Lon, ozone$Lat)
ozone_dist <- as.matrix(dist(cbind(ozone$Lon, ozone$Lat)))

ozone_dist_inv <- 1/ozone_dist
diag(ozone_dist_inv) <- 0

#ozone_dist_inv[1:5, 1:5]
library(ape)
Moran.I(ozone$Av8top, ozone_dist_inv)

#hillR

#install.packages("hillR")
# o instala la versión en desarrollo del github
#devtools::install_github("daijiang/hillR")
set.seed(123)
dummy_data <- FD::dummy
comunidades<-  dummy_data$abun
funciones <- dummy_data$trait
arbol <- ape::rtree(n = ncol(comunidades), 
                    tip.label = paste0("sp", 1:ncol(comunidades)))
head(comunidades)
head(funciones)
head(arbol)

library(hillR)
hill_taxa(comunidades, q = 0)
hill_func(comunidades, funciones, q = 0)
hill_phylo(comunidades, arbol, q = 0) 

hill_taxa(comunidades, q = 1)
hill_taxa(comunidades, q = 2)

hill_taxa_parti(comunidades, q = 0)

hill_func_parti(comunidades, funciones, q = 0)

hill_phylo_parti(comunidades, arbol, q = 0)


hill_taxa_parti_pairwise(comunidades, q = 0, show_warning = FALSE, .progress = FALSE)


hill_func_parti_pairwise(comunidades, funciones, q = 0, show_warning = FALSE, .progress =FALSE)

hill_phylo_parti_pairwise(comunidades, arbol, q = 0, show_warning = FALSE, .progress = FALSE) 

#inext
## instalando iNEXT del CRAN
#install.packages("iNEXT")

## instalando la versión de desarrollo
#install.packages('devtools')
#library(devtools)
#install_github('AnneChao/iNEXT')

library(iNEXT)
library(ggplot2)
out <- iNEXT(bird, q=c(0, 1, 2), datatype="abundance", endpoint=500)

ggiNEXT(out, type=1, facet.var="site")
ggiNEXT(out, type=2)
ggiNEXT(out, type=3, facet.var="site")
ggiNEXT(out, type=3, facet.var="order")
estimateD(bird, datatype="abundance", base="coverage",  conf=0.95)
data(ciliates)
#str(ciliates)
out2 <- iNEXT(ciliates, q=c(0,1,2), datatype="incidence_raw")
ggiNEXT(out2, facet.var="order", type=1)

#hilldiv
#versión de CRAN
install.packages("hilldiv")

#versión en desarrollo
#install.packages("devtools")
#library(devtools)
#install_github("anttonalberdi/hilldiv")
library(hilldiv)
data(bat.diet.otutable)
data(bat.diet.tree)
data(bat.diet.hierarchy)
bat.diet.otutable[1:3, 1:4]
class(bat.diet.tree)
head(bat.diet.hierarchy)


hill_div(bat.diet.otutable,0)

hill_div(bat.diet.otutable, 1, bat.diet.tree)

hill_div(to.incidence(bat.diet.otutable,bat.diet.hierarchy),2)

profile.multiplegroups <- div_profile(bat.diet.otutable,hierarchy=bat.diet.hierarchy,level="alpha")
div_profile_plot(profile.multiplegroups)

pareada<-div_test(bat.diet.otutable,qvalue=0,hierarchy=bat.diet.hierarchy,posthoc=TRUE)
div_test_plot(pareada,chart="jitter",posthoc=TRUE,threshold=0.5)

head(depth_cov(bat.diet.otutable,qvalue=1))


#visualización
library(tidyverse)
library(ggpubr)
q0<-hill_div(bat.diet.otutable,0) %>% as.data.frame() %>% mutate(
  qs="q0") %>% rownames_to_column(var = "Sample")
q1<-hill_div(bat.diet.otutable,1)%>% as.data.frame() %>% mutate(
  qs="q1") %>% rownames_to_column(var = "Sample")
q2<-hill_div(bat.diet.otutable,2)%>% as.data.frame() %>% mutate(
  qs="q2") %>% rownames_to_column(var = "Sample")

diversidad_data<- rbind(q0,q1,q2) %>%  full_join(bat.diet.hierarchy)
colnames(diversidad_data)[2]<- "value"

ggboxplot(data = diversidad_data, x = "Species", y = "value", 
          fill = "Species", facet.by = "qs")+theme(
            axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggbarplot(data = diversidad_data, x = "Species", y = "value", 
          fill = "Species", facet.by = "qs", add = "mean_sd")+theme(
            axis.text.x = element_blank(), axis.ticks.x = element_blank())