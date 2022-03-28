#pos hoc tablas de contingencia 

#install.packages("RVAideMemoire")
#library(devtools)
#install_github("dustinfife/fifer")
#library(remotes)
#install_version("fifer", "1.0")

library(RVAideMemoire)
library(fifer)

iris$size_sepal <- ifelse(iris$Sepal.Length < median(iris$Sepal.Length),
                          "pequeño", "grande")


tabla_contingencia<-table(iris$Species, iris$size_sepal)
tabla_contingencia

#chi.square
chisq.test(tabla_contingencia)
chisq.post.hoc(tabla_contingencia)


#fisher
fisher.test(tabla_contingencia)
fisher.multcomp(tabla_contingencia)

