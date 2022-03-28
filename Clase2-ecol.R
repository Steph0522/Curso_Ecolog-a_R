#estadísticos descriptivos
data("iris")
str(iris)

#probando normalidad
qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width)
shapiro.test(iris$Sepal.Width)
hist(iris$Sepal.Width)

modelo <- lm(Sepal.Width ~ Species, data = iris) 
plot(modelo, which = 2)

#probando homocedasticidad
bartlett.test(iris$Sepal.Width, iris$Species)

plot(modelo, which = 1)
aggregate(Sepal.Width ~Species, data = iris, FUN = var) 

#tablas de contingencia
iris$size_sepal <- ifelse(iris$Sepal.Length < median(iris$Sepal.Length),  "pequeño", "grande")

ggplot(iris) +
  aes(x = Species, fill = size_sepal) +
  geom_bar()

tabla_contingencia<-table(iris$Species, iris$size_sepal)
tabla_contingencia

chisq.test(tabla_contingencia)
fisher.test(tabla_contingencia)

## Análisis 1 variable cuantitativa y 2 grupos independientes
library(dplyr)
iris_dos<- iris %>% filter(!Species == "versicolor")
unique(iris_dos$Species)
t.test(iris_dos$Sepal.Width ~ iris_dos$Species)
wilcox.test(iris_dos$Sepal.Length ~ iris_dos$Species)

## Análisis 1 variable cuantitativa y 2 grupos dependientes
#individuos
ratones<- paste0("raton_", 1:10)
#Peso antes 
pa<-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#peso después
pd<-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

data_ratones<- data.frame(ratones=ratones, peso_antes=pa, peso_despues=pd)

t.test(data_ratones$peso_antes, data_ratones$peso_despues, paired=TRUE)
wilcox.test(data_ratones$peso_antes, data_ratones$peso_despues, paired = TRUE)

## Análisis 1 variable cuantitativa y 2 o más grupos independientes:
modelo<- lm(data = iris, Sepal.Width ~ Species)
anova_modelo<- aov(modelo)
summary(anova_modelo)

kruskal.test(iris$Petal.Length, iris$Species)

## Análisis 1 varible cuantitva y 2 o más grupos dependientes
valoracion <- c( 9, 5, 2, 6, 3, 1, 5, 5, 5, 11, 5, 1, 8, 4, 3, 10, 4, 1, 7, 3, 4 )
hora <- factor( rep( c( "mañana", "tarde", "noche" ), 7 ) )
sujeto <- factor( rep( 1:7, each = 3 ) )
datos <- data.frame( valoracion, hora, sujeto )
head(datos)
by(data = datos$valoracion, INDICES = datos$hora, FUN = median)

friedman.test(valoracion, hora, sujeto)
anova_bloques<- aov(lm(valoracion ~ hora+sujeto))
summary(anova_bloques)

## Análisis de medidas repetidas
individuos<- factor(c(rep(1,5), rep(2,5), rep(3, 5), rep(4, 5), rep(5,5), rep(6,5)))
tiempo<- factor(rep(1:5, 6))
rendimiento<- c(8.5, 8.2,8.9, 7.7, 7.4,
                9.8,8.9,8.9,8.8,8.1,
                9.6,9.0, 9.3, 7.5, 7.1,
                7.5, 7.8, 7.8, 4.5, 4.6,
                5.8, 5.8, 5.9, 2.6, 1.2,
                9.9, 9.8, 9.6, 8.6, 8.7)
data_rendimiento<- data.frame(individuos=individuos, tiempo=tiempo, rendimiento=rendimiento)
str(data_rendimiento)

#install.packages("ez")
library(ez)
ezANOVA(data=data_rendimiento, dv=rendimiento, wid=individuos, within=tiempo)
boxplot(rendimiento~tiempo, xlab="tiempo", 
        ylab="Rendimiento académico", 
        main="rendimiento alumnos con el paso del tiempo",
        col="blue", data=data_rendimiento)


#install.packages("jmv")
library(tidyverse)
library(jmv)
#cambiamos el formato de la data
data_rendimiento_notidy<-data_rendimiento %>% mutate(tiempo =case_when(
  tiempo== 1 ~ "T1",
  tiempo== 2 ~ "T2",
  tiempo== 3 ~ "T3",
  tiempo== 4 ~ "T4",
  tiempo== 5 ~ "T5")) %>% pivot_wider(names_from =tiempo, values_from = rendimiento )

jmv::anovaRMNP(data_rendimiento_notidy, measures=vars(T1,T2,T3,T4,T5))

## Correlación lineal simple
cor(iris$Petal.Length, iris$Petal.Width)
cor(iris$Petal.Length, iris$Petal.Width, method = "spearman")

## Regresión lineal simple lm y glm
modelo_lm <- lm(Petal.Width ~ Petal.Length, data = iris) 
summary(modelo)

modelo_glm <- glm(Petal.Width ~ Petal.Length, data = iris) 
summary(modelo)

library(ggplot2)

ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(modelo_lm)$adj.r.squared, 5),
                     " P =",signif(summary(modelo_lm)$coef[2,4], 5)))

#modelos lineales mixtos
library(readr)
rabbit<- read_tsv("https://raw.githubusercontent.com/Steph0522/Curso_R_basico/main/Data/rabbit.tsv")
head(rabbit)

library(lme4)
library(nlme)
lme.rabbit1 <- lmer(gain~ treat +(1|block), data=rabbit)
lme.rabbit2 <- lme(gain~ treat, random = ~1|block, data=rabbit)

anova(lme.rabbit1)
anova(lme.rabbit2)

#lme.rabbit1 <- glmer(gain~ treat +(1|block), data=rabbit, family = "poisson")

#transformando datos
data("pressure")
str(pressure)
cor(pressure$temperature, pressure$pressure)
model<- lm(pressure ~ temperature, data = pressure)
summary(model)
shapiro.test(pressure$pressure)
plot(pressure$pressure, pressure$temperature)

library(dplyr)
pressure_log<- pressure %>% mutate(pressure_log=log(pressure))
model_log<-lm(pressure_log~ temperature, data=pressure_log)
summary(model_log)

plot(pressure_log$pressure_log, pressure_log$temperature)

ggplot(pressure_log, aes(x = pressure_log, y = temperature)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")