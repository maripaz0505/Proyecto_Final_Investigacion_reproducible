# Proyecto_Final_Investigacion_reproducible

---
title: Análisis preliminar de variables en la estandarización de un modelo animal
  de obesidad inducido por dieta
author: "Maripaz Castro-Murillo"
output: 
    prettydoc::html_pretty:
    theme: hpstr
    highlight: github
    toc: true
    number_sections: yes
    
---
# **INTRODUCCIÓN**

#### El aumento en las tasas de obesidad ha aumentado a nivel mundial en los últimos años. Los datos indican que esto podría deberse a los malos estilos de vida de las personas, incluyendo la ingesta de alimentos altos en grasa y carbohidratos, unido a estilos de vida sedentarios.

![Causas de obesidad en el mundo](z0p2A45xbw6wplv6h278kjzcvnnx6l2qf1w18nfqyl5zkm513293frbpfgv1c1pddlgrl7vkcztl28xpr5hlvb7l5hs7qgbp9zccw42qfcxAyv38wwzrj7p6zb4j7hf9300rp744n1618yj4ld8v16s9wtcjbrsAm9cfl9ggt1w2rgdvqc31.jpeg){width=60%, }

#### Desde hace varios años en la Universidad de Costa Rica, específicamente el Instituto de Investigaciones en Salud, el Centro de Investigación en Neurociencias y el Laboratorio de Ensayos Biológicos, han estudiado conductas asociadas con el desarrollo de obesidad, incluyendo alteraciones moleculares que llevan a la adicción a la comida. Para esto se quiere hacer un modelo animal de obesidad inducido por dieta, de forma que nos permita estudiar fenómenos asociados con la obesidad. Este modelo consistiría en ratas las cuales consumirán alimentos como los que conseguimos en Costa Rica, con un alto índice glicémico y un alto contenido de grasas. El grupo control serían animales que consuman alimento convencional para animales de laboratorio, el agua lo tendrán ambos grupos (control y tratamiento).

![Áreas del cerebro involucradas en la activación del sistema de recompensa](Imagen 1.jpg){width=90%}

#### Los animales recibieron los alimentos por 60 días. Al final del experimento se pesaron los animales, y el peso será nuestra medida para saber si los animales expuestos a la dieta CAF aumentaron de peso. Igualmente se midieron otras variables, y nuestro objetivo es identificar las variables que tengan un efecto significativo sobre la variable respuesta, para saber cuáles no son importantes de considerar en futuros modelos. 

#### Nosotros esperaríamos que el grupo tratamiento tenga un mayor peso, y que las variables con efecto aumenten también. 

![Hipótesis sobre el modelo animal de obesidad](Imagen 2.jpg){width=90%}


![Ratas Wistar utilizadas en el modelo animal de obesidad](PHOTO-2019-11-30-17-42-49.jpg){width=60%}

```{r, echo=FALSE, include=F}

setwd("~/Downloads")
```

```{r, echo=FALSE, message=FALSE}

library(knitr)
library(markdown)
library(httpuv)
library(caTools)
library(ggplot2)
library(car)
library(ggplot2)
library(GGally)
library(psych)
library(pscl)
library(MuMIn)
library(lmtest)
library(leaps)
library(olsrr)
library(hier.part)
library(nlme)
library(R.utils)

```


#### Las librerías necesarias son las siguientes:

```{r, eval=FALSE, echo=TRUE}
library(knitr)
library(prettydoc)
library(caTools)
library(ggplot2)
library(car)
library(ggplot2)
library(psych)
library(pscl)
library(MuMIn)
library(lmtest)
library(nlme)

```

#### Cargamos la base de datos con todas las variables, recordando que la variable dependiente es el peso al final de la semana 9 (PESOSEM)

```{r, echo= T}

 obesos <- read.csv("Piloto1.csv", header = T, sep = ";")

```

```{r}

str(obesos)
head(obesos)


```

#### Primero vamos a evaluar los supuestos de toda la base:

# **NORMALIDAD**

```{r}
pairs.panels(obesos, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)   

```

#### Las variables presentan una distribución normal de sus datos.

# **MULTICOLINEALIDAD**

```{r, echo=F, eval=T}

names(obesos)
cor(obesos,  method = c("pearson"))

vif(lm(PESOSEM9 ~ GRUPO + TADIPOSO.REL + COL.mg.dL.+ GLUC..mg.dL. + TG..mg.dL. + ED.BDNF + ED.TrkB + ED.CREB + Nac.CREB + Nac.BDNF + Nac.TrkB, data = obesos))

```
#### Existen correlaciones positivas y negativas entre las variables, la variable respuesta peso está correlacionada positivamente con variables como el tejido adiposo relativo, la expresión de BDNF en el estriado dorsal, la expresión de CRF, TrkB y CREB en la corteza prefrontal, así como en el consumo total de grasa y carbohidratos.

# **RESIDUALES**

```{r, echo=T, eval=T}

obesos$GRUPO <- as.factor(obesos$GRUPO)
obesos.lm <- lm(PESOSEM9 ~ GRUPO + TADIPOSO.REL + COL.mg.dL.+ GLUC..mg.dL. +                     TG..mg.dL. + ED.BDNF + ED.TrkB + ED.CREB + Nac.CREB + 
                    Nac.BDNF + Nac.TrkB, data = obesos)

par(mfrow = c(2, 2), oma = c(0, 0, 0, 0))
plot(obesos.lm, ask = F, which = 1:4)

par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
hist(obesos.lm$residuals)

shapiro.test(obesos.lm$residuals)
```

#### Conclusión: El gráfico Q-Q e histograma muestra que los residuales no desvián de una distribución normal. La prueba SW tampoco muestra que no hay evidencia de que los residuos no sigan una distribución normal. Entonces las pruebas de hipótesis son confiables.

# **SELECCIÓN DEL MODELO**

#### Selección automatizada:

```{r, echo=T, eval=T}

options(na.action = "na.fail")
m <- dredge(obesos.lm, rank="AIC")

```

#### La selección automatizada nos da varias variables que aparecen en casi todos los modelos, vamos a elegir las que sean más constantes en los modelos y a probar combinaciones entre ellas.

```{r, echo=T, eval=T}

m0 <- lm(PESOSEM9 ~ 1, data = obesos)
m.n <- lm(PESOSEM9 ~ GLUC..mg.dL. + GRUPO + Nac.BDNF + Nac.TrkB + TADIPOSO.REL + Nac.CREB, data = obesos)
m.n1 <- lm(PESOSEM9 ~ GLUC..mg.dL., data = obesos)
m.n2 <- lm(PESOSEM9 ~ GLUC..mg.dL. + GRUPO, data = obesos)
m.n3 <- lm(PESOSEM9 ~ GLUC..mg.dL. + GRUPO + Nac.BDNF, data = obesos)
m.n4 <- lm(PESOSEM9 ~ GLUC..mg.dL. + GRUPO + Nac.BDNF + Nac.TrkB, data = obesos)
m.n5 <- lm(PESOSEM9 ~ GLUC..mg.dL. + GRUPO + Nac.BDNF + Nac.TrkB + TADIPOSO.REL, data = obesos)
m.n6 <- lm(PESOSEM9 ~ GLUC..mg.dL. + GRUPO + TADIPOSO.REL, data = obesos)

```

#### Ahora comparamos los AIC de los modelos hechos para elegir el que tenga un AIC menor

```{r, echo=T, eval=T}

AIC(m0, m.n, m.n1, m.n2, m.n3, m.n4, m.n5, m.n6)

anova(m.n, m.n5, test ="Chi")

summary(m.n5)

avPlots(m.n, ask=F)

```


####Estos gráficos nos permiten evaluar los residuales de las variables predictoras.


# **GRÁFICOS DE PREDICCIONES**

```{r, echo=T, eval=T}



newdata <- data.frame(TADIPOSO.REL= seq(from= min(obesos$TADIPOSO.REL), 
                      to= max(obesos$TADIPOSO.REL), 
                      l=1000),
                      GLUC..mg.dL. = mean(obesos$GLUC..mg.dL.),
                      Nac.BDNF= mean(obesos$Nac.BDNF),
                      Nac.TrkB= mean(obesos$Nac.TrkB),
                      Nac.CREB = mean(obesos$Nac.CREB),
                      GRUPO = levels(obesos$GRUPO))

# Objeto con valores predichos por el modelo
newdata2 <- cbind(newdata, predict(m.n5, newdata, type = "response", 
                                   se.fit = TRUE))

fit <- newdata2$fit
LL <- newdata2$fit - 1.96 * newdata2$se.fit
UL <- newdata2$fit + 1.96 * newdata2$se.fit

# Guardar valores predichos en nueva base de datos
newdata3 <- data.frame(newdata2$TADIPOSO.REL,
                       newdata2$GLUC..mg.dL.,
                       newdata2$Nac.BDNF,
                       newdata2$Nac.TrkB,
                       newdata2$Nac.CREB,
                       newdata2$GRUPO,
                       fit, LL, UL)

names(newdata3) <- c("adiposo", "glucosa","Nac_BDNF", 
                     "Nac_TrkB", "Nac_CREB", "grupo",
                     "fit", "LL", "UL")


# Poner datos originales y la predicci?n del modelo en una misma base
#newdata4 <- cbind(obesos, newdata3)
#head(newdata4)

# Grafico 1
p1 <- ggplot(newdata3, aes(x = adiposo, y = fit, fill = grupo)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.30) +
  geom_line(aes(color = grupo), size = 1, lty = 5) +
  labs(x = "Tejido adiposo", y = "Peso") +
  theme_classic() +
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = .7),
        axis.line.y = element_line(color="black", size = .7)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  theme(text = element_text(size=12))

p1


```

```{r, eval=T, echo=T}

newdata <- data.frame(TADIPOSO.REL= mean(obesos$TADIPOSO.REL),
                      GLUC..mg.dL. = seq(min(obesos$GLUC..mg.dL.), 
                                         max(obesos$GLUC..mg.dL.),
                                         l = 1000),
                      Nac.BDNF= mean(obesos$Nac.BDNF),
                      Nac.TrkB= mean(obesos$Nac.TrkB),
                      Nac.CREB = mean(obesos$Nac.CREB),
                      GRUPO = levels(obesos$GRUPO))

# Objeto con valores predichos por el modelo
newdata2 <- cbind(newdata, predict(m.n5, newdata, type = "response", 
                                   se.fit = TRUE))

fit <- newdata2$fit
LL <- newdata2$fit - 1.96 * newdata2$se.fit
UL <- newdata2$fit + 1.96 * newdata2$se.fit

# Guardar valores predichos en nueva base de datos
newdata3 <- data.frame(newdata2$TADIPOSO.REL,
                       newdata2$GLUC..mg.dL.,
                       newdata2$Nac.BDNF,
                       newdata2$Nac.TrkB,
                       newdata2$Nac.CREB,
                       newdata2$GRUPO,
                       fit, LL, UL)

names(newdata3) <- c("adiposo", "glucosa","Nac_BDNF", 
                     "Nac_TrkB", "Nac_CREB", "grupo",
                     "fit", "LL", "UL")


# Poner datos originales y la predicci?n del modelo en una misma base
#newdata4 <- cbind(obesos, newdata3)
#head(newdata4)

# Grafico 1
p1 <- ggplot(newdata3, aes(x = glucosa, y = fit, fill = grupo)) +
    geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.30) +
    geom_line(aes(color = grupo), size = 1, lty = 5) +
    labs(x = "Glucosa", y = "Peso") +
    theme_classic() +
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black", size = .7),
          axis.line.y = element_line(color="black", size = .7)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) +
    theme(text = element_text(size=12))

p1
```

# **CONCLUSIONES**

#### El modelo elegido nos permitió identificar las variables *GLUC..mg.dL. + GRUPO + Nac.BDNF + Nac.TrkB + TADIPOSO.REL + Nac.CREB* como las variables que explicaban el aumento en el peso de los animales al recibir dieta de cafetería. Sin embargo, solo el tejido adiposo en gramos y la concentración de glucosa en sangre tuvieron un efecto significativo sobre la variable respuesta. Es interesante mencionar que la concentración de glucosa en sangre más bien aumentó en animales control, esto fue un indicador para empezar a estudiar la calidad de alimento estándar que reciben los animales de laboratorio. Se realizaron otras pruebas posteriores para lograr identificar que este alimento no era el mejor, y se realizó el cambio a niver institucional.
