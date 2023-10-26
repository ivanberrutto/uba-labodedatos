#Guía 6

#install.packages("palmerpenguins")
require(palmerpenguins)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("tidyverse")
require(dplyr)


#1.1 Cargar la librería palmerpenguins. Usando el dataset penguins y borrando las observaciones que tengan algún NA responder estas preguntas.
pinguinos = penguins %>% na.omit()
#1.2 Realizar un gráfico de dispersión que muestre la relación entre el ancho y el largo del pico de los pinguinos de la especie Adelie (columnas bill_depth_mm y bill_length_mm).
ggplot(penguins %>% na.omit() %>% filter(species=="Adelie"),aes(x= bill_depth_mm,y=bill_length_mm))+
  geom_point()


#1.3 Escribir la ecuación del modelo de regresión lineal simple que tenga como variable respuesta el largo del pico y como explicativa al ancho (usar lm() para calcular los coeficientes del modelo)
modeloadelie <- lm(bill_length_mm ~ bill_depth_mm, data=pinguinos[pinguinos$species == "Adelie", ])
#modeloadelie <- pinguinos %>% filter(species == "Adelie") %>%  lm(bill_length_mm ~ bill_depth_mm, data = .)
b0adelie=coef(modeloadelie)[1]
b1adelie=coef(modeloadelie)[2]
#Entonces: largo= 23.36682 + 0.8424775 x ancho
#1.4 ¿Qué unidades tienen la ordenada al origen y la pendiente? ¿Cómo se interpretan los valores estimados de la ordenada al origen y la pendiente?

#55.067 seria la ordenada al origen y -0.649 la pendiente 

#1.5 ¿Cuál es el error cuadrático medio del modelo? ¿Cuál es el coeficiente de determinación (r^2)? Programar una función que calcule el error cuadrático medio y r^2

#error cuadratico medio:
ecm = mean(modeloadelie$residuals ^ 2)
#coeficiente de determinacion r2:

resumen <- summary(modeloadelie)
resumen$r.squared


#grafico q estaba en la explicacion, recta de regresión: ( nada que ver )
ggplot(na.omit(penguins),aes(x= flipper_length_mm,y=body_mass_g))+
  geom_point()+
  #Agregar la linea de regresion
  geom_abline(intercept=beta0, slope=beta1, color= "red")+
  ggtitle("Recta de regresion lineal")+ geom_smooth(method="lm", se = F) +
  theme_minimal()

#1.6 Suponga que se encuentra un pinguino de la especie Adelie que tiene un pico de 2 cm de ancho. El dato del largo del pico se perdió. Usando el modelo lineal simple, ¿qué valor de largo de pico tendría ese pinguino? Si se encuentra un pinguino bebé con un pico de 5mm de ancho, ¿sería adecuado usar este modelo para conocer el largo del pico dado su ancho?

#predigo el largo con ancho 2:
b0adelie + (b1adelie * 2)
#predigo el largo del pinguino bebe con ancho 0.5:
b0adelie + (b1adelie * 0.5)

#Usar este sistema para pinguinos adultos en un pinguino bebe podria no ser adecuado


#1.7 Repetir 1.3 para los pinguinos de las otras 3 especies.
modelogentoo <- pinguinos %>% filter(species == "Gentoo") %>%  lm(bill_length_mm ~ bill_depth_mm, data = .)
b0gentoo=coef(modelogentoo)[1]
b1gentoo=coef(modelogentoo)[2]

modelochin <- pinguinos %>% filter(species == "Chinstrap") %>%  lm(bill_length_mm ~ bill_depth_mm, data = .)
b0chin=coef(modelochin)[1]
b1chin=coef(modelochin)[2]

#1.8 Reproducir el gráfico que se muestra abajo usando geom_smooth(method="lm", se = F) y luego, “a mano”, usando los resultados de 1.7 y geom_abline().

ggplot(pinguinos, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE, aes(group = species))

#la verdad se lo pedi a chatgpt porque no queria usar tiempo en esto


#1.9 Sólo para los pinguinos de la especie Adelie, definir una nueva variable que sea el ancho del pico centrado respecto al ancho promedio. 

promedio_ancho_adelie <- pinguinos %>%
  filter(species == "Adelie") %>%
  summarise(promedio_ancho_adelie = mean(bill_depth_mm)) %>%
  pull(promedio_ancho_adelie)

pinguinos <- pinguinos %>% 
  filter(species == "Adelie") %>% 
  mutate(ancho.cen = bill_depth_mm - promedio_ancho_adelie)

modeloadelie2 <- pinguinos %>% filter(species == "Adelie") %>%  lm(bill_length_mm ~ ancho.cen, data = .)
b0adelie2=coef(modeloadelie2)[1]
b1adelie2=coef(modeloadelie2)[2]
#largo= 38.82397 + 0.8424775 * ancho.cen

#el b1 me quedo igual pero el b0 distinto

#coeficiente de determinacion de adelie2:
resumen2 <- summary(modeloadelie2)
resumen2$r.squared

#1.11 Calcular el coeficiente de determinación para este nuevo modelo. ¿Es igual o diferente al calculado en 1.5? Explicar.
#me quedo el mismo, desconozco la explicacion




#2
#2.1 Utilizando la librería mtcars incluida en R-base, que contiene datos sobre automóviles, crear un gráfico para visualizar la relación entre la potencia del motor (columna hp) y la eficiencia en millas por galón (columna mpg). ¿Qué patrón se observa?
  
#  2.2 Realiza una regresión lineal simple para predecir la eficiencia en millas por galón en función de la potencia del motor, ¿Cuál es el valor del coeficiente de determinación (
#  )?
  
#  2.3 Discutir si parece adecuado un modelo lineal para describir esta relación.
data(mtcars)
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  labs(x = "Potencia del Motor (hp)", y = "Millas por Galón (mpg)") +
  ggtitle("Relación entre Potencia del Motor y Eficiencia en MPG")

modelomt <- lm(mpg ~ hp, data = mtcars)
# coeficiente de determinacion
r_cuadrado <- summary(modelomt)$r.squared
r_cuadrado
#hay mucha variabilidad, asi que no pareceria bueno

#3.1 Cargar el conjunto de datos iris, incluida en R-base, que contiene información sobre especies de flores y sus características. Intenta realizar una regresión lineal simple para predecir la longitud del sépalo (columna Sepal.Length) en función del ancho del sépalo (columna Sepal.Width). ¿Cuál es el valor del coeficiente de determinación ()?
data(iris)
modeloiris <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#coeficiente de determinacion r2:
r_cuadrado <- summary(modeloiris)$r.squared
r_cuadrado
#osea que no tiene variabilidad  :D

#4.1 Cargar la librería gapminder. Seleccionar datos de un año particular y realizar un gráfico de dispersión que muestre la relación entre el PIB per cápita (columna gdpPercap) y la esperanza de vida (columna lifeExp).
# Cargar la librería gapminder
library(gapminder)

datos1997 <- gapminder[gapminder$year == 1997 & gapminder$continent=="Americas" , ]
ggplot(datos1997, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(x = "PIB per Cápita", y = "Esperanza de Vida") +
  ggtitle("Relación entre PIB per Cápita y Esperanza de Vida (Año 1997)")
#4.2 Realizar una regresión lineal simple para predecir la esperanza de vida en función del PBI per cápita para 1997 en el continente americano.
modelogap= lm(lifeExp ~ gdpPercap, data = datos1997)
#coeficiente de determinacion r2:
r_cuadrado <- summary(modelogap)$r.squared
r_cuadrado

#4.3 Discutir si el modelo es adecuado para describir esta relación.
#no parece tan adecuado
#4.4 Calcular el error estándar de la estimación (SEE) para evaluar la precisión del modelo de regresión.
residuos <- residuals(modelogap)
n <- length(residuos)
k <- length(coef(modelogap)) - 1
# Calcular el SEE
SEE <- sqrt(sum(residuos^2) / (n - k - 1))
SEE



#4.5 Repetir 4.2 para un modelo pero utilizando como variable respuesta el logaritmo de la esperanza de vida y como variable explicativa el logaritmo del PBI per capita. Discutir la conveniencia de usar el logaritmo de las variables.

modelogaplog= lm(log(lifeExp) ~ log(gdpPercap), data = datos1997)









