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
#Entonces: largo= 23.4 + -0.842 x ancho
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




