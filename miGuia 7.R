require(palmerpenguins)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("tidyverse")
require(dplyr)

pinguinos = penguins %>% na.omit()

#Ejercicio 1:

modelopinguinos <- lm(body_mass_g ~ flipper_length_mm + sex, data = pinguinos)
b0=coef(modelopinguinos)[1]
b1=coef(modelopinguinos)[2]
b2=coef(modelopinguinos)[3]

#error cuadratico medio:
ecm <- mean(modelopinguinos$residuals^2)
ecm

#preddicion (?)
predicciones <- predict(modelopinguinos)


ggplot(pinguinos, aes(x = flipper_length_mm, y = body_mass_g, color = sex)) +
  geom_point() +  # Puntos para los datos reales
   geom_abline(mapping=aes(intercept=b0 + b2,slope=b1),color="blue") +
   geom_abline(mapping=aes(intercept=b0,slope=b1),color="purple") +
  labs(x = "Largo de la Aleta (mm)", y = "Peso") +
  ggtitle("Datos y Predicciones del Modelo de Regresión")

#como el b2 es de 348, significa que predice que tendra una diferencia de peso de 348 gramos


#Ejercicio 2:

modelopinguinos2 <- lm(body_mass_g ~ flipper_length_mm + species, data = pinguinos)
b02=coef(modelopinguinos2)[1]
b12=coef(modelopinguinos2)[2]
b22=coef(modelopinguinos2)[3]
b32=coef(modelopinguinos2)[4]
#la diferencia es que ahora hay 3 factores envedes de 2

#Usa a Adelie como referencia
#b22 mide el peso de diferencia entre Adelie y Chinstrap
#b32 mide el peso de diferencia entre Adelie y Gentoo
#si es negativo significa que la especie comparada es peso menor, si es positivo es peso mayor


ggplot(pinguinos, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +  # Puntos para los datos reales
  geom_abline(mapping=aes(intercept=b02 + b22,slope=b12),color="green") +
  geom_abline(mapping=aes(intercept=b02,slope=b12),color="red") +
  geom_abline(mapping=aes(intercept=b02+b32,slope=b12),color="blue") +
  labs(x = "Largo de la Aleta (mm)", y = "Peso") +
  ggtitle("Comparacion entre las especies")

#Ejercicio 3:
modelopinguinos3 <- lm(body_mass_g ~ flipper_length_mm * species, data = pinguinos)
b0=coef(modelopinguinos3)[1]
b1=coef(modelopinguinos3)[2]
b2=coef(modelopinguinos3)[3]
b3=coef(modelopinguinos3)[4]
b4=coef(modelopinguinos3)[5]
b5=coef(modelopinguinos3)[6]

ggplot(pinguinos, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +  # Puntos para los datos reales
  geom_abline(mapping=aes(intercept=b0 + b2,slope=b1+b4),color="green") +
  geom_abline(mapping=aes(intercept=b0,slope=b1),color="red") +
  geom_abline(mapping=aes(intercept=b0+b3,slope=b1+b5),color="blue") +
  labs(x = "Largo de la Aleta (mm)", y = "Peso") +
  ggtitle("Comparacion entre las especies")


#predice que el chinstrap tendra -500, y el gentoo -4000 creo



#Ejercicio 4:
set.seed(123)  
indice_entrenamiento <- sample(1:nrow(pinguinos), 0.8 * nrow(pinguinos))
entrenamiento <- pinguinos[indice_entrenamiento, ]
prueba <- pinguinos[-indice_entrenamiento, ]

#sobreajuste

# 1 variable predictora
modelo1 <- lm(body_mass_g ~ flipper_length_mm, data = entrenamiento)

#prueba[,"pesopredicho"]=predict(modelo1,newdata=prueba)

#error de prediccion con los datos nuevos
mean((predict(modelo1,newdata=prueba)-prueba$body_mass_g)^2)
#error de prediccion con los mismos datos de entrenamiento
mean(modelo1$residuals ^ 2)

# 2 variables predictoras
modelo2 <- lm(body_mass_g ~ flipper_length_mm + species, data = entrenamiento)
#error de prediccion con los datos nuevos
mean((predict(modelo2,newdata=prueba)-prueba$body_mass_g)^2)
#error de prediccion con los mismos datos de entrenamiento
mean(modelo2$residuals ^ 2)

# 3 variables predictoras
modelo3 <- lm(body_mass_g ~ flipper_length_mm + species + year, data = entrenamiento)
#error de prediccion con los datos nuevos
mean((predict(modelo3,newdata=prueba)-prueba$body_mass_g)^2)
#error de prediccion con los mismos datos de entrenamiento
mean(modelo3$residuals ^ 2)

# 4 variables predictoras
modelo4 <- lm(body_mass_g ~ flipper_length_mm + species + year + bill_length_mm, data = entrenamiento)
#error de prediccion con los datos nuevos
mean((predict(modelo4,newdata=prueba)-prueba$body_mass_g)^2)
#error de prediccion con los mismos datos de entrenamiento
mean(modelo4$residuals ^ 2)

# 5 variables predictoras
modelo5 <- lm(body_mass_g ~ flipper_length_mm + species + year + bill_length_mm + bill_depth_mm, data = entrenamiento)
#error de prediccion con los datos nuevos
mean((predict(modelo5, newdata=prueba)-prueba$body_mass_g)^2)
#error de prediccion con los mismos datos de entrenamiento
mean(modelo5$residuals ^ 2)



# 6 variables predictoras
modelo6 <- lm(body_mass_g ~ flipper_length_mm + species + year + bill_length_mm + bill_depth_mm + sex, data = entrenamiento)
#error de prediccion con los datos nuevos
mean((predict(modelo6,newdata=prueba)-prueba$body_mass_g)^2)
#error de prediccion con los mismos datos de entrenamiento
mean(modelo6$residuals ^ 2)





#no entiendo tanto el calculo de ecm y del predict, lo voy a preguntar asi q lo dejo aca

