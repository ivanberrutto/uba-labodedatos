#TEPE
#install.packages("ggplot2")
#install.packages("tidyverse")
require(dplyr)
require(ggplot2)
require(tidyverse)




viajes = clima_ecobici

viajes = viajes %>% 
  mutate( date = as.POSIXlt(viajes$date) )

viajes = viajes %>% mutate(lluvia = ifelse(prcp > 0.6, "Llovio", "No Llovio"))


viajes = mutate(viajes,dia=as.factor(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                       "Friday", "Saturday")[as.POSIXlt(viajes$date)$wday + 1]))

feriados_o_dia_no_laborable <- c("2022-01-01", "2022-02-28", "2022-03-01", "2022-03-24", "2022-04-02", "2022-04-14",
                                 "2022-04-15", "2022-04-16", "2022-04-17", "2022-04-22", "2022-04-23", "2022-04-24",
                                 "2022-05-01", "2022-05-02", "2022-05-18", "2022-05-18", "2022-05-25", "2022-06-17",
                                 "2022-06-20", "2022-07-09", "2022-07-30", "2022-08-15", "2022-09-26", "2022-09-27",
                                 "2022-10-05", "2022-10-07", "2022-10-10", "2022-11-20", "2022-11-21", "2022-12-08",
                                 "2022-12-09", "2022-12-25")
findesemana = c("Saturday","Sunday")

feriados_o_dia_no_laborable_datatime <- as.POSIXlt(feriados_o_dia_no_laborable)

viajes <- viajes %>%
  mutate(dialaborable = ifelse(date %in% feriados_o_dia_no_laborable  , "Dia no laborable", "Dia laborable")) %>% 
  mutate(dialaborable = ifelse(dia %in% findesemana  , "Dia no laborable", "Dia laborable"))



#comentario notamos que los dia que llueve que la gente usa menos la bici ahi pongo un grafo *grafico* 
#los findes y los dias no laborables hay muy poco uso de la bici *grafico*

#para analizar la temperatura del dia agarramos el dato de la temperatura maxima porque nos resulta interesante ver el uso de las bicis con respecto 
viajes %>%
  group_by(date, dialaborable) %>%
  summarise(cantidad_de_viajes = n) %>%
  group_by(dialaborable) %>%
  summarise(promedio = mean(cantidad_de_viajes))%>%
  na.omit() %>%
  ggplot(aes(x = dialaborable, y = promedio)) +
  geom_bar(stat = "identity", fill = c("#52D6CE", "#F27B64")) +
  labs(
    title = "Promedio de Uso dia laborables y no laborable",
    x = "Dia",
    y = "Promedio de Uso de Bicicletas"
  )


viajes %>%
  group_by(date, lluvia) %>%
  summarise(cantidad_de_viajes = n )%>%
  group_by(lluvia) %>%
  summarise(promedio = mean(cantidad_de_viajes))%>%
  na.omit() %>%
  ggplot(aes(x = lluvia, y = promedio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Promedio de Uso de Bicicletas en Días con y sin Lluvia",
    x = "Lluvia",
    y = "Promedio de Uso de Bicicletas"
  )

viajes %>% 
  ggplot(aes(x=pres,y=n))+
  geom_point()+geom_smooth()+labs(
    title = "presion",
    x = "presion",
    y = "n"
  )

#en conclusion 


viajes %>% 
  ggplot(aes(x=wspd,y=n))+
  geom_point()+geom_smooth(se=F)+labs(
    title = "velocidad del viento",
    x = "velocidad",
    y = "n"
  )


viajes %>% 
  ggplot(aes(x=tavg,y=n))+
  geom_point()+geom_smooth(se=F)+labs(
    title = "temperatura ",
    x = "temperatura ",
    y = "n"
  )



viajes %>% filter(prcp<200) %>% 
  ggplot(aes(x=prcp,y=n))+
  geom_point()+geom_smooth(se=F)+labs(
    title = "precipitaciones ",
    x = "precipitacion",
    y = "n"
  )

set.seed(123)  
indice_entrenamiento <- sample(1:nrow(viajes), 0.8 * nrow(viajes))
entrenamiento <- viajes[indice_entrenamiento, ]
prueba <- viajes[-indice_entrenamiento, ]

mimodelo <- lm(n ~ dialaborable + lluvia, data = entrenamiento)
b0 <- coef(mimodelo)[1]
b1 <- coef(mimodelo)[2] #sin esto es dia laborable, sumando esto es dia no laborable
b2 <- coef(mimodelo)[3] #sin esto es dia de lluvia, sumando esto es dia sin lluvia


ggplot(viajes, aes(x = tavg, y = n,color=paste(lluvia,dialaborable))) +
  geom_point() +
  geom_abline(mapping=aes(intercept=b0,slope=1),color="red") +
  geom_abline(mapping=aes(intercept=b0+b2,slope=1),color="blue") +
  geom_abline(mapping=aes(intercept=b0+b1,slope=1),color="green") +
  geom_abline(mapping=aes(intercept=b0+b1+b2,slope=1),color="purple") 


#concluimos que es malardo




modelotavg <- lm(n ~ dialaborable * tavg, data = entrenamiento)
b0tavg <- coef(modelotavg)[1]
b1tavg <- coef(modelotavg)[2] #sin esto es dia laborable, sumando esto es dia no laborable
b2tavg <- coef(modelotavg)[3]

ggplot(viajes, aes(x = tavg, y = n,color=paste(dialaborable))) +
  geom_point() +
  geom_abline(mapping=aes(intercept=b0tavg,slope=b2tavg),color="red") +
  geom_abline(mapping=aes(intercept=b0tavg+b1tavg,slope=b2tavg),color="blue")












