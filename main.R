################################################################################
# Importacion de base de datos 
################################################################################

setwd("C:/Users/SU_USUARIO/Desktop/LUGAR_DE_TRABAJO")
covid19 <- read.csv("201104COVID19MEXICO.csv")
covid19 <- read.csv("201104COVID19MEXICO.csv")
### Se seleccionan unicamente los pacientes positivos a SARS-COV 2           ###
indice <- c(covid19$RESULTADO_LAB) == 1
covid19 <- covid19[indice,]
# Comprobacion de filtracion
levels(as.factor(covid19$RESULTADO_LAB)) 
### Se eliminan las columnas que carecen de importancia para el estudio      ###
covid19[,c(1,3,7,17,18,19,20,22,24,26,27,29,31,32,33,34,35,36,37,38)] <- NULL
### Se busca eliminar los residentes extranjeros                             ###
# No se encontraron residentes extranjeros...
levels(as.factor(covid19$ENTIDAD_RES))
### Se crea una nueva columna con los datos de defunciones (Si/No)           ###
DEFUNCIONES <- c(NA)
covid19 <- cbind(covid19,DEFUNCIONES)
# Muerte
covid19[covid19$FECHA_DEF != "9999-99-99", 19] <- 1 
# Sobrevivientes
covid19[covid19$FECHA_DEF == "9999-99-99",19] <- 2 
# Comprobacion de filtracion
levels(as.factor(covid19$DEFUNCIONES)) 
### Se crea una nueva columna con el numero de dias desde                    ###
### que se presentan los SINTOMAS al dia de FALLECIMIENTO (si ocurre)        ###
DIAS_SIN <- c(NA)
covid19 <- cbind(covid19,DIAS_SIN)
inicio <- as.Date(as.character(covid19$FECHA_SINTOMAS), format="%Y-%m-%d")
final <- as.Date(as.character(covid19$FECHA_DEF), format="%Y-%m-%d")
covid19[, 20] <- final - inicio
### Se crea una nueva columna con el numero de dias desde                    ###
### que se INGRESA a la UM al dia de FALLECIMIENTO (si ocurre)               ###
DIAS_ING <- c(NA)
covid19 <- cbind(covid19,DIAS_ING)
inicio <- as.Date(as.character(covid19$FECHA_INGRESO), format="%Y-%m-%d")
final <- as.Date(as.character(covid19$FECHA_DEF), format="%Y-%m-%d")
covid19[, 21] <- final - inicio
### Limpieza                                                                 ###
rm(DEFUNCIONES)
rm(DIAS_ING)
rm(DIAS_SIN)
rm(final)
rm(indice)
rm(inicio)
################################################################################
# Se crea una tabla con los datos por edades de la cantidad de personas
# positivas a SARS-COV-2 que fallecieron. 
# Las columnas muestran si el fallecimiento iba acompanado de otra enfermedad,
# ademas de los dias desde la aparicion de sintomas al fallecimiento, o bien, 
# desde el ingreso a la Unidad Medica al dia de fallecimiento.
################################################################################

levels(as.factor(covid19$EDAD)) #Se observo que habia edades saltadas
EDAD <- c(0:100) #Se creo un vector completo
DIABETES <- NA
sarscov2 <- cbind(EDAD,DIABETES)
for (i in 0:100) {
  sarscov2[i+1,2] <- nrow(covid19[(covid19$EDAD == i & 
                                     covid19$DIABETES == 1 & covid19$DEFUNCIONES == 1),])
}
ASMA <- NA
sarscov2 <- cbind(sarscov2,ASMA)
for (i in 0:100) {
  sarscov2[i+1,3] <- nrow(covid19[(covid19$EDAD == i & 
                                     covid19$ASMA == 1 & covid19$DEFUNCIONES == 1),])
}
HIPERTENSION <- NA
sarscov2 <- cbind(sarscov2,HIPERTENSION)
for (i in 0:100) {
  sarscov2[i+1,4] <- nrow(covid19[(covid19$EDAD == i & 
                                     covid19$HIPERTENSION == 1 & covid19$DEFUNCIONES == 1),])
}
OBESIDAD <- NA
sarscov2 <- cbind(sarscov2,OBESIDAD)
for (i in 0:100) {
  sarscov2[i+1,5] <- nrow(covid19[(covid19$EDAD == i & 
                                     covid19$OBESIDAD == 1 & covid19$DEFUNCIONES == 1),])
}
TABAQUISMO <- NA
sarscov2 <- cbind(sarscov2,TABAQUISMO)
for (i in 0:100) {
  sarscov2[i+1,6] <- nrow(covid19[(covid19$EDAD == i & 
                                     covid19$TABAQUISMO == 1 & covid19$DEFUNCIONES == 1),])
}
GENERAL <- NA
sarscov2 <- cbind(sarscov2,GENERAL)
for (i in 0:100) {
  sarscov2[i+1,7] <- nrow(covid19[(covid19$EDAD == i & 
                                     covid19$DEFUNCIONES == 1),])
}
DIAS_SINTOMAS <- NA
sarscov2 <- cbind(sarscov2,DIAS_SINTOMAS)
for (i in 0:100) {
  sarscov2[i+1,8] <- mean(covid19$DIAS_SIN[covid19$EDAD == i &
                                             covid19$DEFUNCIONES == 1], na.rm = TRUE)
}
DIAS_INGRESO <- NA
sarscov2 <- cbind(sarscov2,DIAS_INGRESO)
for (i in 0:100) {
  sarscov2[i+1,9] <- mean(covid19$DIAS_ING[covid19$EDAD == i & 
                                             covid19$DEFUNCIONES == 1], na.rm = TRUE)
}
sarscov2 <- as.data.frame(sarscov2)
### Limpieza
rm(ASMA)
rm(DIABETES)
rm(DIAS_INGRESO)
rm(DIAS_SINTOMAS)
rm(EDAD)
rm(GENERAL)
rm(HIPERTENSION)
rm(i)
rm(OBESIDAD)
rm(TABAQUISMO)
################################################################################
# Se crea una tabla con los datos por edades de la tasa de mortalidad de las
# personas que padecian de otra enfermedad y en general,
# ademas de los dias desde la aparicion de sintomas al fallecimiento, o bien, 
# desde el ingreso a la Unidad Medica al dia de fallecimiento.
################################################################################

levels(as.factor(covid19$EDAD)) #Se observo que habia edades saltadas
EDAD <- c(0:100) #Se creo un vector completo
DIABETES <- NA
sarscov2 <- cbind(EDAD,DIABETES)
for (i in 0:100) {
  muertos <- nrow(covid19[(covid19$EDAD == i & 
                             covid19$DIABETES == 1 & covid19$DEFUNCIONES == 1),])
  sobrevivientes <- nrow(covid19[(covid19$EDAD == i & 
                                    covid19$DIABETES == 1 & covid19$DEFUNCIONES == 2),])
  sarscov2[i+1,2] <- muertos/(muertos + sobrevivientes)
}
ASMA <- NA
sarscov2 <- cbind(sarscov2,ASMA)
for (i in 0:100) {
  muertos <- nrow(covid19[(covid19$EDAD == i & 
                             covid19$ASMA == 1 & covid19$DEFUNCIONES == 1),])
  sobrevivientes <- nrow(covid19[(covid19$EDAD == i & 
                                    covid19$ASMA == 1 & covid19$DEFUNCIONES == 2),])
  sarscov2[i+1,3] <- muertos/(muertos + sobrevivientes)
}
HIPERTENSION <- NA
sarscov2 <- cbind(sarscov2,HIPERTENSION)
for (i in 0:100) {
  muertos <- nrow(covid19[(covid19$EDAD == i & 
                             covid19$HIPERTENSION == 1 & covid19$DEFUNCIONES == 1),])
  sobrevivientes <- nrow(covid19[(covid19$EDAD == i & 
                                    covid19$HIPERTENSION == 1 & covid19$DEFUNCIONES == 2),])
  sarscov2[i+1,4] <- muertos/(muertos + sobrevivientes)
}
OBESIDAD <- NA
sarscov2 <- cbind(sarscov2,OBESIDAD)
for (i in 0:100) {
  muertos <- nrow(covid19[(covid19$EDAD == i & 
                             covid19$OBESIDAD == 1 & covid19$DEFUNCIONES == 1),])
  sobrevivientes <- nrow(covid19[(covid19$EDAD == i & 
                                    covid19$OBESIDAD == 1 & covid19$DEFUNCIONES == 2),])
  sarscov2[i+1,5] <- muertos/(muertos + sobrevivientes)
}
TABAQUISMO <- NA
sarscov2 <- cbind(sarscov2,TABAQUISMO)
for (i in 0:100) {
  muertos <- nrow(covid19[(covid19$EDAD == i & 
                             covid19$TABAQUISMO == 1 & covid19$DEFUNCIONES == 1),])
  sobrevivientes <- nrow(covid19[(covid19$EDAD == i & 
                                    covid19$TABAQUISMO == 1 & covid19$DEFUNCIONES == 2),])
  sarscov2[i+1,6] <- muertos/(muertos + sobrevivientes)
}
GENERAL <- NA
sarscov2 <- cbind(sarscov2,GENERAL)
for (i in 0:100) {
  muertos <- nrow(covid19[(covid19$EDAD == i & 
                             covid19$DEFUNCIONES == 1),])
  sobrevivientes <- nrow(covid19[(covid19$EDAD == i & 
                                    covid19$DEFUNCIONES == 2),])
  sarscov2[i+1,7] <- muertos/(muertos + sobrevivientes)
}
DIAS_SINTOMAS <- NA
sarscov2 <- cbind(sarscov2,DIAS_SINTOMAS)
for (i in 0:100) {
  sarscov2[i+1,8] <- mean(covid19$DIAS_SIN[covid19$EDAD == i & 
                                             covid19$DEFUNCIONES == 1], na.rm = TRUE)
}
DIAS_INGRESO <- NA
sarscov2 <- cbind(sarscov2,DIAS_INGRESO)
for (i in 0:100) {
  sarscov2[i+1,9] <- mean(covid19$DIAS_ING[covid19$EDAD == i &
                                             covid19$DEFUNCIONES == 1], na.rm = TRUE)
}
sarscov2 <- as.data.frame(sarscov2)
### Limpieza
rm(ASMA)
rm(DIABETES)
rm(DIAS_INGRESO)
rm(DIAS_SINTOMAS)
rm(EDAD)
rm(GENERAL)
rm(HIPERTENSION)
rm(i)
rm(OBESIDAD)
rm(TABAQUISMO)
rm(sobrevivientes)
rm(muertos)
### Cambio de NAN a 0
sarscov2[is.na(sarscov2)] <- 0

################################################################################
# MODELO DE REGRESIÓN LINEAL
################################################################################

sarscov2 <- sarscov2[(sarscov2$EDAD > 17)&(sarscov2$EDAD < 89),]

regresion <- lm(DIABETES~EDAD,sarscov2[,c("DIABETES","EDAD")])
summary(regresion)

qt(1 - 0.025, df=69)

prueba <- cbind(as.numeric(sarscov2$EDAD),as.numeric(sarscov2$DIABETES))
plot(prueba$EDAD, prueba$DIABETES,
     ylab="",
     type="p",
     col="black")
lines(prueba$EDAD,(0.0074392*prueba$EDAD -0.1597019), col="blue")
lines(prueba$EDAD,(0.0069*prueba$EDAD -0.1854), col="violet")
lines(prueba$EDAD,(0.00789*prueba$EDAD -0.1339), col="violet")
legend("topleft",
       c("Recta de regresion lineal","Tasa de mortalidad",
         "Intervalo de confianza de los coeficientes"),
       fill=c("blue","black","violet")
)

qf(p = 1 - 0.05,df1 = 1,df2 = 69)

colnames(prueba) <- c("EDAD","DIABETES")
cor(prueba)