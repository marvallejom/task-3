#==============================================================================#
# Autores: Maria Vallejo, Andrea Cortes, Miguel Castillo
# # Fecha elaboracion:18 de mayo de 2021
# Ultima modificacion: 08 de junio de 2021
# Version de R: 4.0.3
# TALLER B
#==============================================================================#
#Se cargan las librerias
pacman::p_load(tidyverse,sf,raster,coefplot,outreg)
#==============================================================================#
#1.DATOS ESPACIALES
#1.1 Importar datos espaciales
#1.1.1 ========================================================================#
via=st_read(dsn = 'data/input/VIAS.shp')
puntos=st_read(dsn = 'data/input/MGN_URB_TOPONIMIA.shp')

#1.1.2=========================================================================#
posiciones_a_eliminar=c()
for(i in 1:nrow(puntos)){
  if(puntos$CSIMBOL[i]!="021001" & puntos$CSIMBOL[i]!="021002" & puntos$CSIMBOL[i]!="021003"){
    posiciones_a_eliminar=c(posiciones_a_eliminar,i)
  }
}
c_medico=puntos[-posiciones_a_eliminar,]

#1.1.3=========================================================================#
c_poblado=read_rds('data/input/c poblado (2017).rds')
depto=read_rds('data/input/dp deptos (2017).rds')
mapmuse=read_rds('data/input/victimas_map-muse.rds')
#Se dejan unicamente los centros poblados con codigo DANE >= 54001 & < 55000
posiciones_a_dejar=c()
for(i in 1:nrow(c_poblado)){
  if(c_poblado$cod_dane[i]>=54001 & c_poblado$cod_dane[i]<55000){
    posiciones_a_dejar=c(posiciones_a_dejar,i)
  }
}
c_poblado=c_poblado[posiciones_a_dejar,]

#2.REGRESIONES==================================================================#
mapmuse=read_rds('data/output/f_mapmuse.rds')
dist_vias=mapmuse$dist_vias
dist_cmedico=mapmuse$dist_hospi
dist_cpoblado=mapmuse$dist_cpoblado
fallecido=mapmuse$fallecido

#Dataframe sin estado, geometry y month
mapmuse=mapmuse[,-3]

vector=colnames(mapmuse)
vector[7]="dist_cmedico"

colnames(mapmuse)=vector

#2.1. Regresion Lineal=========================================================#
ols=lm(formula = fallecido~.,data = mapmuse)
ols %>% summary() 

#2.2.Exportacion de graficos===================================================#
jpeg("Gráfica con los coeficientes de las estimaciones.jpeg")
coefplot(ols,col=2)

#2.3. Modelo logit y probit====================================================#
logit = glm(formula = fallecido~., data = mapmuse, family = binomial(link = "logit")) 
logit %>% summary()

probit = glm(formula = fallecido~., data = mapmuse, family = binomial(link = "probit") ) 
probit %>% summary()

#2.4.Resultados obtenidos======================================================#
lista_modelos=list(ols,logit,probit)
resultados=outreg(lista_modelos)
colnames(resultados)=c("Variable", "Estadistica","OLS","Logit","Probit")
view(resultados)

#2.5.Efecto marginal de la distancia a un centro medico========================#
jpeg("Gráfica de efecto marginal para Logit.jpeg")
coefplot(model=logit, coefficients=c("dist_cmedico"),xlab="efecto marginal")

jpeg("Gráfica de efecto marginal para Probit.jpeg")
coefplot(model=probit, coefficients=c("dist_cmedico"),xlab="efecto marginal")



