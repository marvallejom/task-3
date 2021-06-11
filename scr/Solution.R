#==============================================================================#
# Autores: Maria Vallejo, Andrea Cortes, Miguel Castillo
# # Fecha elaboracion:18 de mayo de 2021
# Ultima modificacion: 10 de junio de 2021
# Version de R: 4.0.3
# TALLER B
#==============================================================================#
#Se cargan las librerias
pacman::p_load(tidyverse,sf,raster,coefplot,outreg, here, skimr, viridis, gapmindere, XML, rvest, xml2)
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

#1.2.Atributos de los objetos=========================================================================#
skim(c_poblado)
skim(depto)
skim(mapmuse)
skim(c_medico)
skim(via)
#Para explorar los objetos cargados se usa skim para conocer la descripción.

#1.3. Geometria del objeto=========================================================================#

#1.3.1=========================================================================#

st_bbox(c_poblado)
st_crs(c_poblado)

st_bbox(via)
st_crs(via)

st_bbox(depto)
st_crs(depto)

st_bbox(mapmuse)
st_crs(mapmuse)

st_bbox(c_medico)
st_crs(c_medico)

#se obtienen las coordenadas y el CRS de los objetos del 1.1

#1.3.2=========================================================================#

c_poblado = st_transform(c_poblado, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_medico = st_transform(c_medico, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
depto = st_transform(depto, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
mapmuse = st_transform(mapmuse, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
via = st_transform(via, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
puntos = st_transform(puntos, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")

# Se muestra el CRS de los objetos

# 1.4. Operaciones geométricas

#1.4.1=========================================================================#

mapmuse=mapmuse[depto,]
mapmuse=st_intersection(mapmuse, depto)
ggplot() + geom_sf (data=mapmuse[depto,], col="blue") + geom_sf(data=depto, fill=NA, col="yellow") + theme_bw()

#1.4.2=========================================================================#

len_vias = st_intersection(vias,c_poblados %>% subset(cod_dane == 54001)) %>% st_length()
sample(c_poblado$codmopio,1)

# 1.5 Pintar mapas

#1.5.1=========================================================================#

mapa=leaflet() %>% addTiles() %>% addCircleMarkers(data=puntos %>% st_transform(.,4326))

#1.5.2=========================================================================#

mapa2=ggplot(puntos%>% st_transform(.,4326), col="green") + geom_sf(fill="blue", alpha=0.4, col="blue", size=1) +
  geom_sf(data=depto, fill=NA, col="red") + themebw()+north(puntos%>% st_transform(.,4326)) + scale_fill_continuous(low="##B2182B",high="#2166AC")

ggsave(plot = mapa2, file = "mapa.pdf")



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

#3.WEB-SCRAPING==================================================================#
#3.1.Extraer HTML=========================================================#
require(pacman)
p_load(tidyverse,XML,rvest,xml2)


myurl = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"

myhtml = read_html(myurl)
class(myhtml)


#3.2.Extraer título de la página==================================================#

myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/title')
myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/title') %>% 
  class()
texto = myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/title') %>% 
  html_text() # Convertir en texto
texto

#3.3.Extraer tabla de departamentos==================================================#

