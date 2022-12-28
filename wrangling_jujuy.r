library(readxl)
library(stringr)
library(tidyverse)
install.packages("xlsx")
#library("xlsx")

#seteo de workdirectory
wd <-setwd("C:/Users/mortiz/Downloads/portal/estilos/Resultados GENERALES 2017/Resultados GENERALES 2017")


#tabla para los datos generales de cada seccion
general <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(general) <- c('loc_electoral', 'sec_electoral', 'inscriptos','porcentaje','mesas')
general_template <- general

#por ahora no se usa
detalle <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(detalle) <- c("NRO","AGRUPACION POLITICA","SENADOR NACIONAL","DIPUTADO NACIONAL","DIPUTADO PROVINCIAL","INTENDENTE MUNICIPAL",
                       "COMISION COMUNAL",'CONCEJALES MUNICIPALES',"codigo")         

#listado de nombre de archivos
lista <- list.files(wd)
lista <- paste0("/", lista)


for (x in 1:length(lista)) {
  nombre_archivo <- lista[x]
  archivo <- read_excel(paste0(wd,nombre_archivo))
  
  nombre_archivo <- lista[x]
  archivo <- read_excel(paste0(wd,nombre_archivo))
  
  
  if (archivo[11,1] == "Extranjeros"){  
    general_temporal <- general_template
    loc <- as.character(archivo[6,2])
    sec <- archivo[7,2]
    inscriptos <- as.integer(archivo[12,2])
    porcentaje <- as.character(archivo[13,2])
    mesas <- as.integer(archivo[12,5])
    general[nrow(general) + 1,] <- list(loc,sec,inscriptos,porcentaje,mesas)
    rm(loc,sec,inscriptos,porcentaje,mesas)
    
    
    
  } else{
    general_temporal <- general_template
    loc <- archivo[6,2]
    sec <- archivo[7,2]
    inscriptos <- as.integer(archivo[10,2])
    porcentaje <- as.character(archivo[11,2])
    mesas <- as.integer(archivo[10,5])
    general[nrow(general) + 1,] <- list(loc,sec,inscriptos,porcentaje,mesas)
    rm(loc,sec,inscriptos,porcentaje,mesas)
    
  }
  
  
  
  #deteccion del campo NRO  
  z <- archivo[,1]
  z <- as.vector(z)
  c <- which(z=="NRO") # this will give you numerical value
  c <- c-1
  #leo el archivo desde la fila NRO  
  
  archivo <- read_excel(paste0(wd,nombre_archivo),na = "0", skip = c)
  archivo[is.na(archivo)]  <-  0
  variable_codigo <- nombre_archivo %>% str_sub(12,16)
  print(colnames(archivo))
  
  
  
  
  if (colnames(archivo)[6] == "COMISION COMUNAL"){
    
    archivo['INTENDENTE MUNICIPAL'] <- 0
    archivo[, colnames(archivo)[c(1:5,7,6)]]
    archivo['CONCEJALES MUNICIPALES'] <- 0
    
    
  }else if(colnames(archivo)[6] == "CONCEJALES MUNICIPALES"){
    archivo['INTENDENTE MUNICIPAL'] <- 0
    archivo['COMISION COMUNAL'] <- 0
    archivo[, colnames(archivo)[c(1:5,7,8,6)]]
  }  else {
    archivo['COMISION COMUNAL'] <- 0
    archivo[, colnames(archivo)[c(1:6,8,7)]]
  }
  
  archivo <- archivo %>% mutate(codigo=variable_codigo)
  
  detalle  <-  rbind(detalle, archivo)
  print(x)
  
}
#write_excel_csv(detalle,"detalle.csv")


#separar los codigos de los lugares del general
general[c('cod_elec', 'loc_electoral')] <- str_split_fixed(general$loc_electoral, ' - ', 2)
general[c('cod_sec', 'sec_electoral')] <- str_split_fixed(general$sec_electoral, ' - ', 2)

write_csv2(general,"general.csv",escape = c("double"), append = FALSE, quote = "all",)
