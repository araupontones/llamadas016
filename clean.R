library(tidyverse)
library(lubridate)
library(sf)

raw_dir = "0.rawdata"
clean_dir = "1.cleandata"

##raw data de llamadas
llamadas = read.csv(file.path(raw_dir, "linea-mujeres.csv"), encoding = "UTF-8")

##shapefile con codigos postales
shp = file.path(raw_dir,"codigos-postales-de-la-cdmx/codigos-postales-de-la-cdmx.shp")
shape = sf::st_read(shp)

##codigo postal con colonias
# colonias = read.csv(file.path(raw_dir,"codigo_postal.csv"), encoding = "UTF-8") %>%
#    rename(d_cp = X.U.FEFF.d_codigo,
#           colonia = d_asenta) %>%
#   mutate(d_cp = if_else(str_length(d_cp)==4, 
#                   paste0("0",d_cp),
#                   as.character(d_cp)
#                   )
#          )
# 
# 
# setdiff(shape$d_cp, colonias$d_cp)



##limpiar Fecha y renombrar variables de ubicacion
 llamadas2 = llamadas %>%
   mutate(Fecha = str_replace_all(as.character(FECHA_ALTA),'T|\\+02:00'," "),
          Ano = year(Fecha),
          Mes = month(Fecha, 
                      label = T, 
                      locale = Sys.setlocale("LC_TIME", "Spanish"),
                      abbr = F),
          Semana = week(Fecha),
          Hora = hour(Fecha),
          Dia = wday(Fecha,
                     label = T,
                     locale = Sys.setlocale("LC_TIME", "Spanish"),
                     abbr = F)
          ) %>%
    rename(Estado = ESTADO_USUARIA,
           Colonia = COLONIA_USUARIA,
           Municipio = MUNICIPIO_USUARIA) %>%
    select(-c(FECHA_ALTA,AÑO_ALTA,MES_ALTA, DÍA_ALTA))
          
  

  
        


 ##identificar variables de tema 
vars_tema =  tidyselect::vars_select(names(llamadas2), starts_with("TEMA"))

 
 violentas = llamadas2 %>%
   mutate_at(vars_tema, as.character) %>% 
    filter(TEMATICA_1=="VIOLENCIA",
           TEMATICA_2=="DE GÉNERO",
           TEMATICA_3=="FAMILIAR") %>%
    mutate(Dia = day(Fecha),
           Date = ymd(paste(Ano, Mes, Dia, sep = "-"))) %>%
    group_by(Ano, Semana) %>%
    arrange(Date) %>%
    mutate(Week = first(Date)) %>%
    ungroup() %>%
    mutate(covid = if_else(Ano==2020, "covid", "Nocovid"))
    
 

##save llamadas violentas 
write_rds(violentas, file.path(clean_dir,"llamadas-de-violencia.rds"))


###por meses de coronavirus
covid = violentas %>%
   filter(Mes %in% c("enero", "febrero", "marzo", "abril"), Ano > 2017) %>% ##2017 tiene pocas observaciones
   mutate(covid = if_else(Ano==2020, "covid", "Nocovid")) %>%
   group_by(covid, Semana) %>% ##promedio entre ambos
   summarise(total = mean(n()),
             Date = first(Date) 
   )%>%
   ungroup() 
 

write_rds(covid, file.path(clean_dir,"llamadas-comparacion-covid.rds"))


 
##data para mapear
tomap = violentas %>%
   filter(Estado=="CIUDAD DE MÉXICO") %>%
   rename(d_cp = CP_USUARIA) %>%
   mutate(d_cp = as.character(d_cp),
          d_cp = if_else(str_length(d_cp)==4, 
                      paste0("0",d_cp),
                      d_cp)
   ) %>%
   group_by(d_cp, Colonia) %>%
   summarise(total = n())
      



#data to be mmaped 
tomap = full_join(shape, tomap,by ="d_cp") %>%
   mutate(l = st_is_empty(geometry)) %>%
          #total = if_else(is.na(total), 0, as.numeric(total))) %>%
   filter(l==FALSE)

to_map= st_simplify(to_map)
to_map= st_simplify(to_map)
to_map= st_simplify(to_map)

write_rds(tomap, file.path(clean_dir,"llamadas_shape.rds"))

