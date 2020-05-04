
library(tidyverse)

##read data
Semana!=18
Mes %in% c("enero", "febrero", "marzo", "abril")
  
  clean_dir = "1.cleandata"
  charts_dir = "charts"
  
  
 cov19 =  read_rds(file.path(clean_dir,"llamadas-de-violencia.rds")) %>%
                     filter(Ano %in% c(2018,2019,2020)
                            ) %>%
   group_by(Ano, Semana) %>%
   summarise(Total = n(),
             Fecha= first(Date)) %>%
   mutate(covid = if_else(Ano==2020, "covid", "Nocovid")) %>%
   ungroup() %>%
   group_by(covid, Semana) %>%
   mutate(Promedio = mean(Total),
          Fecha = first(Fecha)) %>%
   ungroup()
 
 #
sum(cov19$Total)
 
 covid = cov19 %>%
   filter(covid == "covid")
 
 Nocovid = cov19 %>%
   filter(covid == "Nocovid") %>%
   filter(Semana <53)
 
 dif = cov19 %>%
   group_by(covid, Semana) %>%
   slice(1) %>%
   select(covid, Semana, Fecha, Promedio) %>%
   arrange(Semana, Fecha) %>%
   ungroup() %>%
   group_by(Semana) %>%
   mutate(dif = round((Promedio - lag(Promedio))/
            lag(Promedio), digits = 2) *100
   )
 

 3338 ##llamadas en el periodo de los tres anos
 1494 ##llamadas en 2018, 2019
 1884 ## llamadas en 2020
 




colorMujeres= "#652D90"
grisText = "#545658"



tema = theme(
  
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color=alpha("grey",.3)),
  panel.grid.major.x = element_line(color=alpha("grey",.3)),
  axis.title = element_text(size = 13, colour = "#585858"),
  axis.text = element_text(size = 10),
  axis.ticks = element_blank(),
  plot.title = element_text(size = 32,face = "bold",  margin = margin(10,5,20,5)),
  plot.subtitle = element_text(size = 24, colour = "gray35", margin = margin(0,5,20,5)),
  plot.caption = element_text(hjust = 0, size = 10),
)

##chart

sizeAnnotation = 6
sizeText = 5

 chart = ggplot() +
  geom_area(data = Nocovid %>%
              group_by(Semana) %>%
              slice(1),
            aes(x = Semana,
                y = Promedio),
            fill =alpha("grey",.2)
            ) +
  geom_line(data = Nocovid %>%
              group_by(Semana) %>%
              slice(1),
            aes(x = Semana,
                y = Promedio),
            size = 1,
            color = grisText,
            linetype = "dotted") +
  
geom_point(data = Nocovid,
           aes(x= Semana,
               y = Total),
           shape = 21,
           fill =alpha("grey",.6),
           color = "grey")+
  
  
  geom_line(data = covid ,
            aes(x = Semana,
                y = Total),
            size = 1,
            color = alpha(colorMujeres,.7)) +
  geom_point(data = covid[nrow(covid),],
             aes(x = Semana,
                 y = Total),
             shape = 21, 
             fill = alpha(colorMujeres,.7),
             color = colorMujeres
             ) +
  geom_text(aes(label = "Llamadas semanales\n2018-2019",
                x = 40,
                y = 20),
            colour = grisText,
            fontface = "italic",
            size = sizeAnnotation,
            hjust="left") +
  
  geom_text(aes(label = "2020",
                x = 3.5,
                y = 70),
            colour = alpha(colorMujeres,.7),
            fontface = "bold",
            size = sizeAnnotation,
            hjust="left") +
  
  geom_segment(aes(x = 11.1,
                 y = 207,
                 xend = 8.5,
                 yend=202),
               size=.5,
               color= alpha(grisText,.5)
               ) +
  geom_text(aes(x=3,
                y = 195,
                label= "Semana del\nDía Internacional\nde la Mujer"),
            size = sizeText,
            color = colorMujeres,
            hjust="left") +
  
  geom_segment(aes(x = 16,
                   y = 191,
                   xend = 19.3,
                   yend=180),
               size=.5,
               color= alpha(grisText,.5)
  ) +
   
   geom_segment(aes(x = 13,
                    y = 122,
                    xend = 11.5,
                    yend=100),
                size=.8,
                color= alpha(grisText,.5)
   ) +
   
   
   geom_text(aes(x=11,
                 y = 90,
                 label= "Comienzo de\nJornada Nacional de Sana Distancia"),
             size = sizeText,
             color = colorMujeres,
             hjust = "left") +
   
  geom_text(aes(x=19.7,
                y = 172,
                label= "Cuarta semana de\nJornada Nacional de Sana Distancia"),
            size = sizeText,
            color = colorMujeres,
            hjust = "left") +


  xlab("Semana del año") +
  ylab("") +
  labs(title='Llamadas semanales a "línea mujeres" categorizadas como\nviolencia de género intrafamiliar',
       subtitle = "Las llamadas se triplicaron en las semanas del 9 de marzo y del 13 de abril del 2020",
       caption = "Gráfica: www.andresarau.com\nDatos:Llamadas realizadas a Línea Mujeres (https://datos.cdmx.gob.mx/explore/dataset/linea-mujeres/table/)\n3 de Mayo,2020") +

  scale_x_continuous(breaks = seq(0,50, 5)) +
  
  
  ##text 
  tema

chart


exfile = file.path(charts_dir,"covidVSNocovid.png")
ggsave(exfile, chart, dpi = 150, width = 14, height = 9)




