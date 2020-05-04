
###chart tipo de violencia 
##durante covid 

library(tidyverse)

##read data
clean_dir = "1.cleandata"
during = read_rds(file.path(clean_dir,"llamadas-de-violencia.rds")) %>%
  filter(Ano==2020,
         !is.na(TEMATICA_5),
         str_detect(TEMATICA_5,"POR PARTE")) %>%
  mutate(TEMATICA_5 = str_to_title(TEMATICA_5),
         TEMATICA_5 = str_remove(TEMATICA_5,"Por Parte Del |Por Parte De La "),
         total = n()) %>%
  group_by(TEMATICA_5) %>%
  summarise(perc = (round(n()/ mean(total),digits = 3))*100) %>%
  arrange(perc)

  
  
  
during$TEMATICA_5 <- reorder(during$TEMATICA_5, during$perc)


table(during$TEMATICA_5)
colorMujeres= "#652D90"
grisText = "#545658"

tema = theme(
  
  panel.background = element_blank(),
  plot.title = element_text(size = 30,face = "bold", margin = margin(10,0,20,0), hjust = .5),
  axis.text.y = element_text(size = 12, hjust = .1, colour = grisText),
  axis.text.x = element_blank(),
  axis.ticks = element_blank(),
  plot.subtitle = element_text(size = 18, colour = "gray35"),
  plot.caption = element_text(hjust = 1, size = 10),
)



last = nrow(during)
thirdlast = last -3

ggplot(data = during,
       aes(x = TEMATICA_5,
           y = perc)) +
 
  
  geom_bar(stat = "identity",
           fill = alpha(colorMujeres, 0.7))  +
    coord_flip() +
  
  geom_text(data = during[thirdlast:last,], aes(label= paste0(perc,"%")),
            nudge_y = 7,
            color = grisText,
            size = 5) +
  
  xlab("")+
  ylab("")+
  ylim(0,100)+
  labs(title = "¿Quién es el agresor?",
       caption = "@AndresArau\nDatos:Llamadas realizadas a Línea Mujeres (https://datos.cdmx.gob.mx/explore/dataset/linea-mujeres/table/)\n3 de Mayo,2020") +
  tema
  
  