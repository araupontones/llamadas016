
###chart tipo de violencia 
##durante covid 

library(tidyverse)

##read data
clean_dir = "1.cleandata"
charts_dir = "charts"

during = read_rds(file.path(clean_dir,"llamadas-de-violencia.rds")) %>%
  filter(Ano==2020,
         !is.na(TEMATICA_4)) %>%
  mutate(TEMATICA_4 = str_to_title(TEMATICA_4),
         total = n()) %>%
  group_by(TEMATICA_4) %>%
  summarise(perc = n()/mean(total)) %>%
  arrange(perc)


during$TEMATICA_4 <- reorder(during$TEMATICA_4, during$perc)

colorMujeres= "#652D90"
grisText = "#545658"

tema = theme(
  
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color=alpha("grey",.3)),
  panel.grid.major.x = element_line(color=alpha("grey",.3)),

  axis.text = element_text(size = 12, hjust = 0),
  axis.ticks = element_blank(),
  
  plot.title = element_text(size = 32,face = "bold",  margin = margin(10,5,20,5)),
  plot.subtitle = element_text(size = 24, colour = "gray35", margin = margin(0,0,20,0)),
  plot.caption = element_text(hjust = 0, size = 10),
)





chart = ggplot(data = during,
       aes(x = TEMATICA_4,
           y = perc))+
  
  geom_bar(stat = "identity",
           fill = alpha(colorMujeres, 0.7)) +
  
  geom_text(aes(label= paste(round(perc,
                             digits = 2) *100,"%")),
            nudge_y = .08,
            stat = "identity",
            color = grisText,
            size = 5) +
  
  coord_flip() +
  
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(0,1),
    breaks = seq(0,1,.2),
    labels = paste0(seq(0,1,.2)*100,"%")) +
  labs(title = "Tipo de violencia atendida en las llamadas categorizadas\ncomo violencia de género intrafamiliar",
       caption = "Gráfica: www.andresarau.com\nDatos:Llamadas realizadas a Línea Mujeres (https://datos.cdmx.gob.mx/explore/dataset/linea-mujeres/table/)\n3 de Mayo,2020") +
 
 tema
  
exfile = file.path(charts_dir,"tipo.png")
ggsave(exfile, chart, dpi = 150, width = 14, height = 9)




