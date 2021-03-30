# load necessary libraries
library(readxl)  # load dataset
library(ggplot2) # plots
library(dplyr)   # piping 
library(ggtext)  # add html & css
library(extrafont) # add fonts
library(patchwork) # caption below subtitle

# load dataset
antiguedad <- read_excel("data/1. Antiguedad.xlsx")

# clean dataset
antiguedad <- antiguedad %>%
  rename(universidad = Universidad,
         media_publicacion = `Media publicación`,
         media_publicacion_sin_ac = `Media publicacion (sin a.C.)`,
         de = `Desv. Est.`
  ) %>% 
  select(universidad, media_publicacion, media_publicacion_sin_ac, de) 

# plotting
antiguedad %>% 
  arrange(media_publicacion) %>% 
  mutate(universidad = factor(universidad, levels = universidad)) %>% 
  ggplot(aes(x = media_publicacion, y = as.factor(universidad), label = media_publicacion)) +
  geom_bar(width=0.7, stat="identity", aes(x = media_publicacion_sin_ac, fill="")) +
  geom_bar(width=0.7, stat="identity", aes(fill="blue")) +
  coord_cartesian(xlim=c(1940,1990)) +
  labs(title = "<span style = 'color:gray20;'>**Antigüedad media**</span> de los textos de las carreras de psicología argentinas según <span style = 'color:gray20;'>**Universidad**</span>",
       x = "<span style = 'color:gray20;'>**Antigüedad media**</span>", 
       y = "<span style = 'color:gray20;'>**Universidad**</span>",
       tag = "En <strong>promedio</strong>, la bibliografía de las <strong>asignaturas obligatorias</strong> de las carreras tiene <strong>44</strong> años de antigüedad cuando consideramos todos los textos.<br>
                   En <strong>ninguna</strong> carrera de psicología el promedio de publicación de la bibliografía es <strong>posterior</strong> al año <strong>1982</strong>.<br>
                   En la mayoría de las carreras, la bibliografía de las asignaturas se publicó en promedio entre la <strong>década</strong> de <strong>1970</strong> y <strong>1980</strong>.") +
  geom_vline(xintercept = mean(antiguedad$media_publicacion),
            linetype = "dashed",
            size = 2,
            colour = "gray80",
            fontface = "bold") +
  geom_label(colour = "gray20", 
            fontface = "bold", 
            size = 4, 
            hjust = 1.1, 
            family = "IBM Plex Sans") +
  geom_label(aes(mean(antiguedad$media_publicacion), 
            1,
            label = "Antigüedad media promedio: 1974"),
            size = 3.5,
            hjust = 0.1,
            family = "IBM Plex Sans", 
            show.legend = FALSE,
            colour = "gray80",
            fontface = "bold") +
  guides(alpha=FALSE) +
  scale_fill_manual(values=c("#FFD166", "gray20"), 
                    name="",
                    labels=c("Antigüedad media\nsustrayendo obras\nanteriores al siglo X", "Antigüedad media"),
                    guide = guide_legend(reverse = TRUE)) +
  theme(text = element_text(family = "IBM Plex Sans", size = 15),
        legend.text = element_text(size = 9),
        legend.position = c(0.87, 0.25),
        legend.direction = "vertical",
        legend.box.just = "center",
        plot.title = element_textbox_simple(
          hjust = 5, padding = margin(10, 0, 10, 10), margin = margin(0, 0, 0, 0)),
        plot.tag.position = "bottom",
        plot.tag = element_textbox_simple(size = 10,
                                   lineheight = 1.5,
                                   padding = margin(10, 0, 0, 10),
                                   margin = margin(0, 0, 5, 0),
                                   family = "IBM Plex Sans"),
        plot.title.position = "plot",
        axis.title.x = element_markdown(margin = margin(5, 0, 5, 0)),
        axis.title.y = element_markdown(margin = margin(0, 5, 0, 5)),
        panel.grid = element_blank(),
        plot.margin = margin(0, 15, 0, 0),
        plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
        legend.background = element_blank()
        ) +
  plot_annotation(caption = "Fuente: https://github.com/joaquinmenendez/analisis-bibliometrico-psiologia-Argentina | Gráfico: @francosbenitez", 
                  theme = theme(
                    plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
                    plot.caption = element_text(family = "IBM Plex Sans", 
                                                size = 8, 
                                                color = "gray30",
                                                hjust = 0.07))) +
  ggsave("antiguedad.png", device = "png", type = "cairo")
