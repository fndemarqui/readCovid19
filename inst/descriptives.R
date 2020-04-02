
br <- brasil %>%
  group_by(data) %>%
  summarise(casosNovos = sum(casosNovos),
            casosAcumulados = sum(obitosAcumulados),
            casosAcumulados = sum(casosAcumulados),
            obitosAcumulados = sum(obitosAcumulados))


estados  <- brasil %>%
  group_by(estado, data) %>%
  summarise(casosNovos = sum(casosNovos),
            casosAcumulados = sum(obitosAcumulados),
            casosAcumulados = sum(casosAcumulados),
            obitosAcumulados = sum(obitosAcumulados))


regioes  <- brasil %>%
  group_by(regiao, data) %>%
  summarise(casosNovos = sum(casosNovos),
            casosAcumulados = sum(obitosAcumulados),
            casosAcumulados = sum(casosAcumulados),
            obitosAcumulados = sum(obitosAcumulados))


ggplot(filter(regioes, data>"2020-03-14"), aes(x=data, y=casosAcumulados, color=regiao)) +
  geom_point() +
  geom_line()



ggplot(filter(estados, data>"2020-03-14"), aes(x=data, y=casosAcumulados, color=estado)) +
  geom_point() +
  geom_line()


#-------------------------------------------------------------
# packages to plot maps:
pcks <- c("cowplot", "googleway", "ggrepel", "ggspatial",
          "libwgeom", "rnaturalearth", "rnaturalearthdata", "rgeos")
#install.packages(pcks)
#-------------------------------------------------------------


theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

data(world)
mundo <- world
paises <- unique(mundo$local)
mundo <- world %>%
  group_by(data) %>%
  summarise(mortes=sum(mortes))


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

teste1 <- world$name



ggplot(data = world) +
  geom_sf()


ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))


ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")



ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
