library(sp)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(RStoolbox)
library(landsat8)
library(ggspatial)

# Cargar bandas de sentinel
B2 = raster("Imagen/LC08_L1TP_003069_20220917_20220928_02_T1_B2.tif")
B5 = raster("Imagen/LC08_L1TP_003069_20220917_20220928_02_T1_B5.tif")
B6 = raster("Imagen/LC08_L1TP_003069_20220917_20220928_02_T1_B6.tif")

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Per           <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Amazonas       <- subset(Per, NAME_1  == "Madre de Dios")
Amazonas_box = st_as_sfc(st_bbox(Amazonas))

COLOR652= stack(B6,B5,B2)
COLOR652_py= projectRaster(COLOR652, crs="+proj=longlat +datum=WGS84 +no_defs")
ventana1  = extent(370000, 390000, -1400000, -1380000)
ventana11  = extent(-70.4, -70.1, -12.42, -12.22)

Mapa =ggRGB(COLOR652, r=1,g=2,b=3, stretch="lin", ext = ventana1)+
  ggspatial::annotation_scale(location = "tr", width_hint =0.5, pad_x = unit(0.7, "cm"))+ 
  annotate(geom = "text", x = 370000, y = -1400000, hjust = 0, vjust = 1,
           label = "Imágenes Landsat 8 de 2022/08/24",size = 3, family="serif", color =
             "black",  fontface="italic")+
             theme_minimal()+
             theme( axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                                family="serif",size=8),
                    axis.text.x  = element_text(face="bold", color="black", size=8,
                                                family="serif"),
                    panel.border = element_rect( color = "white", fill = NA, size = 1),
                    panel.background = element_rect(fill = NA),
                    plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
                    plot.subtitle = element_text(size = 8,  face = "italic", family="serif"),
                    plot.caption = element_text(size = 9, family="serif", face = "italic"),
             )+
  labs(
       x = "Long",
       y = "Lat") 


SurA= ggplot()+
  
  geom_sf(data = Per , fill="gray", color="gray", size=0.05)+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.1)+
  geom_sf(data = Amazonas, fill="black", color="black", size=0.01)+
  geom_sf(data = Amazonas_box, fill=NA, color="red", size=0.1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        
        panel.background = element_rect(fill = "#BFD1FF"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1,
           label = "Sur America",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
             annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1,
                      label = "Pacific ocean",size = 3, family="serif", color =
                        "black",  fontface="italic", angle=90)+
                        annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1,
                                 label = "Atlantic ocean",size = 3, family="serif", color =
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -75, y = -13, hjust = 0, vjust = 1,
                                            label = "Peru",size = 3, family="serif", color =
                                              "black",  fontface="italic")
                                            
SurA



# Mapa final
library(cowplot)
im=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Mapa, width = 21, height = 21,x = 0, y = 0)+
  draw_plot(SurA, width = 12, height = 12,x = 18.5, y = 5)+
  
  theme(panel.background = element_rect(fill = "white"))
 


# Exportacion
ggsave(plot = im ,"Mapas/Bol_area.png",
       units = "cm", width = 21,height = 29, dpi = 900)# guardar grafico  






































