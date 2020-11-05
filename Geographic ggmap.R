####
setwd("~/Dropbox/Miscanthus Flowering Study")
####
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(raster)
library(elevatr)
library(rgdal)
library(sf)
library(grid)

# Download Natural Earth Data rivers file and read shape file
fileName <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_rivers_lake_centerlines.zip"
temp <- tempfile()
download.file(fileName, temp, mode="wb")
unzip(temp)
shapeData <- readOGR("ne_50m_rivers_lake_centerlines.shp")
#unlink(c(temp, "ne_*"))

# I read in the shapefile, but I'm not sure how to work with that. But
# I do understand data frames, so that's what I'm converting it to.
shapeData@data$id <- rownames(shapeData@data)
watershedPoints <- fortify(shapeData, region = "id")
watershedDF <- merge(watershedPoints, shapeData@data, by = "id")

# Now just subset the data to include the rivers that I want to plot:
my.rivers <- watershedDF[watershedDF$name == "Jinsha" 
                         |watershedDF$name == "Chang Jiang"
                         |watershedDF$name == "Yangtze"
                         | watershedDF$name == "Huang"
                         | watershedDF$name == "Xi"
                         |watershedDF$name == "Hongshui"
                         |watershedDF$name == "Quan"
                         |watershedDF$name == "Nanpan"
                         |watershedDF$name == "Xun",]


world <- map_data("world")
labs <- read.csv("Miscanthus flowering time.csv", header = TRUE)


Photoperiod=read.csv("Boxplot and regression.csv",header = TRUE)
# Remove Msa 'Hortico', Mxg, hybrid
Msi.Photoperiod=Photoperiod[!c(Photoperiod[,2]=='UI10-00008' | Photoperiod[,2]=='UI10-00107' | Photoperiod[,2]=='PMS-300'),]
length(unique(Msi.Photoperiod[,2])) # 31 Miscanthus genotypes
str(Msi.Photoperiod)
Msi.Photoperiod$Photoperiod <- as.factor(Msi.Photoperiod$Photoperiod)
  
Asia_bbox <- c(left = 103, bottom = 17, right = 160, top = 47)
Asia_main_map <- get_stamenmap(Asia_bbox, zoom = 5, maptype = "terrain-background")






tiff("Background.tiff", width = 11.75, height = 7.1, units = "in", res = 400)
p_main <- ggmap(Asia_main_map) +
  theme_void() +
  theme(axis.title = element_blank(), 
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  geom_path(data=my.rivers, 
            aes(x = long, y = lat, group = group), 
            color = 'cyan', size=0.5)+
  
  geom_hline(yintercept=40, linetype="dashed", color = "floralwhite") +
  geom_hline(yintercept=30, linetype="dashed", color = "floralwhite") +
  geom_hline(yintercept=20, linetype="dashed", color = "floralwhite") +
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 0.3)


# Next, get Paupa New Guinea
guinea_bbox <- c(left = 140, bottom = -22, right = 170, top = 0)
guinea_map <- get_stamenmap(guinea_bbox, zoom = 5, maptype = "terrain-background") 
p_guinea <- ggmap(guinea_map) + 
  theme_void() +
  theme(axis.title = element_blank(), 
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  geom_hline(yintercept=-10, linetype="dashed", color = "floralwhite") +
  geom_hline(yintercept=-20, linetype="dashed", color = "floralwhite") +
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 0.3)


# boxplot for first flower
Msi.first.flower=Msi.Photoperiod[c(Msi.Photoperiod[,3]=="Leaf number"),]

# Basic box plot
p_box <- ggplot(Msi.first.flower, aes(x=Value, y=Photoperiod, fill=Photoperiod)) + 
  scale_fill_manual(values = c("#0070C0", "#33CC33", "#FFC000")) +
  xlab("Leaf number (count)") +
  ylab("Photoperiod (h)") +
  theme(axis.text=element_text(size=10, color = "black"),
        axis.title=element_text(size=10),
        plot.background = element_rect(color = "black", fill="white", size=1)) +
  geom_boxplot(outlier.shape = 20) + theme(legend.position = "none")


# Regression against latitude
p_regression <- ggplot(Msi.first.flower, aes(x=abs(Latitude), y=Value, group=Photoperiod)) +
  geom_smooth(method = lm, se=FALSE, fullrange=TRUE, aes(color=Photoperiod))+
  scale_color_manual(values = c("#0070C0", "#33CC33", "#FFC000")) +
  geom_point(aes(color=Photoperiod), size=1)+
  scale_color_manual(values = c("#0070C0", "#33CC33", "#FFC000")) +
  theme(legend.position = "none") +
  xlab("Latitude") +
  ylab("Leaf number (count)") +
  theme(axis.text=element_text(size=10, color = "black"),
        axis.title=element_text(size=10),
        plot.background = element_rect(color = "black", fill="white", size=1))


p_main + 
  inset(ggplotGrob(p_guinea), xmin = 146, xmax = 160, ymin = 12, ymax = 31.3) +
  inset(ggplotGrob(p_regression), xmin = 146, xmax = 160, ymin = 26.5, ymax = 37.06) +
  inset(ggplotGrob(p_box), xmin = 146, xmax = 160, ymin = 37.04, ymax = 47)

dev.off()
