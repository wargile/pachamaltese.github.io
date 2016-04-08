
setwd("/Users/pacha/pachamaltese.github.io/blog/content/tradechilechina/")
library(XLConnect)
library(rCharts)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(plyr)
library(cowplot)


file <- paste0(getwd(),"/datos-TLC-Chile-China.xlsx")

data <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A3:K12", header = TRUE)
data <- as.data.frame(data)

data2 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A16:F25", header = TRUE)
data2 <- as.data.frame(data2)

data3 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A28:C33", header = TRUE)
data3 <- as.data.frame(data3)
data3$pais <- factor(data3$pais, levels = c("china","eeuu","ue","japon","corea"), labels = c("China","EEUU","UE","Japón","Corea"))

data4 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A37:E55", header = TRUE)
data4 <- as.data.frame(data4)
data4$producto <- factor(data4$producto, levels = c("cobre","otros"), labels = c("Cobre","Celulosa, fruta, salmón y otros"))
data4 <- ddply(data4, .(anio), transform, pos = cumsum(porcentaje) - (0.5 * porcentaje))
data4 <- ddply(data4, .(anio), transform, pos2 = cumsum(expo) - (0.5 * expo))

data5 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A58:G67", header = TRUE)
data5 <- as.data.frame(data5)

data6 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A70:C115", header = TRUE)
data6 <- as.data.frame(data6)
data6$producto <- factor(data6$producto, levels=c("frutas",	"alimentosprocesados",	"vinoembotellado",	"salmon",	"forestalymuebles"), labels=c("Frutas",	"Alimentos\nprocesados",	"Vino\nembotellado",	"Salmón",	"Forestal y\nmuebles de madera"))

brewer.pal(8, "Paired")
cbPalette <- c("#1F78B4", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#556b2f", "#D55E00", "#CC79A7")
cbPalette2 <- c("#4169e1", "#d68a59","#556b2f", "#FB9A99", "#33A02C")
cbPalette3 <- c("#56B4E9", "#F0E442")

g1 <- ggplot() + geom_bar(aes(y = porcentaje, x = pais, fill = pais), data = data3, stat="identity")
g1 <- g1 + scale_fill_manual(values=cbPalette2) + geom_text(data=data3, aes(x = pais, y = (porcentaje + 2), label = paste0(porcentaje,"%")), colour="black", family="OfficinaSanITC-Book", size = 5)
g1 <- g1 + scale_y_continuous(breaks=seq(0,25,5), labels = dollar_format(suffix = "%", prefix = "")) + expand_limits(y=c(0,25)) + labs(x="",y="Porcentaje")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g1 <- g1 + theme(legend.position="none", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Principales destinos de exportación en 2014") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book"), axis.text.x=element_text(colour="black"))

g2 <- ggplot() + geom_bar(aes(y = pcentexpo, x = anio, fill = "pcentexpo"), data = data5, stat="identity")
g2 <- g2 + scale_fill_manual(values="#4169e1") + geom_text(data=data5, aes(x = anio, y = pcentexpo +2, label = paste0(pcentexpo,"%")), colour="black", family="OfficinaSanITC-Book", size = 5)
g2 <- g2 + scale_x_continuous(breaks=seq(2006,2014,1)) + scale_y_continuous(breaks=seq(0,25,5), labels = dollar_format(suffix = "%", prefix = "")) + expand_limits(y=c(0,25)) + labs(x="Año",y="Porcentaje")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g2 <- g2 + theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("% de exportación total que representa China") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book"))

g3 <- ggplot() + geom_bar(aes(y = pcentimpo, x = anio, fill = "pcentimpo"), data = data5, stat="identity")
g3 <- g3 + scale_fill_manual(values="#000080") + geom_text(data=data5, aes(x = anio, y = pcentimpo +2, label = paste0(pcentimpo,"%")), colour="black", family="OfficinaSanITC-Book", size = 5)
g3 <- g3 + scale_x_continuous(breaks=seq(2006,2014,1)) + scale_y_continuous(breaks=seq(0,25,5), labels = dollar_format(suffix = "%", prefix = "")) + expand_limits(y=c(0,25)) + labs(x="Año",y="Porcentaje")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g3 <- g3 + theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("% de importación total que representa China") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book"))

g4 <- ggplot() + geom_bar(aes(y = porcentaje, x = anio, fill = producto), data = data4, stat="identity")
g4 <- g4 + scale_fill_manual(values=cbPalette3) + geom_text(data=data4, aes(x = anio, y = pos, label = paste0(porcentaje,"%")), colour="black", family="OfficinaSanITC-Book", size = 5)
g4 <- g4 + scale_x_continuous(breaks=seq(2006,2014,1)) + scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + labs(x="Año", y="Porcentaje")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g4 <- g4 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Composición de exportaciones a China (%)") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book"))

g5 <- ggplot() + geom_bar(aes(y = expo, x = anio, fill = producto), data = data4, stat="identity")
g5 <- g5 + scale_fill_manual(values=cbPalette3) + geom_text(data=data4, aes(x = anio, y = pos2, label = expo), colour="black", family="OfficinaSanITC-Book", size = 5)
g5 <- g5 + scale_x_continuous(breaks=seq(2006,2014,1)) + labs(x="Año", y="Millones de USD")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g5 <- g5 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Composición de exportaciones a China ($)") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book"))

g6 <- ggplot() + geom_area(aes(y = expo, x = anio, fill = producto), data = data6, stat="identity") 
g6 <- g6 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,1600,400)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
#g6 <- g6 + scale_fill_manual(values=cbPalette)
g6 <- g6 + scale_fill_brewer(palette="Paired")
g6 <- g6 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Exportaciones sin cobre ni celulosa a China") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book"))

g7 <- ggplot() + geom_line(aes(y = expocc, x = anio, colour = "expocc"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impocc, x = anio, colour = "impocc"), size=1.5, data = data, stat="identity") 
g7 <- g7 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g7 <- g7 + scale_color_manual(labels = c("Exportaciones", "Importaciones"), values = c("#4169e1", "#000080"))
g7 <- g7 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Comercio Chile-China") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 2))

#range(data2$bccm)
g8 <- ggplot() + geom_line(aes(y = expoceeuu, x = anio, colour = "expoceeuu"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impoceeuu, x = anio, colour = "impoceeuu"), size=1.5, data = data, stat="identity") 
g8 <- g8 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g8 <- g8 + scale_color_manual(labels = c("Exportaciones", "Importaciones"), values = c("#a0522d", "#E18942"))
g8 <- g8 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Comercio Chile-Estados Unidos") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 2))

g9 <- ggplot() + geom_line(aes(y = expocue, x = anio, colour = "expocue"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impocue, x = anio, colour = "impocue"), size=1.5, data = data, stat="identity") 
g9 <- g9 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g9 <- g9 + scale_color_manual(labels = c("Exportaciones", "Importaciones"), values = c("#556b2f", "#2f556b"))
g9 <- g9 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Comercio Chile-UE") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 3))

g10 <- ggplot() + geom_line(aes(y = bccc, x = anio, colour = "bccc"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccm, x = anio, colour = "bccm"), size=1.5, data = data2, stat="identity") 
g10 <- g10 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g10 <- g10 + scale_color_manual(labels = c("Balanza comercial con China", "Balanza comercial con el Mundo"), values = c("#4169e1", "#FF43A4"))
g10 <- g10 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Balanza comercial con China y el Mundo") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 2))

#range(data2$bccm);range(data2$bccue);range(data2$bcceeuu)
g11 <- ggplot() + geom_line(aes(y = bcceeuu, x = anio, colour = "bcceeuu"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccm, x = anio, colour = "bccm"), size=1.5, data = data2, stat="identity") 
g11 <- g11 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(-10000,25000,5000)) + expand_limits(y=c(-10000,25000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g11 <- g11 + scale_color_manual(labels = c("Balanza comercial con EEUU", "Balanza comercial con el Mundo"), values = c("#d68a59", "#FF43A4"))
g11 <- g11 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Balanza Comercial con EEUU y el Mundo") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 3))

g12 <- ggplot() + geom_line(aes(y = bccue, x = anio, colour = "bccue"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccm, x = anio, colour = "bccm"), size=1.5, data = data2, stat="identity") 
g12 <- g12 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g12 <- g12 + scale_color_manual(labels = c("Balanza comercial con el mundo", "Balanza comercial con UE"), values = c("#FF43A4", "#556b2f"), guide = guide_legend(reverse=TRUE))
g12 <- g12 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Balanza Comercial con UE y el Mundo") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 2))

g13 <- ggplot() + geom_line(aes(y = expocc, x = anio, colour = "expocc"), size=1.5, data = data, stat="identity") + geom_line(aes(y = expoceeuu, x = anio, colour = "expoceeuu"), size=1.5, data = data, stat="identity") + geom_line(aes(y = expocue, x = anio, colour = "expocue"), size=1.5, data = data, stat="identity") 
g13 <- g13 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g13 <- g13 + scale_color_manual(labels = c("Exportaciones a China (socio comercial #1)", "Exportaciones a EEUU (socio comercial #2)", "Exportaciones a UE (socio comercial #3)"), values = c("#4169e1", "#d68a59","#556b2f"))
g13 <- g13 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Exportaciones a China, EEUU y UE") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 3))

#range(data$impocc);range(data$impoceeuu);range(data$impocue)
g14 <- ggplot() + geom_line(aes(y = impocc, x = anio, colour = "impocc"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impoceeuu, x = anio, colour = "impoceeuu"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impocue, x = anio, colour = "impocue"), size=1.5, data = data, stat="identity") 
g14 <- g14 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(4000,20000,4000)) + expand_limits(y=c(4000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g14 <- g14 + scale_color_manual(labels = c("Importaciones de China (socio comercial #1)", "Importaciones de EEUU (socio comercial #2)", "Importaciones de UE (socio comercial #3)"), values = c("#4169e1", "#d68a59","#556b2f"))
g14 <- g14 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Importaciones de China, EEUU y UE") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 3))

#range(data2$bccc);range(data2$bcceeuu);range(data2$bccue)
g15 <- ggplot() + geom_line(aes(y = bccc, x = anio, colour = "bccc"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bcceeuu, x = anio, colour = "bcceeuu"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccue, x = anio, colour = "bccue"), size=1.5, data = data2, stat="identity") 
g15 <- g15 + ylab("Millones de USD") + xlab("Año") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(-10000,15000,5000)) + expand_limits(y=c(-10000,15000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g15 <- g15 + scale_color_manual(labels = c("Balanza Comercial con China (socio comercial #1)", "Balanza Comercial con EEUU (socio comercial #2)", "Balanza Comercial con UE (socio comercial #3)"), values = c("#4169e1", "#d68a59","#556b2f"))
g15 <- g15 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("Balanza Comercial con China, EEUU y UE") + theme(plot.title=element_text(size=20, family="OfficinaSanITC-Book"), text=element_text(size=18, family="OfficinaSanITC-Book")) + guides(col = guide_legend(nrow = 3))

png("chile-china-mundo-es.png",width=600*1,height=380*16) 
grid.arrange(
top = textGrob("\nComercio Chile-China-Mundo\nAnálisis a 10 años del TLC Chile-China\nHecho por @pachamaltese\nFuentes: ADUANA, BCCH y DIRECON\n",gp=gpar(fontsize=25,fontfamily="OfficinaSanITC-Book")),
plot_grid(g1,g2,g3,
          g4,g5,g6,
          g7,g8,g9,
          g10,g11,g12,
          g13,g14,g15,
          nrow=15, ncol=1, align="v"),
nrow=1,ncol=1
)
dev.off()


