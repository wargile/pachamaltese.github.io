
setwd("/Users/pacha/pachamaltese.github.io/stats/trade-chile-china/")
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
data3$pais <- factor(data3$pais, levels = c("china","eeuu","ue","japon","corea"), labels = c("中国","美国","欧盟","日本","朝鲜"))

data4 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A37:E55", header = TRUE)
data4 <- as.data.frame(data4)
data4$producto <- factor(data4$producto, levels = c("cobre","otros"), labels = c("铜","木浆, 水果, 鱒屬和等产品"))
data4 <- ddply(data4, .(anio), transform, pos = cumsum(porcentaje) - (0.5 * porcentaje))
data4 <- ddply(data4, .(anio), transform, pos2 = cumsum(expo) - (0.5 * expo))

data5 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A58:G67", header = TRUE)
data5 <- as.data.frame(data5)

data6 <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A70:C115", header = TRUE)
data6 <- as.data.frame(data6)
data6$producto <- factor(data6$producto, levels=c("frutas",	"alimentosprocesados",	"vinoembotellado",	"salmon",	"forestalymuebles"), labels=c("水果",	"再制",	"瓶装酒",	"鱒屬",	"林业和木家具"))

brewer.pal(8, "Paired")
cbPalette <- c("#1F78B4", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#556b2f", "#D55E00", "#CC79A7")
cbPalette2 <- c("#4169e1", "#d68a59","#556b2f", "#FB9A99", "#33A02C")
cbPalette3 <- c("#56B4E9", "#F0E442")

g1 <- ggplot() + geom_bar(aes(y = porcentaje, x = pais, fill = pais), data = data3, stat="identity")
g1 <- g1 + scale_fill_manual(values=cbPalette2) + geom_text(data=data3, aes(x = pais, y = (porcentaje + 2), label = paste0(porcentaje,"%")), colour="black", family="AdobeHeitiStd-Regular", size = 5)
g1 <- g1 + scale_y_continuous(breaks=seq(0,25,5), labels = dollar_format(suffix = "%", prefix = "")) + expand_limits(y=c(0,25)) + labs(x="",y="百分数")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g1 <- g1 + theme(legend.position="none", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("2014年领先的出口市场") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular"), axis.text.x=element_text(colour="black"))

g2 <- ggplot() + geom_bar(aes(y = pcentexpo, x = anio, fill = "pcentexpo"), data = data5, stat="identity")
g2 <- g2 + scale_fill_manual(values="#4169e1") + geom_text(data=data5, aes(x = anio, y = pcentexpo +2, label = paste0(pcentexpo,"%")), colour="black", family="AdobeHeitiStd-Regular", size = 5)
g2 <- g2 + scale_x_continuous(breaks=seq(2006,2014,1)) + scale_y_continuous(breaks=seq(0,25,5), labels = dollar_format(suffix = "%", prefix = "")) + expand_limits(y=c(0,25)) + labs(x="年",y="百分数")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g2 <- g2 + theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("出口中国的产品的结构") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular"))

g3 <- ggplot() + geom_bar(aes(y = pcentimpo, x = anio, fill = "pcentimpo"), data = data5, stat="identity")
g3 <- g3 + scale_fill_manual(values="#000080") + geom_text(data=data5, aes(x = anio, y = pcentimpo +2, label = paste0(pcentimpo,"%")), colour="black", family="AdobeHeitiStd-Regular", size = 5)
g3 <- g3 + scale_x_continuous(breaks=seq(2006,2014,1)) + scale_y_continuous(breaks=seq(0,25,5), labels = dollar_format(suffix = "%", prefix = "")) + expand_limits(y=c(0,25)) + labs(x="年",y="百分数")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g3 <- g3 + theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("进口中国的产品的结构") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular"))

g4 <- ggplot() + geom_bar(aes(y = porcentaje, x = anio, fill = producto), data = data4, stat="identity")
g4 <- g4 + scale_fill_manual(values=cbPalette3) + geom_text(data=data4, aes(x = anio, y = pos, label = paste0(porcentaje,"%")), colour="black", family="AdobeHeitiStd-Regular", size = 5)
g4 <- g4 + scale_x_continuous(breaks=seq(2006,2014,1)) + scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + labs(x="年", y="百分数")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g4 <- g4 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("出口中国结构 (%)") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular"))

g5 <- ggplot() + geom_bar(aes(y = expo, x = anio, fill = producto), data = data4, stat="identity")
g5 <- g5 + scale_fill_manual(values=cbPalette3) + geom_text(data=data4, aes(x = anio, y = pos2, label = expo), colour="black", family="AdobeHeitiStd-Regular", size = 4)
g5 <- g5 + scale_x_continuous(breaks=seq(2006,2014,1)) + labs(x="年", y="百万美元")  + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g5 <- g5 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("出口中国结构 ($)") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular"))

g6 <- ggplot() + geom_area(aes(y = expo, x = anio, fill = producto), data = data6, stat="identity") 
g6 <- g6 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,1600,400)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
#g6 <- g6 + scale_fill_manual(values=cbPalette)
g6 <- g6 + scale_fill_brewer(palette="Paired")
g6 <- g6 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("无铜矿或造纸木材出口中国") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular"))

g7 <- ggplot() + geom_line(aes(y = expocc, x = anio, colour = "expocc"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impocc, x = anio, colour = "impocc"), size=1.5, data = data, stat="identity") 
g7 <- g7 + ylab("百万美元") + xlab("年") + ylab("百分数") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g7 <- g7 + scale_color_manual(labels = c("出口的产品", "进口产品"), values = c("#4169e1", "#000080"))
g7 <- g7 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("商业智利-中国") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 2))

#range(data2$bccm)
g8 <- ggplot() + geom_line(aes(y = expoceeuu, x = anio, colour = "expoceeuu"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impoceeuu, x = anio, colour = "impoceeuu"), size=1.5, data = data, stat="identity") 
g8 <- g8 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g8 <- g8 + scale_color_manual(labels = c("出口的产品", "进口产品"), values = c("#a0522d", "#E18942"))
g8 <- g8 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("商业智利-美国") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 2))

g9 <- ggplot() + geom_line(aes(y = expocue, x = anio, colour = "expocue"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impocue, x = anio, colour = "impocue"), size=1.5, data = data, stat="identity") 
g9 <- g9 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g9 <- g9 + scale_color_manual(labels = c("出口的产品", "进口产品"), values = c("#556b2f", "#2f556b"))
g9 <- g9 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("商业智利-欧盟") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 2))

g10 <- ggplot() + geom_line(aes(y = bccc, x = anio, colour = "bccc"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccm, x = anio, colour = "bccm"), size=1.5, data = data2, stat="identity") 
g10 <- g10 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g10 <- g10 + scale_color_manual(labels = c("淨出口中国的产品", "淨出口人间的产品"), values = c("#4169e1", "#FF43A4"))
g10 <- g10 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("淨出口中国和人间") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 2))

#range(data2$bccm);range(data2$bccue);range(data2$bcceeuu)
g11 <- ggplot() + geom_line(aes(y = bcceeuu, x = anio, colour = "bcceeuu"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccm, x = anio, colour = "bccm"), size=1.5, data = data2, stat="identity") 
g11 <- g11 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(-10000,25000,5000)) + expand_limits(y=c(-10000,25000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g11 <- g11 + scale_color_manual(labels = c("淨出口美国的产品", "淨出口人间的产品"), values = c("#d68a59", "#FF43A4"))
g11 <- g11 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("淨出口美国和人间") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 2))

g12 <- ggplot() + geom_line(aes(y = bccue, x = anio, colour = "bccue"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccm, x = anio, colour = "bccm"), size=1.5, data = data2, stat="identity") 
g12 <- g12 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g12 <- g12 + scale_color_manual(labels = c("淨出口人间的产品", "淨出口欧盟的产品"), values = c("#FF43A4", "#556b2f"), guide = guide_legend(reverse=TRUE))
g12 <- g12 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("淨出口欧盟和人间") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 2))

g13 <- ggplot() + geom_line(aes(y = expocc, x = anio, colour = "expocc"), size=1.5, data = data, stat="identity") + geom_line(aes(y = expoceeuu, x = anio, colour = "expoceeuu"), size=1.5, data = data, stat="identity") + geom_line(aes(y = expocue, x = anio, colour = "expocue"), size=1.5, data = data, stat="identity") 
g13 <- g13 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(0,20000,5000)) + expand_limits(y=c(5000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g13 <- g13 + scale_color_manual(labels = c("中国(第一贸易合作伙伴)出口智利的产品", "美国(第二贸易合作伙伴)出口智利的产品", "欧盟(第三贸易合作伙伴)出口智利的产品"), values = c("#4169e1", "#d68a59","#556b2f"))
g13 <- g13 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("出口中国，美国和欧盟的产品") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 3))

#range(data$impocc);range(data$impoceeuu);range(data$impocue)
g14 <- ggplot() + geom_line(aes(y = impocc, x = anio, colour = "impocc"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impoceeuu, x = anio, colour = "impoceeuu"), size=1.5, data = data, stat="identity") + geom_line(aes(y = impocue, x = anio, colour = "impocue"), size=1.5, data = data, stat="identity") 
g14 <- g14 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(4000,20000,4000)) + expand_limits(y=c(4000,20000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g14 <- g14 + scale_color_manual(labels = c("中国(第一贸易合作伙伴)进口智利的产品", "美国(第二贸易合作伙伴)进口智利的产品", "欧盟(第三贸易合作伙伴)进口智利的产品"), values = c("#4169e1", "#d68a59","#556b2f"))
g14 <- g14 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("从中国，美国和欧盟进口产品") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 3))

#range(data2$bccc);range(data2$bcceeuu);range(data2$bccue)
g15 <- ggplot() + geom_line(aes(y = bccc, x = anio, colour = "bccc"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bcceeuu, x = anio, colour = "bcceeuu"), size=1.5, data = data2, stat="identity") + geom_line(aes(y = bccue, x = anio, colour = "bccue"), size=1.5, data = data2, stat="identity")
g15 <- g15 + ylab("百万美元") + xlab("年") + scale_x_continuous(breaks=seq(2000,2014,2)) + scale_y_continuous(breaks=seq(-10000,15000,5000)) + expand_limits(y=c(-10000,15000)) + theme(panel.background = element_rect(fill="white"), panel.grid = element_line(colour="white"))
g15 <- g15 + scale_color_manual(labels = c("中国(第一贸易合作伙伴)淨出口智利的产品", "美国(第二贸易合作伙伴)淨出口智利的产品", "欧盟(第三贸易合作伙伴)淨出口智利的产品"), values = c("#4169e1", "#d68a59","#556b2f"))
g15 <- g15 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.line = element_line(size=1.5)) + ggtitle("淨出口中国，美国和欧盟进口产品") + theme(plot.title=element_text(size=20, family="AdobeHeitiStd-Regular"), text=element_text(size=18, family="AdobeHeitiStd-Regular")) + guides(col = guide_legend(nrow = 3))


png("chile-china-mundo-cn.png",width=600*1,height=380*16) 
grid.arrange(
  top = textGrob("\n智利和中国以及世界的贸易\n自中智自由贸易协定签约10年来的分析\n作者帕夏 (推特 @pachamaltese)\n 消息灵通人士: 智利海关总署, 智利中央银行\n& 商務處智利中國\n",gp=gpar(fontsize=25,fontfamily="AdobeHeitiStd-Regular")),
  plot_grid(g1,g2,g3,
            g4,g5,g6,
            g7,g8,g9,
            g10,g11,g12,
            g13,g14,g15,
            nrow=15, ncol=1, align="v"),
  nrow=1,ncol=1
)
dev.off()
