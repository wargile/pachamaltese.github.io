library(XLConnect)
library(rCharts)

setwd("/Users/pacha/pachamaltese.github.io/blog/content/coppercomposition/")
file <- paste0(getwd(),"/coppercomposition.xlsx")
data <- readWorksheetFromFile(file, sheet = "Sheet1", region = "A29:D45", header = TRUE)

h <- hPlot(QUANTITY.ROUND ~ YEAR, data = data, type = c('column', 'line'), group = 'CAT')
h$show('inline')
