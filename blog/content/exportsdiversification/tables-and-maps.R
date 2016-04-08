library(XLConnect)
library(googleVis)
setwd("/Users/pacha/pachamaltese.github.io/blog/content/exportsdiversification/")

file <- paste0(getwd(),"/agreements.xlsx")

data <- readWorksheetFromFile(file, sheet = "Sheet1", region = "D1:G26", header = TRUE)
table <- gvisTable(data)
plot(table)

data2 <- readWorksheetFromFile(file, sheet = "Sheet2", region = "A1:C65", header = TRUE)
map <- gvisGeoChart(data2, "country", "year", hovervar="text",
                     options=list(region="world", 
                                  #displayMode="regions", 
                                  #resolution="provinces",
                                  width=600, height=430))
plot(map)

data3 <- readWorksheetFromFile(file, sheet = "Sheet3", region = "A1:D72", header = TRUE)
tree <- gvisTreeMap(data3,  
                    "Region", "Parent", 
                    "Val", "Fac", 
                    options=list(fontSize=16))
plot(tree)

data4 <- readWorksheetFromFile(file, sheet = "Sheet4", region = "A1:E4", header = TRUE)
table2 <- gvisTable(data4, formats=list('x2014'="#,###", 'x2015'="#,###", 'pvar'='#.#%', 'usdvar'="#,###"))
plot(table2)
