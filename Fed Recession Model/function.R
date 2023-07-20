library(readxl)

import_data <- function(){
  data <- read_excel("allmonth.xls")
  data <- na.omit(data)
  data
}

