### Social Progress Index

### Set working directory
setwd("~/documents/data_science_R/happy_country")

### Import data from local device
social <- read.csv("data/2017_SPI.csv")#select year: 2014-2017
#str(social)

### Preprocess
gsub("\\.", "_", colnames(social))
