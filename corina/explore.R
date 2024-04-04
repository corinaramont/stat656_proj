# Description: explore the data for missingness, what's there, etc using
# the cleaned dataset provided by Dylan 4/4/2024
# -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

data = read.csv("datasets/CleanedProsperData.csv")

ggplot_missing = function(x){
  # this function is from Dr. Homrighausen
  if(!require(reshape2)){warning('you need to install reshape2')} 
  require(reshape2)
  require(ggplot2)
  # This function produces a plot of the missing data pattern
  # in x. It is a modified version of a function in the 'neato' package
  x %>%
    is.na %>%
    melt %>% ggplot(data = .,
                    aes(x = Var2,
                        y = Var1)) +
    geom_raster(aes(fill = value)) + 
    scale_fill_grey(name = "", labels = c("Present","Missing")) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
} 
ggplot_missing(data)