# if (!require("remotes")) {
#   install.packages("remotes")
# }
#
# remotes::install_github("Yingjie4Science/SDGdetector")
# library(SDGdetector)
# citation(package = 'SDGdetector')

library(testthat)
test_check("SDGdetector")
??SDGdetector


## string as input data
x <- 'our goal is to mitigate climate change, end poverty, and reduce inequality globally'
SDGdetector(x)


## dataframe as input data
df <- data.frame(text = c('our goal is to end poverty globally',
                          'this product contributes to slowing down climate change',
                          'ensure access to updated information'))
SDGdetector(x = df, col = text)

### warning occur when the number of characters exceed 764
# x <- ''
# nchar(x)
# SDGdetector(x)


