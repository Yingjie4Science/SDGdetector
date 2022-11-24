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
x <- 'our goal is to mitigate climate change, end poverty, and reducing inequality globally'
SDGdetector(x)


## dataframe as input data
my_text <- data.frame(my_col=c('our goal is to end poverty globally',
                               'this product contributes to slowing down climate change',
                               'ensure access to updated information'))
SDGdetector(x = my_text, col = my_col)


