# findSDGs

In 2015, leaders worldwide adopted 17 Sustainable Development Goals (SDGs) with 169 targets to be achieved by 2030 (https://sdgs.un.org). The framework of SDGs serves as a blueprint for shared prosperity for both people and the earth. `{findSDGs}` identifies both direct and indirect expressions of SDGs and associated targets in chunks of text. It takes a data frame with a specified column of text to process as inputs, and outputs a data frame with original columns plus matched SDGs and targets.

<p align="center">
  <img src="/data/SDG_detector.png" width="300" height="300"/>
</p>

## Installation

You can install `{findSDGs}` from GitHub with:

    #install.packages("devtools")
    devtools::install_github("caimeng2/findSDGs")
    
## Example usage

    library(findSDGs)
    my_text <- data.frame(my_col=c('our goal is to end poverty globally', 'this product contributes to slowing down climate change'))
    findSDGs(my_text, my_col)
