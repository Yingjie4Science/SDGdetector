[![DOI](https://zenodo.org/badge/431620191.svg)](https://zenodo.org/badge/latestdoi/431620191)

<p align="center">
  <img src="/docs/images/SDG_detector.png" width="300" height="300"/>
</p>

# SDG detector

In 2015, leaders worldwide adopted 17 Sustainable Development Goals (SDGs) with 169 targets to be achieved by 2030 (https://sdgs.un.org). The framework of SDGs serves as a blueprint for shared prosperity for both people and the earth. `SDGdetector` identifies both direct and indirect expressions of SDGs and associated targets in chunks of text. It takes a data frame with a specified column of text to process as inputs, and outputs a data frame with original columns plus matched SDGs and targets.

## Installation

You can install `SDGdetector` from GitHub as follows:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}

remotes::install_github("Yingjie4Science/SDGdetector")
```    
    
## Example usage

    library(SDGdetector)
    my_text <- data.frame(my_col=c('our goal is to end poverty globally', 'this product contributes to slowing down climate change'))
    SDGdetector(my_text, my_col)



## Accuracy Evaluation

This package has achieved high accuracy in detecting SDG-related statements within textual data (> 75%, measured by the alignment between the R package results and four experts' manually-coded results; see this [supplement doc](https://docs.google.com/document/d/1mEjlyu17JZUIphL4VeVrGr4txKBu5jJObzRtJO7G6dg/edit?usp=sharing) for more information.

<p align="center">
  <img src="/docs/images/Inspection_Accuracy.png" height="300"/>
</p>


## License

The SDGdetector **R** package is distributed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html).


## How to cite

Get citation information for `SDGdetector` in R doing
    `citation(package = 'SDGdetector')`


## Reporting Bugs

*SDGdetector* is distributed as is and without warranty of suitability for application. If you encounter flaws with the software (i.e. bugs) please report the issue. Providing a detailed description of the conditions under which the bug occurred will help to identify the bug. *Use the [Issues tracker](https://github.com/Yingjie4Science/SDGdetector/issues) on GitHub to report issues with the software and to request feature enhancements.* 

