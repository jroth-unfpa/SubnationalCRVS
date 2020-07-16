# The `SubnationalCRVS` R Package

### Package created by: Jeremy Roth

### Package last updated: 20 July 2020

### Package license: [CC BY-SA 3.0 IGO](https://creativecommons.org/licenses/by-sa/3.0/)

## Installation
We can install `SubnationalCRVS` using the R code shown below. Since `SubnationalCRVS` is hosted on GitHub instead of CRAN, it cannot be installed with the usual `install.packages()` function. Instead, `SubnationalCRVS` can be installed with the `install_github()` function from the `devtools` package. The key dependency `DemoTools` [Riffe et al. 2019] is also hosted on GitHub instead of CRAN and can also be installed with `install_github()`.

```r
install.packages("devtools")
library(devtools)
install_github("timriffe/DemoTools") # install the DemoTools dependency 
install_github("ConVERGE-UNFPA/SubnationalCRVS") # install the SubnationalCRVS package
```
## Description
The `SubnationalCRVS` R package produces visualizations of data quality (age ratios, sex ratios, age-heaping indices) separately within subnational levels (if provided) based on a dataset that reports population estimates from two time points disaggregated by sex and age. Although subnational levels often represent distinct geographic regions (e.g. provinces/states/regions), they may also represent other characteristics that vary within countries (e.g. income levels or education levels). `SubnationalCRVS` also produces national-level visualizations if sex- and age-disaggregated population counts are provided in the dataset at the national level (either in addition to or instead of at the subnational level).

If the two time points in the dataset represent consecutive Censuses that provide the population estimates, age is coded in five-year categories, and the dataset includes the number of registered deaths in the inercensal period, `SubnationalCRVS` also provides a convenient wrapper to the `DDM` package [Riffe, Lima, Quieroz, 2017] to estimate death registration completeness between the two Census years within the included levels of subnational disaggregation. `SubnationalCRVS` also provides visualizations of the DDM estimates of completeness -- using the GGB, SEG, GGB-SEG methods [Moultrie et al. 2013] -- to give a sense of subnational variation and the sensitivity of estimation to the choice of age range during the underlying fitting procedure. 

## Key Dependencies
Riffe T, Lima E, Queiroz B (2017). **DDM**: Death registration coverage estimation. URL: https://cran.r-project.org/package=DDM

Riffe T, Aburto JM, Alexander M, Fennell S, Kashnitsky I, Pascariu M and Gerland P (2019). **DemoTools**: An R package of tools for aggregate demographic analysis URL: https://github.com/timriffe/DemoTools/.

Wickham H,  François R, Henry L, Müller K (2020). **dplyr**: A grammar of data manipulation. URL: https://cran.r-project.org/package=dplyr

Wickham H,  Chang W, Henry L, Pedersen TL, Takahashi K, Wilke C, Woo K, Yutani H, Dunnington D (2020). **ggplot2**: Create elegant data visualizations using the grammar of graphics. URL: https://cran.r-project.org/package=ggplot2


## Other References
Moultrie TA, Dorrington RE, Hill AG, Hill K, Timæus IM, and Zaba B. (2013). Tools for demographic estimation. International Union for the Scientific Study of Population.
URL: http://demographicestimation.iussp.org/content/get-pdf-book-website