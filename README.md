
# expressionAnalysis

## Description

`expressionAnalysis` is an R package aiming at identifying potential
biomarkers in a specific disease by analyizing gene expression data. It
aims at making the process of computing and visualising the data more
accessible and efficient. <br> <br> The `expressionAnalysis` package was
developed using `R version 4.2.1 (2022-06-23)`,
`Platform: x86_64-apple-darwin17.0` and
`Running under: macOS Ventura 13.0.1`.

## Installation

To install the latest version of the package:

``` r
require("devtools")
devtools::install_github("ari-beau/expressionAnalysis", build_vignettes = TRUE)
library("expressionAnalysis")
```

To run the Shiny app:

``` r
expressionAnalysis::runExpressionAnalysis()
```

## Overview

``` r
ls("package:expressionAnalysis")
data(package = "expressionAnalysis") 
browseVignettes("expressionAnalysis")
```

`expressionAnalysis` contains 8 functions to help analyze gene
expression data. The *loadData* function loads input data files in the
format required for functions in the package. The *exprNormalization*
function normalizes expression data, using different methods including
total count normalization, log2 transformation and standardization. The
*correlationPlot* function calculates the pairwise correlation between
genes, and produces a correlation plot to for visualization. The
*rankDEG* function uses different methods, including t-test and Wilcoxon
rank sum test, to rank differentially expressed genes between case
samples and control samples. The *exprPlot* function produces boxplots
to visualize differential gene expression. The *keggDiseaseGenes*
function retrieves genes associated with a specific Kyoto Encyclopedia
of Genes and Genomes (KEGG) disease entry. The *exprPCA* function
produces a principal component analysis plot of the expression data. The
*runExpressionAnalysis* function launchse the Shiny app for this
package.

The package also contains two datasets from a gene expression profiling
experiment in ovarian cancer, called *OVExpression* and *OVSample.*

An overview of the package is illustrated below.

![](./inst/extdata/image1.png)

## Contributions

The author of the package is Arianne Beauregard. The *loadData* function
reads in data from multiple file types to prepare the data for the rest
of the package. The *exprNormalization* function makes use of the
`dplyr` R package. The *correlationPlot* function uses the `corrplot` R
package to produce a pairwise correlation plot of genes. The *rankDEG*
function uses different methods to rank differential gene expression.
The *exprPlot* function uses the function *melt* from the `reshape` R
package to reshape the expression dataframe. It also uses the `ggplot2`
package to produce the boxplot. The *keggDiseaseGenes* function uses the
`KEGGREST` package as an interface to KEGG, and uses the `stringr`
package to manipulate strings. The *exprPCA* function uses the
`FactoMineR` package and the `factoextra` package to produce the PCA
plot. The `shiny` package was used to create the Shiny app. The
`assertthat` R package is used for checking for valid function inputs.

<br> <br> The datasets *OVSample* and *OVExpression* are from an ovarian
cancer gene expression profiling experiment from Bowen N.J. et
al. (2009).

## References

Bowen, N. J., Walker, L. D. E., Matyunina, L. V., Logani, S., Totten, K.
A., Benigno, B. B., & McDonald, J. F. (2009). Gene expression profiling
supports the hypothesis that human ovarian surface epithelia are
multipotent and capable of serving as ovarian cancer initiating cells.
*BMC Medical Genomics*, 2(1). <https://doi.org/10.1186/1755-8794-2-71>

Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
McPherson J, Dipert A, Borges B (2022). *shiny: Web Application
Framework for R*. R package version 1.7.3,
<https://CRAN.R-project.org/package=shiny>.

Kassambara A, Mundt F (2020). *factoextra: Extract and Visualize the
Results of Multivariate Data Analyses*. R package version 1.0.7,
<https://CRAN.R-project.org/package=factoextra>.

R Core Team (2022). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria. URL
<https://www.R-project.org/>.

Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R
Package for Multivariate Analysis. Journal of Statistical Software,
25(1), 1-18. 10.18637/jss.v025.i01

Tenenbaum D, Maintainer B (2022). *KEGGREST: Client-side REST access to
the Kyoto Encyclopedia of Genes and Genomes (KEGG)*. R package version
1.36.3.

Wei T, Simko V (2021). R package ‘corrplot’: Visualization of a
Correlation Matrix. (Version 0.92),
<https://github.com/taiyun/corrplot>.

Wickham H (2022). *stringr: Simple, Consistent Wrappers for Common
String Operations*. R packageversion 1.5.0,
<https://CRAN.R-project.org/package=stringr>.

Wickham H, François R, Henry L, Müller K (2022). dplyr: A Grammar of
Data Manipulation. <https://dplyr.tidyverse.org>,
<https://github.com/tidyverse/dplyr>.

Wickham H (2019). *assertthat: Easy Pre and Post Assertions*. R package
version 0.2.1, <https://CRAN.R-project.org/package=assertthat>.

Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
Springer-Verlag New York. ISBN 978-3-319-24277-4,
<https://ggplot2.tidyverse.org>.

Wickham H (2007). “Reshaping data with the reshape package.” Journal of
Statistical Software, 21(12). <https://www.jstatsoft.org/v21/i12/>.

## Acknowledgements

This package was developed as part of an assessment for 2022 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. `expressionAnalysis` welcomes issues, enhancement requests, and
other contributions. To submit an issue, use the [GitHub
issues](https://github.com/ari-beau/expressionAnalysis/issues).
