#' Gene Expression from an Ovarian Cancer Experiment
#'
#' Data from ovarian cancer gene expression profiling of 24 samples (12 normal
#' human ovaries and 12 ovarian cancer epithelial cells). Only a subset of genes
#' (10 genes) were kept for the example expression data in this package.
#'
#' @source NCBI Gene Expression Omnibus (Accession
#' \href{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE14407}{GSE14407})
#'
#' @format A matrix with rows as genes, and columns as samples. The values are
#' gene expressions for corresponding genes in corresponding samples.
#' @examples
#' \dontrun{
#'  OVExpression
#' }
#' @references
#' Bowen NJ, Walker LD, Matyunina LV, Logani S et al. Gene expression profiling
#' supports the hypothesis that human ovarian surface epithelia are multipotent
#' and capable of serving as ovarian cancer initiating cells. BMC Med Genomics
#' 2009 Dec 29;2:71. PMID: \href{https://pubmed.ncbi.nlm.nih.gov/20040092/}{20040092}
#'
"OVExpression"

#' Sample Information from an ovarian cancer experiment
#'
#' Sample information from ovarian cancer gene expression profiling of
#' 24 samples (12 normal human ovaries and 12 ovarian cancer epithelial cells).
#'
#' @source NCBI Gene Expression Omnibus (Accession
#' \href{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE14407}{GSE14407})
#'
#' @format A matrix with rows as samples, and one column called "type" that
#' identifies if each sample is from a normal human ovary or from an ovarian
#' cancer epithelial cell.
#' @examples
#' \dontrun{
#'  OVSample
#' }
#' @references
#' Bowen NJ, Walker LD, Matyunina LV, Logani S et al. Gene expression profiling
#' supports the hypothesis that human ovarian surface epithelia are multipotent
#' and capable of serving as ovarian cancer initiating cells. BMC Med Genomics
#' 2009 Dec 29;2:71. PMID: \href{https://pubmed.ncbi.nlm.nih.gov/20040092/}{20040092}
#'
"OVSample"

# [END]
