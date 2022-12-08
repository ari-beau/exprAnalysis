#' Gets Genes Associated with KEGG Disease Entry
#'
#' A function that returns the genes associated with a KEGG disease entry, using
#' the function keggGET from the KEGGREST package. This function can be used to
#' get a list of genes of interest to include in the plot in the exprPlot function.
#'
#' @param entryID A string of the KEGG disease entry ID
#'
#' @return Returns a character vector containing genes from the inputted entry
#'
#' @examples
#' # Example:
#' # Getting genes for KEGG entry for Ovarian cancer (H00027)
#' keggDiseaseGenes(entryID = "H00027")
#'
#' @export
#' @importFrom KEGGREST keggGet
#' @importFrom stringr word
#'
#' @references
#' Tenenbaum D, Maintainer B (2022). _KEGGREST: Client-side REST access to the
#' Kyoto Encyclopedia of Genes and Genomes (KEGG)_. R package version 1.36.3.
#'
#' Wickham H (2022). _stringr: Simple, Consistent Wrappers for Common String
#' Operations_. R packageversion 1.5.0, <https://CRAN.R-project.org/package=stringr>.

keggDiseaseGenes <- function(entryID) {
  # use keggGet to entry
  disease <- tryCatch({
    KEGGREST::keggGet(entryID)
    },
    error=function(cond) {
      stop("Invalid entryID, please ensure it is a valid KEGG disease entry ID.")
    }
  )

  # tell user which KEGG entry
  message(paste("KEGG Disease Entry ", disease[[1]][["ENTRY"]], ": ", disease[[1]][["NAME"]], sep = ""))

  # get vector of genes
  # get first word only because first word is gene name
  genes <- stringr::word(disease[[1]][["GENE"]])

  return(genes)
}


# [END]
