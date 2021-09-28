# Roxygen documentation

#' Get the latest version from GitHub.
#'
#' Syncs your local library with our latest GTA GitHub release.
#'
#' @return Be up to date with our latest functions.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

dpa_update_library = function(x){
  devtools::install_github("global-trade-alert/dpaclassifier", force=T)
  library("dpaclassifier")
  print("You are up to date. Restart RStudio for updates to work fully.")
}
