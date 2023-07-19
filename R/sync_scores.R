
#' Sync scores from remote server to a local directory
#' @param dest local directory to cache scores
#' @export
sync_scores <- function(dest = "cache/") {
  
  minioclient::install_mc()
  minioclient::mc_alias_set("efi",  endpoint="data.ecoforecast.org",
               access_key = "", secret_key = "")
  cmd <- paste("mirror --overwrite efi/neon4cast-scores", dest)
  minioclient::mc(cmd)

}


