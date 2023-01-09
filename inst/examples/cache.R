
# remotes::install_github("cboettig/minio")
minio::install_mc()
minio::mc_alias_set("efi",  endpoint="data.ecoforecast.org",
             access_key = "", secret_key = "")
minio::mc("mirror --overwrite efi/neon4cast-scores cache/")


