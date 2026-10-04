# projections-publish.R — publish the projections archive to S3.
#
# Mirrors upload/ (633 per-land zips + pdf/) and native-land.parquet to
# s3://native-resilience/projections/, served at
# https://data.native-resilience.com/projections/. Run locally after
# projections.R has rebuilt upload/:
#
#   aws sso login --profile mco
#   Rscript projections-publish.R
#
# Re-runs are cheap: `aws s3 sync` skips files whose size and mtime match.
# Sizes in upload/ were verified byte-for-byte against the original
# data.climate.umt.edu/native-climate/projections/ host on 2026-10-04.

source("R/s3-archive.R")
s3_preflight()

s3_bucket_name <- Sys.getenv("S3_BUCKET", unset = "native-resilience")
s3_prefix      <- Sys.getenv("S3_PREFIX", unset = "projections")
staging        <- "upload"

# ---- Mirror upload/ (zips at the prefix root, PDFs under pdf/) ----
# delete = FALSE: the prefix also holds native-land.parquet and the
# manifests, which are not in upload/.
s3_push(s3_bucket_name, s3_prefix, staging, delete = FALSE)

s3_put(s3_bucket_name,
       paste0(s3_prefix, "/native-land.parquet"),
       "native-land.parquet",
       content_type = "application/vnd.apache.parquet",
       cache_control = "max-age=3600")

s3_verify(s3_bucket_name, s3_prefix, staging,
          allow_extra = paste0(s3_prefix, c("/_manifest.txt",
                                            "/native-land.parquet",
                                            "/projections-manifest.json")))

# ---- projections-manifest.json (path, size, sha256) ----
# sha256 of ~114 GB takes a while, so hashes are cached by path + size + mtime.
hash_cache_file <- ".sha256-cache.tsv"
local_files <- c(list.files(staging, recursive = TRUE, full.names = TRUE),
                 "native-land.parquet")
local_files <- local_files[basename(local_files) != ".DS_Store"]
info <- file.info(local_files)
current <- data.frame(file  = local_files,
                      size  = info$size,
                      mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%OS3"),
                      stringsAsFactors = FALSE)

cache <- if (file.exists(hash_cache_file))
  utils::read.delim(hash_cache_file, colClasses = "character", encoding = "UTF-8") else
  data.frame(file = character(), size = character(), mtime = character(),
             hash = character())
current$key <- paste(current$file, current$size, current$mtime)
cache$key   <- paste(cache$file, cache$size, cache$mtime)
current$hash <- cache$hash[match(current$key, cache$key)]

todo <- which(is.na(current$hash))
if (length(todo) > 0) {
  message("Hashing ", length(todo), " files...")
  current$hash[todo] <- unlist(parallel::mclapply(
    current$file[todo],
    function(f) digest::digest(file = f, algo = "sha256"),
    mc.cores = max(1L, parallel::detectCores() - 2L)))
}
utils::write.table(current[, c("file", "size", "mtime", "hash")], hash_cache_file,
                   sep = "\t", row.names = FALSE, quote = FALSE,
                   fileEncoding = "UTF-8")

current$path <- ifelse(startsWith(current$file, paste0(staging, "/")),
                       substring(current$file, nchar(staging) + 2),
                       current$file)
hashes <- stats::setNames(as.list(current$hash), current$path)

generate_tree_flat <- function(output_file = "projections-manifest.json") {
  remote <- s3_list_keys(s3_bucket_name, s3_prefix)
  remote$path <- sub(paste0("^", s3_prefix, "/"), "", remote$Key)
  remote <- remote[!startsWith(remote$path, "_") &
                     remote$path != output_file, ]
  remote <- remote[order(remote$path), ]
  entries <- lapply(seq_len(nrow(remote)), function(i) {
    entry <- list(path = remote$path[i], size = remote$Size[i])
    if (!is.null(hashes[[remote$path[i]]])) entry$hash <- hashes[[remote$path[i]]]
    entry
  })
  jsonlite::write_json(entries, output_file, pretty = TRUE, auto_unbox = TRUE)
  message("Wrote ", length(entries), " entries to ", output_file)
}

manifest_file <- file.path(tempdir(), "projections-manifest.json")
generate_tree_flat(manifest_file)

s3_put(s3_bucket_name,
       paste0(s3_prefix, "/projections-manifest.json"),
       manifest_file,
       content_type = "application/json",
       cache_control = "max-age=3600")

s3_write_manifest(s3_bucket_name, s3_prefix)

cf_invalidate(paste0("/", s3_prefix, c("/projections-manifest.json",
                                       "/_manifest.txt",
                                       "/native-land.parquet")))
