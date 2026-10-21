# projections-publish.R — publish the projections archive to S3.
#
# Mirrors upload/ (per-land zips + pdf/), native-land.parquet and the data/
# prefix (per-land and by-variable parquet, lookup tables) to
# s3://native-resilience/projections/, served at
# https://data.native-resilience.com/projections/. Run after projections.R:
#
#   aws sso login --profile mco
#   Rscript projections-publish.R
#
# Two sources for the zips and PDFs:
#   default                       mirror the local upload/ folder
#   NR_PUBLISH_FROM=projections/_staging
#                                 copy a staged archive (written by an EC2 run
#                                 of projections.R) over the published one,
#                                 S3-to-S3; nothing is downloaded
# Re-runs are cheap: `aws s3 sync` skips files whose size and mtime match.
# Published zips/PDFs that no current land produces are reported; with
# NR_DELETE_ORPHANS=1 they are deleted (bucket versioning keeps a copy).

source("R/setup.R")
s3_preflight()

staging <- DIR_UPLOAD
publish_from <- Sys.getenv("NR_PUBLISH_FROM", unset = "")

if (nzchar(publish_from)) {
  # ---- Copy the staged archive over the published one (S3-to-S3) ----
  s3_run(c("s3", "sync",
           paste0("s3://", s3_bucket_name, "/", publish_from, "/"),
           paste0("s3://", s3_bucket_name, "/", s3_prefix, "/"),
           "--no-progress"))
  staged <- s3_list_keys(s3_bucket_name, publish_from)
  staged$Key <- sub(paste0("^", publish_from), s3_prefix, staged$Key)
  published <- s3_list_keys(s3_bucket_name, s3_prefix)
  check <- merge(staged, published, by = "Key", all.x = TRUE,
                 suffixes = c(".staged", ".published"))
  bad <- check$Key[is.na(check$Size.published) |
                     check$Size.published != check$Size.staged]
  if (length(bad))
    stop(length(bad), " staged file(s) missing or different after copy, e.g. ",
         paste(head(bad, 5), collapse = ", "), call. = FALSE)
  message("Published ", nrow(staged), " staged files from s3://",
          s3_bucket_name, "/", publish_from, "/")
} else {
  # ---- Mirror upload/ (zips at the prefix root, PDFs under pdf/) ----
  # delete = FALSE: the prefix also holds data/, native-land.parquet and the
  # manifests, which are not in upload/.
  s3_push(s3_bucket_name, s3_prefix, staging, delete = FALSE)
  s3_verify(s3_bucket_name, s3_prefix, staging,
            allow_extra = character(0), expect_exact = FALSE)
}

s3_put(s3_bucket_name,
       paste0(s3_prefix, "/native-land.parquet"),
       "native-land.parquet",
       content_type = "application/vnd.apache.parquet",
       cache_control = "max-age=3600")

# ---- data/ (dashboard parquet) ----
# projections.R pushes these as it goes; this is the full mirror + check.
for (d in c("annual", "smoothed", "annual-by-variable", "smoothed-by-variable")) {
  local <- file.path(DIR_DATA, d)
  if (!dir.exists(local)) next
  s3_push(s3_bucket_name, file.path(s3_prefix, "data", d), local, delete = FALSE)
  s3_verify(s3_bucket_name, file.path(s3_prefix, "data", d), local,
            allow_extra = character(0), expect_exact = FALSE)
}
for (f in c("variables.parquet", "lands.parquet"))
  if (file.exists(file.path(DIR_DATA, f)))
    s3_put(s3_bucket_name, file.path(s3_prefix, "data", f),
           file.path(DIR_DATA, f),
           content_type = "application/vnd.apache.parquet",
           cache_control = "max-age=3600")

# ---- Orphans: published zips/PDFs that no current land produces ----
land_names <- sf::st_drop_geometry(nr_read_lands())
land_names <- land_names$name[land_names$modeled]
expected <- c(paste0(s3_prefix, "/", gsub(" ", "_", land_names), ".zip"),
              paste0(s3_prefix, "/pdf/", land_names, "_CMIP6-climate-projections.pdf"))
remote <- s3_list_keys(s3_bucket_name, s3_prefix)$Key
orphans <- remote[grepl(paste0("^", s3_prefix, "/(pdf/)?[^/]+\\.(zip|pdf)$"), remote) &
                    !remote %in% expected]
if (length(orphans)) {
  delete <- Sys.getenv("NR_DELETE_ORPHANS", unset = "0") == "1"
  message(length(orphans), " published file(s) no longer produced",
          if (delete) " (deleting):\n" else " (not deleted):\n",
          paste(orphans, collapse = "\n"))
  if (delete)
    for (k in orphans) s3_run(c("s3", "rm", paste0("s3://", s3_bucket_name, "/", k)))
}

# ---- projections-manifest.json (path, size, sha256) ----
# sha256 of ~114 GB takes a while, so hashes are cached by path + size + mtime.
# Hashes come from local files only; when publishing from S3 staging the
# zips and PDFs are listed with their size alone.
hash_cache_file <- ".sha256-cache.tsv"
local_files <- c(if (!nzchar(publish_from))
                   list.files(staging, recursive = TRUE, full.names = TRUE),
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

# Zips, PDFs and data files are overwritten in place on a rebuild, so drop
# the whole prefix from the CDN (one wildcard path costs one invalidation).
cf_invalidate(paste0("/", s3_prefix, "/*"))
