# s3-archive.R — shared S3 archive helpers for native-resilience data repos.
# Vendored from sustainable-fsa (usdm/R/s3-archive.R). Differences: the
# CloudFront base URL and distribution default to the native-resilience
# archive (overridable via CLOUDFRONT_BASE / CLOUDFRONT_DISTRIBUTION_ID), and
# s3_encode_key() also percent-encodes non-ASCII characters (projections has
# keys like "Hawai‘i_Climate_Divisions_—_Kona.zip"). All functions shell out to the AWS CLI v2 via
# processx and stop() loudly on failure — no silent tryCatch.
#
# Auth: under GitHub Actions (OIDC via aws-actions/configure-aws-credentials),
# ambient env credentials are used. Locally, AWS_PROFILE defaults to "mco".

s3_profile_args <- function() {
  profile <- Sys.getenv("AWS_PROFILE",
                        unset = if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) "" else "mco")
  if (nzchar(profile)) c("--profile", profile) else character(0)
}

s3_run <- function(args, echo = TRUE) {
  processx::run("aws", c(args, s3_profile_args()), echo = echo)
}

s3_preflight <- function() {
  sts <- processx::run("aws",
                       c("sts", "get-caller-identity", s3_profile_args()),
                       error_on_status = FALSE)
  if (sts$status != 0)
    stop("AWS credentials unavailable. Locally: aws sso login --profile ",
         Sys.getenv("AWS_PROFILE", unset = "mco"),
         "; in CI check the OIDC role assumption step.")
  invisible(TRUE)
}

# List all keys under a prefix. Returns a tibble with Key and Size
# (zero rows if the prefix is empty). CLI v2 auto-paginates.
s3_list_keys <- function(bucket, prefix) {
  out <- s3_run(c("s3api", "list-objects-v2",
                  "--bucket", bucket,
                  "--prefix", paste0(prefix, "/"),
                  "--output", "json"),
                echo = FALSE)$stdout
  parsed <- jsonlite::fromJSON(out)
  if (is.null(parsed$Contents))
    return(tibble::tibble(Key = character(0), Size = numeric(0)))
  tibble::as_tibble(parsed$Contents)[, c("Key", "Size")]
}

# Pull prior archive state down before an incremental run (pattern P2).
s3_pull <- function(bucket, prefix, local_dir = prefix) {
  s3_run(c("s3", "sync",
           paste0("s3://", bucket, "/", prefix, "/"),
           paste0(local_dir, "/")))
  invisible(TRUE)
}

# Push a local staging dir to its prefix.
#   delete = TRUE  -> exact mirror (patterns P1/P2: local dir holds the FULL archive)
#   delete = FALSE -> append-only (pattern P3: local dir holds only new files)
s3_push <- function(bucket, prefix, local_dir = prefix, delete = TRUE,
                    excludes = c("*.DS_Store", ".Rproj.user/*", ".Rhistory",
                                 "_manifest.txt")) {
  s3_run(c("s3", "sync",
           paste0(local_dir, "/"),
           paste0("s3://", bucket, "/", prefix, "/"),
           if (delete) "--delete",
           unlist(lapply(excludes, function(x) c("--exclude", x))),
           "--no-progress"))
  invisible(TRUE)
}

s3_put <- function(bucket, key, file,
                   content_type = "application/octet-stream",
                   cache_control = "max-age=86400") {
  s3_run(c("s3", "cp", file, paste0("s3://", bucket, "/", key),
           "--content-type", content_type,
           "--cache-control", cache_control))
  invisible(TRUE)
}

# Exact-match verification for mirrored prefixes (P1/P2): every local file
# must exist remotely with the same size, and (if delete-mirroring) no
# unexpected remote keys. `allow_extra` lists remote keys that are permitted
# beyond the local set (e.g. "<prefix>/_manifest.txt").
s3_verify <- function(bucket, prefix, local_dir = prefix,
                      allow_extra = paste0(prefix, "/_manifest.txt"),
                      expect_exact = TRUE) {
  remote <- s3_list_keys(bucket, prefix)
  local <- tibble::tibble(
    path = list.files(local_dir, recursive = TRUE, full.names = TRUE)
  )
  local$Key  <- file.path(prefix, sub(paste0("^", local_dir, "/"), "", local$path))
  local$Size <- file.size(local$path)

  missing <- setdiff(local$Key, remote$Key)
  extra   <- setdiff(remote$Key, c(local$Key, allow_extra))
  merged  <- merge(local, remote, by = "Key", suffixes = c(".local", ".s3"))
  mismatch <- merged[merged$Size.local != merged$Size.s3, ]

  problems <- c(
    if (length(missing) > 0) paste0(length(missing), " missing remotely"),
    if (expect_exact && length(extra) > 0) paste0(length(extra), " unexpected remote keys"),
    if (nrow(mismatch) > 0) paste0(nrow(mismatch), " size mismatches")
  )
  if (length(problems) > 0) {
    print(list(missing = missing, extra = extra, mismatch = mismatch$Key))
    stop("S3 verification FAILED for ", prefix, ": ",
         paste(problems, collapse = "; "))
  }
  message("S3 verified: ", nrow(local), " files match under s3://",
          bucket, "/", prefix, "/")
  invisible(TRUE)
}

# Membership verification for append-only prefixes (P3): every uploaded file
# must be present remotely at the right size; extra remote keys are expected.
s3_verify_subset <- function(bucket, prefix, local_dir, subdir_prefix = prefix) {
  s3_verify(bucket, prefix, local_dir,
            allow_extra = character(0), expect_exact = FALSE)
}

# URL-encode an S3 key for the CloudFront manifest. Order matters: encode
# literal "%" first, then spaces. "=" and "/" stay literal — DuckDB's
# hive_partitioning detection parses key=value from the raw URL path.
s3_encode_key <- function(x) {
  x <- gsub("%", "%25", x, fixed = TRUE)
  x <- gsub(" ", "%20", x, fixed = TRUE)
  x <- gsub("#", "%23", x, fixed = TRUE)
  x <- gsub("?", "%3F", x, fixed = TRUE)
  x <- gsub("+", "%2B", x, fixed = TRUE)
  vapply(enc2utf8(x), function(s) {
    ch <- strsplit(s, "")[[1]]
    hi <- !grepl("^[ -~]$", ch, perl = TRUE)
    ch[hi] <- vapply(ch[hi], function(c)
      paste0("%", toupper(as.character(charToRaw(c))), collapse = ""), "")
    paste(ch, collapse = "")
  }, "", USE.NAMES = FALSE)
}

# Regenerate the prefix's _manifest.txt from the VERIFIED remote listing.
# Skips internal keys (underscore-prefixed path segments, e.g. _cache/).
s3_write_manifest <- function(bucket, prefix,
                              base = Sys.getenv("CLOUDFRONT_BASE",
                                                unset = "https://data.native-resilience.com")) {
  keys <- s3_list_keys(bucket, prefix)$Key
  keys <- keys[!grepl("(^|/)_", sub(paste0("^", prefix, "/"), "", keys))]
  manifest_file <- file.path(tempdir(), "_manifest.txt")
  writeLines(paste0(base, "/", s3_encode_key(sort(keys))), manifest_file)
  s3_put(bucket, paste0(prefix, "/_manifest.txt"), manifest_file,
         content_type = "text/plain", cache_control = "max-age=86400")
  invisible(TRUE)
}

# Invalidate only mutated paths (immutable new weekly files never need it).
cf_invalidate <- function(paths,
                          distribution = Sys.getenv("CLOUDFRONT_DISTRIBUTION_ID",
                                                    unset = "E2O1CP3LOBQEN8")) {
  if (length(paths) == 0) return(invisible(FALSE))
  s3_run(c("cloudfront", "create-invalidation",
           "--distribution-id", distribution,
           "--paths", s3_encode_key(paths)))
  invisible(TRUE)
}

# HTTP existence probe for cross-archive freshness gates. HEAD via curl;
# retries transient failures, TRUE only on a final 200. The tryCatch guards
# the probe itself, not archive work — a FALSE gates a skip, never a write.
url_exists <- function(url, tries = 3L, pause = 5) {
  for (i in seq_len(tries)) {
    res <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(nobody = TRUE)),
      error = function(e) NULL)
    if (!is.null(res) && res$status_code == 200L) return(TRUE)
    if (i < tries) Sys.sleep(pause)
  }
  FALSE
}

# Freshness-gate skip message. Dispatched runs are expected to find fresh
# upstream data, so a skip there surfaces as a warning annotation; scheduled
# and push runs no-op quietly with a notice.
gate_skip <- function(msg) {
  if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
    level <- if (identical(Sys.getenv("GITHUB_EVENT_NAME"), "workflow_dispatch"))
      "warning" else "notice"
    cat(sprintf("::%s::%s\n", level, msg))
  }
  message(msg)
}

# Wait for the CloudFront copy of the archive manifest to match the freshly
# uploaded local copy before rendering the README from it (invalidations
# usually land in seconds). Gives up after tries*pause seconds and proceeds:
# a stale render reproduces the prior README.md byte-for-byte, so the
# commit-back guard simply skips.
cf_wait_manifest <- function(url, local, tries = 18L, pause = 10) {
  target <- tryCatch(jsonlite::fromJSON(local), error = function(e) NULL)
  if (is.null(target)) return(invisible(FALSE))
  for (i in seq_len(tries)) {
    remote <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    if (identical(remote, target)) return(invisible(TRUE))
    if (i < tries) Sys.sleep(pause)
  }
  gate_skip(paste0("CloudFront manifest at ", url, " still stale after ",
                   tries * pause, "s; README render may reflect the prior week."))
  invisible(FALSE)
}
