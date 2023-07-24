#' Unix fortune
#'
#' This function prints a random fortune from the fortunes package.
#'
#' @param include_obscene Whether to include obscene fortunes, TRUE or FALSE
#' @return Invisible NULL
#' @importFrom stringr str_replace_all
#' @export
fortune <- function(include_obscene = FALSE) {
  if (!include_obscene) {
    pattern <- "^[^-].*[^o]$"
  } else {
    pattern <- ".*"
  }

  fortunes_path <- system.file("fortunes",
    package = "fortunes"
  )

  fortune_files <- list.files(
    path = fortunes_path,
    pattern = pattern,
    recursive = FALSE,
    full.names = FALSE
  )

  chosen_file <- sample(fortune_files, 1)

  fortunes <- scan(paste(fortunes_path, chosen_file, sep = "/"),
    what = "list",
    sep = "\n",
    quiet = TRUE,
    multi.line = TRUE,
    blank.lines.skip = FALSE
  )

  fortune_boundaries <- which(fortunes == "%")
  chosen_boundary <- sample(seq_along(fortune_boundaries), 1) + 1
  fortune_boundaries <- c(1, fortune_boundaries)
  chosen_fortune <- fortunes[(fortune_boundaries[chosen_boundary - 1] + 1):
  (fortune_boundaries[chosen_boundary] - 1)]
  if (endsWith(chosen_file, "-o")) {
    chosen_fortune <- .replace_non_escaped(chosen_fortune)
  }
  cat(paste0(chosen_fortune, "\n", collapse = "\n"))
  return(invisible())
}

.rot13 <- function(x) {
  chartr(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM",
    x
  )
}

.replace_non_escaped <- function(string) {
  pattern <- "(?<!\\\\)[a-zA-Z]"
  return(stringr::str_replace_all(string, pattern, function(x) .rot13(x)))
}
