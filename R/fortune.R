fortune <- function() {
    files <- list.files(
        path = "inst",
        pattern = "^[^-].*[^o]$",
        recursive = FALSE,
        full.names = FALSE
    )

    chosen_file <- sample(files, 1)

    fortunes <- scan(paste("inst", chosen_file, sep = "/"),
        what = "list",
        sep = "\n",
        quiet = TRUE,
        multi.line = TRUE,
        blank.lines.skip = FALSE
    )

    fortune_boundaries <- which(fortunes == "%")
    chosen_boundary <- sample(seq_along(fortune_boundaries), 1)
    fortune_boundaries <- c(1, fortune_boundaries)
    chosen_fortune <- fortunes[(fortune_boundaries[chosen_boundary - 1] + 1):
    (fortune_boundaries[chosen_boundary] - 1)]
    cat(paste0(chosen_fortune, collapse = "\n"))
}
