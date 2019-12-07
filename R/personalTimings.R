personalTiming <- function() {
  gitlog <- system("git log --pretty=%h;%s;%ai --reverse", intern = TRUE)
  gitlog <- data.table::fread(text = gitlog, header = FALSE)
  names(gitlog) <- c("hash", "subject", "time")
  timings <- gitlog[, time := strptime(time, "%Y-%m-%d %H:%M:%S %z")][grepl("^(begin|solve)", subject)]
  timings[, day := as.numeric(gsub(".*day (\\d+).*", "\\1", subject))]
  timings[, part := as.numeric(gsub(".*part (\\d+).*", "\\1", subject))]
}
