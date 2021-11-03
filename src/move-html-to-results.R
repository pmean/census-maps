src <- "~/zipcode-example/src"
results <- "~/zipcode-example/results"
for (i in list.files(src, pattern="*.html")) {
  file.copy(paste(src, i, sep="/"), results)
  file.remove(paste(src, i, sep="/"))
}
