DriveIter <- function(tab, vec, id) {
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[tab$id %in% vec, ]
  print(c(id, print(length(vec))))
  write.table(tab, paste0("C:\\OLMA\\TestDrive\\", "file", id), row.names = FALSE)
unique(tab$parent)
}
MainDrive <- function(tab) {
  flag <- TRUE
  id <- 1
  vec <- unique(tab$id)
  while (flag) {
    vec <- DriveIter(tab, vec, id)
    if (length(vec) == 0) {
      flag <- FALSE
    } else {
      id <- id + 1
    }
  }
}
GetFullInfoByParent <- function(id, parent) {
  vec <- c(parent)
  while(id >= 1) {
    parent <- GetInfoByParent(id, parent)
    vec <- c(vec, parent)
    id <- id - 1
  }
  print(vec)
  print(length(vec))
  strvec <- vec[1]
  for (j in 2:length(vec)) {
    strvec <- paste0(strvec, "->", vec[j])
  }
write.table(strvec, "info.txt", row.names = FALSE, col.names = FALSE)
}
GetInfoByParent <-function(id, parent) {
  tab <- fread(paste0("C:\\OLMA\\TestDrive\\file", id))
  vec <- tab$id[tab$parent == parent]
vec
}
