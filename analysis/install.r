repos <- "http://cran.rstudio.com"
lib.loc <- Sys.getenv("R_LIBS_USER")

update.packages(repos=repos, ask=FALSE, lib.loc=lib.loc)

install.packages(c("igraph")    , repos=repos)
