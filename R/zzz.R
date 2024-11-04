.onLoad <- function(libname, pkgname) {

   if (Sys.getenv("INDRA_API_URL") == "") {
    Sys.setenv(INDRA_API_URL = "http://localhost:5000")
  }

}