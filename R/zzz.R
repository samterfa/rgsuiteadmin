.onLoad <- function(libname, pkgname){
 suppressPackageStartupMessages(
   
   {
     pkgs <- c('dplyr')#,'stringr','magrittr','gargle','jsonlite','httr','glue', 'purrr');
     
     for(pkg in pkgs) {
       eval(parse(text = paste0('library(', pkg, ')')))
     }
   }
 )
}