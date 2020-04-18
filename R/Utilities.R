my_credentials_service_account <- function (scopes = NULL, path = "", sub = NULL, ...) {
 
  gargle:::cat_line("trying my_credentials_service_account()")
  info <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(info[["type"]], "service_account")) {
    gargle:::cat_line("JSON does not appear to represent a service account\n", 
                      "did you provide the JSON for an OAuth client instead of for a ", 
                      "service account?")
    return()
  }
  scopes <- httr:::normalize_scopes(gargle:::add_email_scope(scopes))
 
  token <- httr::oauth_service_token(endpoint = gargle:::gargle_outh_endpoint(), secrets = info, scope = scopes , sub = sub)
  
  if (is.null(token$credentials$access_token) || !nzchar(token$credentials$access_token)) {
    return(token)
    NULL
  }
  else {
    gargle:::cat_line("service account email: ", gargle:::token_email(token))
    token
  }
}

checkGoogleAuthentication <- function(scopes, sub = Sys.getenv('ADMIN_DIR_USER_EMAIL'), serviceJsonPath = Sys.getenv('GOOGLE_SERVICE_JSON_PATH')){
  
  require(tidyverse)
  
  if(Sys.getenv('ADMIN_DIR_USER_EMAIL') == '') stop('Missing Environment Variable! Run Sys.setenv(ADMIN_DIR_USER_EMAIL = {YourGoogleAdminUserEmail})')
  if(Sys.getenv('GOOGLE_SERVICE_JSON_PATH') == '') stop('Missing Environment Variable! Run Sys.setenv(GOOGLE_SERVICE_JSON_PATH = {YourGoogleServiceJsonPath})')
  
  Sys.sleep(.1)
  
  if(exists('GoogleAuthToken', inherits = T)){

    suppressWarnings(
      if(class(GoogleAuthToken) == 'character' | length(GoogleAuthToken$params$scope) == 0){

        rm(GoogleAuthToken, inherits = T)
        checkGoogleAuthentication(scopes = scopes)
      }
    )
 
    if(!all(scopes %in% (GoogleAuthToken$params$scope %>% stringr::str_split(' '))%>% purrr::pluck(1)) | is.null(GoogleAuthToken$params$sub)){
      
      GoogleAuthToken <<- my_credentials_service_account(scopes = unique(union(scopes, GoogleAuthToken$params$scope %>% stringr::str_split(' ') %>% purrr::pluck(1))), sub = sub, path = serviceJsonPath)

    }
  }else{
    
      GoogleAuthToken <<- my_credentials_service_account(scopes = scopes, path = serviceJsonPath, sub = sub)
      
    }
}

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