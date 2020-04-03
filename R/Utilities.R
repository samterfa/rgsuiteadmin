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
  
  token <- httr::oauth_service_token(endpoint = gargle:::gargle_outh_endpoint(), 
                                     secrets = info, scope = scopes, sub = sub)
  if (is.null(token$credentials$access_token) || !nzchar(token$credentials$access_token)) {
    NULL
  }
  else {
    gargle:::cat_line("service account email: ", gargle:::token_email(token))
    token
  }
}

assignInNamespace('credentials_service_account', my_credentials_service_account, ns = 'gargle')

checkGoogleAuthentication <- function(scopes, subject = 'adminscripts@minnehahaacademy.net', serviceJsonPath = '~/rCredentials/GoogleServiceCredentials.json'){
  
  require(tidyverse)
  
  Sys.sleep(.1)
  
  if(exists('GoogleAuthToken', inherits = T)){
    
    suppressWarnings(
      if(class(GoogleAuthToken) == 'character'){
        
        file.remove('~/rCredentials/GoogleAuthToken.rds')
        rm(GoogleAuthToken, inherits = T)
        checkGoogleAuthentication(scopes = scopes) 
      }
    )
    
    if(!all(scopes %in% (GoogleAuthToken$params$scope %>% stringr::str_split(' '))[[1]]) | subject != GoogleAuthToken$params$sub){
      
      GoogleAuthToken <<- gargle::credentials_service_account(scopes = unique(union(scopes, GoogleAuthToken$params$scope)), sub = subject, path = serviceJsonPath)
      
      readr::write_rds(GoogleAuthToken, '~/rCredentials/GoogleAuthToken.rds')
    }
  }else{
  
    if(file.exists('~/rCredentials/GoogleAuthToken.rds')){
      
      GoogleAuthToken <<- readr::read_rds('~/rCredentials/GoogleAuthToken.rds')
      
      checkGoogleAuthentication(scopes, subject, serviceJsonPath)
      
    }else{
    
      GoogleAuthToken <<- gargle::credentials_service_account(scopes = scopes, sub = subject, path = serviceJsonPath)
      
      readr::write_rds(GoogleAuthToken, '~/rCredentials/GoogleAuthToken.rds')
    }
  }
}