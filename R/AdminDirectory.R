getGoogleUser <- function(userKey){
  
  endpoint <- paste0("/admin/directory/v1/users/", userKey)
  
  scopes <- "https://www.googleapis.com/auth/admin.directory.user"
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

getGoogleCustomerID <- function(customerKey){
  
  endpoint <- paste0("/admin/directory/v1/customers/", customerKey)
  
  scopes <- "https://www.googleapis.com/auth/admin.directory.customer"
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
  
}

listGoogleUsersInDomain <- function(domain){
  
  endpoint <- "/admin/directory/v1/users/" %>% glue::glue('?domain={domain}&maxResults=500')
  
  scopes <- "https://www.googleapis.com/auth/admin.directory.user"
  
  checkGoogleAuthentication(scopes = scopes)
  
  nextPageToken <- NULL
  results <- NULL
  while(is.null(results) | !is.null(nextPageToken)){
    
    request <- gargle::request_build(method = 'GET', path = ifelse(is.null(results), endpoint, endpoint %>% glue::glue('&pageToken={nextPageToken}')), token = GoogleAuthToken)
    
    response <- gargle::request_make(request)
    
    responseContent <- gargle::response_process(response)
    
    nextPageToken <- responseContent$nextPageToken
    
    results <- dplyr::bind_rows(results, responseContent$users %>% jsonlite::toJSON() %>% jsonlite::fromJSON(flatten = T))
  }
  
  return(results)
}

getGoogleGroupSettings <- function(googleGroupEmail){
  
  endpoint <- paste0("/groups/v1/groups/", googleGroupEmail)
  
  scopes <- "https://www.googleapis.com/auth/apps.groups.settings"
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, params = list(alt = 'json'), token = GoogleAuthToken)
  
  response <- gargle::request_make(request)
  return(response)
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}