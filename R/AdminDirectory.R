#' Get a Google User.
#'
#' This function returns information on a Google user.
#'
#' @param userKey Identifies the user in the API request. The value can be the user's primary email address, alias email address, or unique user ID.
#' @param projection What subset of fields to fetch for this user. Acceptable values are: "basic": Do not include any custom fields for the user (default), "custom": Include custom fields from schemas requested in customFieldMask, and "full": Include all fields associated with this user.
#' @param customFieldMask A comma-separated list of schema names. All fields from these schemas are fetched. This should only be set when projection=custom.
#' @param viewType Whether to fetch the administrator-only or domain-wide public view of the user. Acceptable values are "admin_view": Results include both administrator-only and domain-public fields for the user. (default), and "domain_public": Results only include fields for the user that are publicly visible to other users in the domain.
#' @concept Users
#' @return Information on the Google user.
#' @section References:
#' \href{https://developers.google.com/admin-sdk/directory/v1/reference/users/get}{Google API Documentation}
#' @export
getGoogleUser <- function(userKey, projection = NULL, customFieldMask = NULL, viewType = NULL){
  
  params <- as.list(environment()) %>% purrr::discard(str_detect(names(.), 'userKey'))
  
  endpoint <- paste0("/admin/directory/v1/users/", userKey)
  
  scopes <- "https://www.googleapis.com/auth/admin.directory.user"
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, params = params)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

#' Get a Google User's Customer ID.
#'
#' This function returns the customer key for a Google user.
#'
#' @param customerKey Id of the customer to be retrieved
#' @concept Users
#' @return The customer key of the Google user
#' @section References:
#' \href{https://developers.google.com/admin-sdk/directory/v1/reference/customers/get}{Google API Documentation}
#' @export
getGoogleCustomerID <- function(customerKey){
  
  endpoint <- paste0("/admin/directory/v1/customers/", customerKey)
  
  scopes <- "https://www.googleapis.com/auth/admin.directory.customer"
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

#' List Google Users in a Domain
#'
#' Retrieves a paginated list of either deleted users or all users in a domain.
#'
#' @param domain The domain name. Use this field to get fields from only one domain. To return all domains for a customer account, use the customer query parameter instead. Either the customer or the domain parameter must be provided.
#' @param maxResults Maximum number of results to return. Acceptable values are 1 to 500, inclusive. (Default: 100)
#' @param orderBy Property to use for sorting results. Acceptable values are: "email": Primary email of the user, "familyName": User's family name, and "givenName": User's given name.
#' @param sortOrder Whether to return results in ascending or descending order. Acceptable values are: "ASCENDING": Ascending order, and "DESCENDING": Descending order.
#' @param showDeleted If set to true, retrieves the list of deleted users. (Default: false)
#' @param query Query string for searching user fields. For more information on constructing user queries, see {https://developers.google.com/admin-sdk/directory/v1/guides/search-users} {Search for Users}.
#' @param projection What subset of fields to fetch for this user. Acceptable values are: "basic": Do not include any custom fields for the user (default), "custom": Include custom fields from schemas requested in customFieldMask, and "full": Include all fields associated with this user.
#' @param customFieldMask A comma-separated list of schema names. All fields from these schemas are fetched. This should only be set when projection=custom.
#' @param viewType Whether to fetch the administrator-only or domain-wide public view of the user. Acceptable values are "admin_view": Results include both administrator-only and domain-public fields for the user. (default), and "domain_public": Results only include fields for the user that are publicly visible to other users in the domain.
#' @param customer The unique ID for the customer's G Suite account. In case of a multi-domain account, to fetch all groups for a customer, fill this field instead of domain. You can also use the my_customer alias to represent your account's customerId. The customerId is also returned as part of the Users resource. Either the customer or the domain parameter must be provided.
#' @concept Users
#' @return A paginated list of either deleted users or all users in a domain.
#' @section References:
#' \href{https://developers.google.com/admin-sdk/directory/v1/reference/users/list}{Google API Documentation}
#' @export
listGoogleUsersInDomain <- function(domain, orderBy = NULL, sortOrder = NULL, showDeleted = NULL, query = NULL, projection = NULL, customFieldMask = NULL, viewType = NULL, customer = NULL){
  
  maxResults <- 500
  
  params <- as.list(environment())
  
  endpoint <- "/admin/directory/v1/users/"
  
  scopes <- "https://www.googleapis.com/auth/admin.directory.user"
  
  checkGoogleAuthentication(scopes = scopes)
  
  results <- NULL
  while(is.null(results) | !is.null(params$pageToken)){
    
    request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, params = params)
    
    response <- gargle::request_make(request)
    
    responseContent <- gargle::response_process(response)
    
    params$pageToken <- responseContent$nextPageToken
    
    results <- dplyr::bind_rows(results, responseContent$users %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T, simplifyVector = T))
  }
  
  return(results)
}

#' Get a Google group's settings
#'
#' This function retrieves a group's settings identified by the group email address.
#'
#' @param groupUniqueId The group's email address.
#' @concept Groups
#' @return The Google group's settings
#' @section References:
#' \href{https://developers.google.com/admin-sdk/groups-settings/v1/reference/groups/get}{Google API Documentation}
#' @export
getGoogleGroupSettings <- function(groupUniqueId){
  
  endpoint <- glue::glue('/groups/v1/groups/{groupUniqueId}')
  
  scopes <- "https://www.googleapis.com/auth/apps.groups.settings"
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, params = list(alt = 'json'), token = GoogleAuthToken)
  
  response <- gargle::request_make(request)
 
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}