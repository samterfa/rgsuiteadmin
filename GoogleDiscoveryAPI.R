
DiscoveryAPIlistUrl <- 'https://www.googleapis.com/discovery/v1/apis'

listAPIs <- function(){
  
  require(tidyverse)
  require(httr)
  require(magrittr)
  require(jsonlite)
  
  apis <- content(GET(DiscoveryAPIlistUrl))$items
  
  apis %<>% toJSON(pretty = T) %>% fromJSON(flatten = T)
  
  apis$labels <- NULL
  apis$discoveryLink <- NULL
  
  apis %<>% lapply(unlist) %>% as.data.frame(stringsAsFactors = F)
  
  return(apis)
}

getAdminDirectoryDiscoveryRestURl <- function(googleAPIs = listAPIs()){
  
  require(stringr)
  
  return(googleAPIs$discoveryRestUrl %>% str_subset('admin/directory') %>% unlist())
  
}

getAPIdetails <- function(discoveryRestUrl){
  
  api <- content(GET(discoveryRestUrl))
  
  return(api)
}

getAPImethods <- function(discoveryRestUrl){
  
  api <- content(GET(discoveryRestUrl))
  
  for(objectName in names(api$resources)){
    
    object <- api$resources[[objectName]]
    
    for(methodName in names(object$methods)){
      
      method <- object[[methodName]]
      
      functionName <- paste0(tolower(methodName), toupper(substr(objectName, 1, 1)), substr(objectName, 2, nchar(objectName)))
      
    }
    
  #  return(object)
    
  }
  
}

generateAdminDirectoryFunctions <- function(){
  
  if(!exists('adminDirectoryAPI', inherits = T)) adminDirectoryAPI <- getAPIdetails(getAdminDirectoryDiscoveryRestURl(googleAPIs))
  
  object <- adminDirectoryAPI
  
  resources <- getResourcesForObject(adminDirectoryAPI)
  
  for(resource in resources){
    
    for(method in resource$methods){
      
      print(method$description)
      
    }
    
  }
  
}

getResourcesForObject <- function(objectList){
  
  allObjects <- NULL
  for(object in names(objectList$resources) %>% str_subset('[^resources]')){
        
    allObjects <- append(allObjects, object)
    
  #  allResources[[length(allResources) + 1]] <- resource
    
  }
  
  for(name in names(objectList)){
  
    if('resources' %in% names(objectList[[name]])) allObjects <- append(allObjects, getResourcesForObject(objectList[[name]])) 
    
#    if('resources' %in% names(objectList[[name]])) allResources <- append(allResources, getResourcesForObject(objectList[[name]])) 
      
  }
  
  return(allObjects)
}