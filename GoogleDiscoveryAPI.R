
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
  
  if(!exists('googleAPI', inherits = T)) googleAPI <- listAPIs()
  
  if(!exists('adminDirectoryAPI', inherits = T)) adminDirectoryAPI <- getAPIdetails(getAdminDirectoryDiscoveryRestURl(googleAPIs))
  
  object <- adminDirectoryAPI
  
  resources <- getNestedResourcesForObject(adminDirectoryAPI)
  
  for(resource in resources){
    
    return(resource)
    
    for(method in resource$methods){
      
      print(method$description)
      
    }
    
  }
  
}

getNestedResourcesForObject <- function(objectList){
  
  require(tidyverse)
  
  allResources <- NULL
  if('resources' %in% names(objectList)){
    for(object in names(objectList$resources) %>% str_subset('[^resources]')){
        
      methods <- objectList$resources[[object]]$methods %>% names()
      for(method in methods){
        
        allResources[[length(allResources) + 1]] <- objectList$resources[[object]]$methods[[method]]
        allResources[[length(allResources)]]$method <- method
        allResources[[length(allResources)]]$object <- object
        
        allResources[[length(allResources)]] <- allResources[[length(allResources)]][append(c(which(names(allResources[[length(allResources)]]) == 'object'), which(names(allResources[[length(allResources)]]) == 'method')), (1:(length(names(allResources[[length(allResources)]])) - 2)))]
      }
    }
  }
  
  for(name in names(objectList)){
  
    allResources <- append(allResources, getNestedResourcesForObject(objectList[[name]]))
    
  }
  
  return(allResources)
}