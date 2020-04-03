baseUrl <- 'https://classroom.googleapis.com'

createGoogleCourse <- function(name, section, ownerId){
  
  endpoint <- '/v1/courses/'
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  payload <- list(name = name, section = section, ownerId = ownerId) %>% jsonlite::toJSON(auto_unbox = T)
  
  request <- gargle::request_build(method = 'POST', path = endpoint, params = list(alt = 'json'), body = payload, content_type('text/html'), token = GoogleAuthToken, base_url = 'https://classroom.googleapis.com')
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

listGoogleCourses <- function(studentId = NULL, teacherId = NULL, courseStates = NULL, pageSize = NULL){
  
  endpoint <- '/v1/courses/'
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  nextPageToken <- NULL
  results <- NULL
  while(is.null(results) | !is.null(nextPageToken)){
    
    request <- gargle::request_build(method = 'GET', path = endpoint, params = list(studentId = studentId, teacherId = teacherId, courseStates = courseStates, pageSize = pageSize, pageToken = nextPageToken), token = GoogleAuthToken, base_url = 'https://classroom.googleapis.com')
    
    response <- gargle::request_make(request)
    
    responseContent <- gargle::response_process(response)
    
    nextPageToken <- responseContent$nextPageToken
    
    results <- dplyr::bind_rows(results, responseContent$courses %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T, simplifyVector = T))
  }
  
  return(results)
}

getGoogleCourse <- function(courseId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, base_url = baseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

deleteGoogleCourse <- function(courseId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'DELETE', path = endpoint, token = GoogleAuthToken, base_url = baseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

addStudentsToGoogleCourse <- function(courseId, userIds){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/students')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.rosters'
  
  checkGoogleAuthentication(scopes = scopes)
  
  responseContents <- NULL
  for(userId in userIds){
    
    payload <- list(courseId = courseId, userId = userId) %>% jsonlite::toJSON(auto_unbox = T)
    
    request <- gargle::request_build(method = 'POST', path = endpoint, params = list(alt = 'json'), body = payload, content_type('text/html'), token = GoogleAuthToken, base_url = 'https://classroom.googleapis.com')
    
    response <- gargle::request_make(request)
    
    tryCatch({
      responseContent <- gargle::response_process(response)
    }, error = function(e) {
    })
  }
}

listStudentsInGoogleCourse <- function(courseId, pageSize = NULL){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/students')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.rosters'
  
  checkGoogleAuthentication(scopes = scopes)
  
  nextPageToken <- NULL
  results <- NULL
  while(is.null(results) | !is.null(nextPageToken)){
    
    request <- gargle::request_build(method = 'GET', path = endpoint, params = list(pageSize = pageSize, pageToken = nextPageToken), token = GoogleAuthToken, base_url = 'https://classroom.googleapis.com')
    
    response <- gargle::request_make(request)
    
    responseContent <- gargle::response_process(response)
    
    nextPageToken <- responseContent$nextPageToken
    
    results <- dplyr::bind_rows(results, responseContent$student %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T, simplifyVector = T))
  }
  
  return(results)
}

getStudentInGoogleCourse <- function(courseId, userId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/students/{userId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, base_url = baseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(as.data.frame(responseContent, stringsAsFactors = F))
}


deleteStudentInGoogleCourse <- function(courseId, userId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/students/{userId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'DELETE', path = endpoint, token = GoogleAuthToken, base_url = baseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

addTeachersToGoogleCourse <- function(courseId, userIds){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/teachers')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.rosters'
  
  checkGoogleAuthentication(scopes = scopes)
  
  responseContents <- NULL
  for(userId in userIds){
    
    payload <- list(courseId = courseId, userId = userId) %>% jsonlite::toJSON(auto_unbox = T)
    
    request <- gargle::request_build(method = 'POST', path = endpoint, params = list(alt = 'json'), body = payload, content_type('text/html'), token = GoogleAuthToken, base_url = 'https://classroom.googleapis.com')
    
    response <- gargle::request_make(request)
    
    tryCatch({
      responseContent <- gargle::response_process(response)
    }, error = function(e) {
    })
  }
}

listTeachersInGoogleCourse <- function(courseId, pageSize = NULL){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/teachers')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.rosters'
  
  checkGoogleAuthentication(scopes = scopes)
  
  nextPageToken <- NULL
  results <- NULL
  while(is.null(results) | !is.null(nextPageToken)){
    
    request <- gargle::request_build(method = 'GET', path = endpoint, params = list(pageSize = pageSize, pageToken = nextPageToken), token = GoogleAuthToken, base_url = 'https://classroom.googleapis.com')
    
    response <- gargle::request_make(request)
    
    responseContent <- gargle::response_process(response)
    
    nextPageToken <- responseContent$nextPageToken
    
    results <- dplyr::bind_rows(results, responseContent$teacher %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T, simplifyVector = T))
  }
  
  return(results)
}

getTeacherInGoogleCourse <- function(courseId, userId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/teachers/{userId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, base_url = baseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(as.data.frame(responseContent, stringsAsFactors = F))
}


deleteTeacherInGoogleCourse <- function(courseId, userId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/teachers/{userId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'DELETE', path = endpoint, token = GoogleAuthToken, base_url = baseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}
