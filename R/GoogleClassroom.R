googleClassroomBaseUrl <- googleClassroomBaseUrl

#' Create a Course Section
#'
#' This function creates a Google Classroom Course
#'
#' @param name The name of the course
#' @param section The name of the course section
#' @param ownerId The owner of the created course who is added as a teacher
#' @concept Classroom
#' @return Information on the newly created course.
#' @section References:
#' \href{https://developers.google.com/classroom/reference/rest/v1/courses/create}{Google API Documentation}
#' @export
createGoogleCourse <- function(name, section, ownerId){
  
  endpoint <- '/v1/courses/'
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  payload <- list(name = name, section = section, ownerId = ownerId) %>% jsonlite::toJSON(auto_unbox = T)
  
  request <- gargle::request_build(method = 'POST', path = endpoint, params = list(alt = 'json'), body = payload, content_type('text/html'), token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

#' List Google Courses
#'
#' This function lists all Google Classroom Courses by student, teacher or domain
#'
#' @param studentId Restricts returned courses to those having a student with the specified identifier. The identifier can be one of the following: the numeric identifier for the user, the email address of the user, the string literal "me", or indicating the requesting user
#' @param teacherId Restricts returned courses to those having a teacher with the specified identifier. The identifier can be one of the following: the numeric identifier for the user, the email address of the user, the string literal "me", or the indicating the requesting user
#' @param courseStates Restricts returned courses to those in one of the specified states The default value is ACTIVE, ARCHIVED, PROVISIONED, DECLINED.
#' @concept Classroom
#' @return A list of Google Classroom courses.
#' @section References:
#' \href{https://developers.google.com/classroom/reference/rest/v1/courses/list}{Google API Documentation}
#' @export
listGoogleCourses <- function(studentId = NULL, teacherId = NULL, courseStates = NULL){
  
  pageSize <- 500
  
  params <- as.list(environment())
  
  endpoint <- '/v1/courses/'
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  results <- NULL
  while(is.null(results) | !is.null(params$pageToken)){
    
    request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, params = params, base_url = googleClassroomBaseUrl)
    
    response <- gargle::request_make(request)
  
    responseContent <- gargle::response_process(response)
    
    params$pageToken <- responseContent$nextPageToken
    
    results <- dplyr::bind_rows(results, responseContent$courses %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T, simplifyVector = T))
  }
  
  return(results)
}

getGoogleCourse <- function(courseId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(responseContent)
}

deleteGoogleCourse <- function(courseId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'DELETE', path = endpoint, token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
  
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
    
    request <- gargle::request_build(method = 'POST', path = endpoint, params = list(alt = 'json'), body = payload, content_type('text/html'), token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
    
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
    
    request <- gargle::request_build(method = 'GET', path = endpoint, params = list(pageSize = pageSize, pageToken = nextPageToken), token = GoogleAuthToken, base_url = )
    
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
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(as.data.frame(responseContent, stringsAsFactors = F))
}


deleteStudentInGoogleCourse <- function(courseId, userId){
  
  endpoint <- glue::glue('/v1/courses/{courseId}/students/{userId}')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.courses'
  
  checkGoogleAuthentication(scopes = scopes)
  
  request <- gargle::request_build(method = 'DELETE', path = endpoint, token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
  
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
    
    request <- gargle::request_build(method = 'POST', path = endpoint, params = list(alt = 'json'), body = payload, content_type('text/html'), token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
    
    response <- gargle::request_make(request)
    
    tryCatch({
      responseContent <- gargle::response_process(response)
    }, error = function(e) {
    })
  }
}

removeTeachersFromGoogleCourse <- function(courseId, userIds){
  
  baseEndpoint <- glue::glue('/v1/courses/{courseId}/teachers')
  
  scopes <- 'https://www.googleapis.com/auth/classroom.rosters'
  
  checkGoogleAuthentication(scopes = scopes)
  
  responseContents <- NULL
  for(userId in userIds){
    
    payload <- list(courseId = courseId, userId = userId) %>% jsonlite::toJSON(auto_unbox = T)
    
    endpoint <- baseEndpoint %>% glue::glue('/{userId}')
    
    request <- gargle::request_build(method = 'DELETE', path = endpoint, params = list(alt = 'json'), body = payload, content_type('text/html'), token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
    
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
    
    request <- gargle::request_build(method = 'GET', path = endpoint, params = list(pageSize = pageSize, pageToken = nextPageToken), token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
    
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
  
  request <- gargle::request_build(method = 'GET', path = endpoint, token = GoogleAuthToken, base_url = googleClassroomBaseUrl)
  
  response <- gargle::request_make(request)
  
  responseContent <- gargle::response_process(response)
  
  return(as.data.frame(responseContent, stringsAsFactors = F))
}
