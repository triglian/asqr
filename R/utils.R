#' Filter data for a specific session.
#'
#' This function is used to filter whitelist entries (users), session events
#' and slide transsionts for a specific ASQ session.
#' @param session the session to filter for
#' @param whitelistentries a data.frame with all the whitelistentries
#' @param sessionevents a data.frame with the all the sessionevents
#' @param slide_transitions a data.frame with the all the slide transitions
#' @return A \code{list} that contains the \code{sessionevents}, \code{slide_transitions},
#' \code{slide_transitions_with_ex} (slide transitions with exercises),
#' \code{slide_transitions_without_ex} (slide transitions without exercises) and the \code{users}
#' for the specified session
#' @export
#' @examples
#' session <- data.frame(id = c("s1"))
#' session[,1] <- factor(session[,1], levels=c("s1", "s2"))
#'
#' whitelistentries = data.frame(
#'   id = c("wl1", "wl2", "wl3", "wl4", "wl5"),
#'   session = c("s1", "s2", "s1", "s1", "s2"),
#'   screenName = c("Fernscar Pirate", "Glazebard Dolphin", "Deepmouse Wolverine", "Planetpegasus Eater", "Crazyfalcon Runner"),
#'   role = c("presenter", "presenter", "viewer", "viewer", "viewer")
#' )
#'
#' sessionevents = data.frame(
#'   session = c("s1", "s1", "s2"),
#'   type = c("type1", "type2", "type1")
#' )
#'
#' slidetransitions = data.frame(
#'   session = c("s1", "s1", "s1", "s2", "s2"),
#'   slide = c("slide1", "slide2", "slide3", "slide1", "slide2"),
#'   hasExercise = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' )
#'
#' filter_data_for_session(session, whitelistentries, sessionevents, slidetransitions)
filter_data_for_session <- function(session, whitelistentries, sessionevents, slide_transitions){
  # get only the sessionevents for the specific session
  sid <- session$id
  sessionevents_one_session <-  sessionevents %>% filter(session==sid)

  slide_transitions <-  slide_transitions %>% filter(session==sid) %>% collect()
  slide_transitions_with_ex <- slide_transitions[which(slide_transitions$hasExercise == TRUE),]
  slide_transitions_without_ex <- slide_transitions[which(slide_transitions$hasExercise == FALSE),]

  # get users of session
  users <-  whitelistentries %>% filter(session==sid, role=="viewer") %>% collect()

  list("sessionevents" = sessionevents_one_session,
       "slide_transitions" = slide_transitions,
       "slide_transitions_with_ex" = slide_transitions_with_ex,
       "slide_transitions_without_ex" = slide_transitions_without_ex,
       "users" = users)
}

#' Get a vector with the oids of the questions from the
#' JSON stringified `questions` field of a slideshow
#' document
#'
#' @param questions A JSON stringified string of the array of questions of a slideshow
#' @return A vector of question oids
#' @export
extract_questions_from_slideshow_questions_field <- function (questions){
  slide = ''
  questions = fromJSON(questions)
  questions$`$oid`
}


#' Get the slide that contains a question with a specific id
#'
#' Goes through the associated slideshow and retrieves the name (HTML id)
#' of the slide that contains the question with the specified id.
#' @param slideshow slideshow that contains the question
#' @param qid the question id
#' @return the name of the slide
#' @export
find_slide_which_contains_question <- function(slideshow, qid){

  qps <- NA
  slide <- NA


  qps <- fromJSON(slideshow$questionsPerSlide[1])
  yapply(qps, function(l){
    if(qid %in% l){
      slide <<- NAMES
    }
  })

  slide
}

#' Gets `n` non exercise slide transitions before the `question.slide`
#' plus the question.slide transition
#'
#' It searches for the longest slide transition for `question.slide` and then
#' it tries to find `n` consecutive slide transitions that don't contain
#' an exercise. It will return all the non-exercise slide transitions it
#' could find (up to `n`) plus the question.slide transition.
#' @param slide.transitions a data.frame with the available slide.transitions that contain `question.slide`
#' @param session the session where the `question.slide` belongs
#' @param question.slide the name of the slide that contains the question of interest
#' @param n the number of consecutive non-exercise slides to search for
#' @return a data.frame with the n+1 slide transitions
#' @export
get_n_non_exercise_slides_before_question <- function(
  slide.transitions, session,  question.slide, n = 3){

  # first we find the slide that contains are question
  # and has the greatest duration
  longest.transition <- slide.transitions %>%
    filter(session == session$id,
           slide == question.slide) %>%
    mutate(dur = (time_end - time_start)) %>%
    filter(dur == max(dur)) %>%
    select(-dur)

  # then we find the last slide that had a question before our question
  last.slide.with.ex <- slide.transitions %>%
    filter(session == session$id,
           hasExercise == TRUE,
           time_start < longest.transition$time_start,
           time_end < longest.transition$time_start) %>%
    arrange(time_end) %>%
    filter(time_end == max(time_end))

  # if we don't have a previous slide with exercise we can go up to the start of the session
  time.end.of.last.slide.with.exercise <- ifelse(nrow(last.slide.with.ex)==1, last.slide.with.ex$time_end, session$startDate )

  # get n slides without exercises before our slide together with our slide
  n.plus.one.slide.transitions <- slide.transitions %>%
    filter(session == session$id,
           hasExercise == FALSE,
           time_start > time.end.of.last.slide.with.exercise,
           time_end <= longest.transition$time_start) %>%
    arrange(time_end) %>%
    top_n(n, time_end) %>%
    bind_rows(longest.transition)

  n.plus.one.slide.transitions
}


check_na_null_0length <- function(val){
  # get name of val when it was passed as an arg
  var_name <- deparse(substitute(val))
  if(anyNA(val)) stop("argument ", var_name, " is NA")
  if(is.null(val)) stop("argument ", var_name, " is NULL")
  if(length(val)==0) stop("argument ", var_name, " has 0 length")
}

yapply <- function(X,FUN, ...) {
  index <- seq(length.out=length(X))
  namesX <- names(X)
  if(is.null(namesX))
    namesX <- rep(NA,length(X))

  FUN <- match.fun(FUN)
  fnames <- names(formals(FUN))
  if( ! "INDEX" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(INDEX=) )
  }
  if( ! "NAMES" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(NAMES=) )
  }
  mapply(FUN,X,INDEX=index, NAMES=namesX,MoreArgs=list(...))
}


#' Get the questions for a specific session id
#'
#' Goes through the associated slideshow, finds the questions ids and retrieves
#' the questions
#' @param session The session for which to get questions
#' @param slideshows slideshows to search for the matching slideshow
#' @param questions questions to search
#' @return A data.frame with the found questions
#' @export
get_questions_for_session <- function(session, slideshows, questions){

  # find slideshow at the same time
  sl <- slideshows %>%
    filter(id == session$slides[1])


  # create data.frame with questions
  sq.df <- data.frame(slideshow=character(),
                      question=character())

  sl %>%
    rowwise() %>%
    do({
      question <- extract_questions_from_slideshow_questions_field(.$questions)
      if(is.character(question)){
        slideshow <- rep(.$id, length(question))
        sq.df <<- sq.df %>% bind_rows(data.frame(slideshow, question)) %>% collect()
      }
    }) %>%
    ungroup()

  # add slide name
  qins.df <- sq.df %>%
    inner_join(questions, by=c("question" ="id") ) %>%
    rowwise() %>%
    mutate(slide = find_slide_which_contains_question(sl, question)) %>%
    ungroup()

  qins.df
}

#' Get the questions for a specific session id
#'
#' Goes through the associated slideshow, finds the questions ids and retrieves
#' the questions
#' @param questions questions to add startDate to
#' @param session The session to use to scan for \code{question-activated} events
#' @param sessionevents events that contain the \code{question-activated} events
#' @return A \code{data.frame} with the session and question id and the \code{startDate} field
#' @export
add_startdate_to_questions_for_session <- function(questions, session, sessionevents){

  # find all events for question activated and add a question field
  # with the id of the question
  sid <- session$id

  start_times <-sessionevents %>%
    filter(session == sid,  type == 'question-activated')  %>%
    select(id, session, data, time) %>%
    rowwise()  %>%
    mutate(question= fromJSON(data)$question$`$oid`)  %>%
    ungroup() %>%

    # pick only the first activation event for each question
    group_by(question)  %>%
    slice(which.min(time)) %>%
    # filter(time == min(time))%>% this allows ties of min values

    left_join(questions, by='question')  %>%
    select(session, question, startDate = time)
}
