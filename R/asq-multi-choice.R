#' Get the multi-choice questions for a specific session id
#'
#' Goes through the associated slideshow, finds the questions ids, retrives
#' the questions and then filters for the `asq-multi-choice-q` type.
#' @param sessions sessions to search for the session with `sessionid`
#' @param slideshows slideshows to search for the matching slideshow
#' @param questions questions to search for multi-choice questions
#' @param sessionid the id of the session we want to find multi-choice questions for
#' @return A data.frame with multi-choice questions
#' @export
get_mc_questions_for_session <- function(sessions, slideshows, questions, sessionid){
  session <- sessions %>% filter(id == sessionid)

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

  # get multi-choice only and add slide name
  qins.df <- sq.df %>%
    inner_join(questions, by=c("question" ="id") ) %>%
    filter(type == "asq-multi-choice-q") %>%
    rowwise() %>%
    mutate(slide = find_slide_which_contains_question(sl, question)) %>%
    ungroup()

  qins.df
}


#' Get answers for the provided asq-multi-choice-questions and calculate their score
#'
#' @param answers a data.frame of answers to search in
#' @param questions a data.frame with questions to search answers for
#' @return a data.frame with the matching answers and their scores
#' @export
get_mc_answers_with_score_for_questions <- function(answers, questions){
  result <- answers %>%
    filter(question %in% questions$question ) %>%
    inner_join( questions, by="question") %>%
    select(-settings, -date_modified, -date_created) %>%
    rowwise() %>%
    mutate(score = calc_mc_score(submission, parse_mc_options(data))) %>%
    ungroup() %>%
    select(-slideshow, -data)
  result
}


#' Parse JSON stringified options from an asq-multi-choice-q question `data`
#' field
#'
#' @param data JSON stringifed data-field of an asq-multi-choice-q question
#' @return a vector with the parsed options
#' @export
parse_mc_options <- function(data){
  options <- fromJSON(data)[["options"]]
  options$`_id` = options$`_id`$`$oid`

  options
}


#' Calculate the score for an answer of a multi-choice-question
#'
#' If the submission has exactly the same checked options as the solution
#' then the score is 100. Otherwise, it's 0.
#' @param submission JSON stringifed submission from a multi-choice answer
#' @param solution a vector of multi-choice-q options
#' @return a vector with the parsed options
#' @export
calc_mc_score <- function (submission, solution){
  submission <-  fromJSON(submission)
  submission$`_id` = submission$`_id`$`$oid`

  score <- NA

  # return number only if there is a solution
  # TODO when we know if a multi-choice question is multi or not
  if(TRUE %in% solution$correct){

    solution <- as.data.frame(solution)
    sub <- as.data.frame(submission)

    j <- submission %>%
      inner_join(solution, by="_id") %>%
      filter(value == correct)


    if(nrow(j) ==  nrow(solution)){
      score <- 100
    }else{
      score = 0
    }

  }
  score
}

#' TODO: Document
get_n_back_timeline_with_correctness_for_mc <- function(user.state, session, sessions, questions, answers, whitelistentries, sessionevents, slide.transitions){
  user.state.dt <- user.state
  user.state.dt[, time:= as.POSIXct(time)]
  user.state.dt[, state := ifelse(state == 10, 2, state)]
  user.state.dt[, state := ifelse(state == 11, 3, state)]
  user.state.dt[, state := ifelse(state == 15, 7, state)]
  user.state.dt[, state := ifelse(state == 43, 35, state)]
  user.state.dt[, state := ifelse(state == 47, 39, state)]
  user.state.dt[, state:= as.factor(state)]


  mc.questions.df <- get_mc_questions_for_session(sessions, slideshows, questions, session$id)
  mc.answers.df <- get_mc_answers_with_score_for_questions(answers, mc.questions.df)


  res.dt <- user.state.dt[0]
  res.dt[,targetQuestion := character()]
  res.dt[,correct := logical()]

  foo <- apply(mc.questions.df, 1, function(q){

    n.plus.one.slide.transitions <- get_n_non_exercise_slides_before_question(slide.transitions, session, q[["slide"]], n=5)
    if(nrow(n.plus.one.slide.transitions) == 1){
      return()
    }

    sub.dt <- user.state.dt %>% filter(time >= min(n.plus.one.slide.transitions$time_start),
                                       time <= max(n.plus.one.slide.transitions$time_end))

    d.ids <- sub.dt  %>% select(user) %>% distinct() %>% arrange(user)
    d.ids$user.name <- paste0("viewer #", seq(1, nrow(d.ids)))
    sub.dt  <- sub.dt  %>% left_join(d.ids, by="user")

    first.answers <- mc.answers.df %>%
      filter(question == q[["question"]], session== session$id) %>%
      group_by(question, answeree) %>%
      arrange(submitDate) %>%
      top_n(1, submitDate) %>%
      ungroup()

    if(nrow(first.answers) == 0){
      return()
    }

    correctness.threshold <- 100;


    correct.users <- first.answers %>%
      filter( question ==q[["question"]], score >= correctness.threshold) %>%
      select(answeree) %>%
      inner_join(whitelistentries, by = c("answeree" = "id")) %>%
      select(user)

    state.for.correct.answers.dt <- sub.dt  %>%
      filter (user %in% correct.users$user)

    incorrect.users <- first.answers %>%
      filter( question == q[["question"]], score < correctness.threshold) %>%
      select(answeree) %>%
      inner_join(whitelistentries, by = c("answeree" = "id")) %>%
      select(user)

    state.for.incorrect.answers.dt <- sub.dt  %>%
      filter (user %in% incorrect.users$user)

    state.for.noanswer.dt <- sub.dt  %>%
      filter (user %ni% c(correct.users$user, incorrect.users$user))

    preceding.correct.dt <- state.for.correct.answers.dt %>%
      # filter(time < n.plus.one.slide.transitions$time_start[nrow(n.plus.one.slide.transitions)]) %>%
      mutate(targetQuestion = q[["question"]], correct=TRUE)
    preceding.incorrect.dt <- state.for.incorrect.answers.dt %>%
      # filter(time < n.plus.one.slide.transitions$time_start[nrow(n.plus.one.slide.transitions)]) %>%
      mutate(targetQuestion = q[["question"]], correct=FALSE)

    preceding.noanswer.dt <- state.for.noanswer.dt %>%
      # filter(time < n.plus.one.slide.transitions$time_start[nrow(n.plus.one.slide.transitions)]) %>%
      mutate(targetQuestion = q[["question"]], correct=NA)

    res.dt <<- res.dt %>%
      bind_rows(preceding.correct.dt)  %>%
      bind_rows(preceding.incorrect.dt) %>%
      bind_rows(preceding.noanswer.dt)

  })
  res.dt
}
