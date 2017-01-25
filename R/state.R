#' Calculates the connected state.
#'
#' Given a \code{data.frame} of user event and an associated timeline, for each
#' second of the timeline it calculates if the user was connected or not.
#' @param user_events \code{data.frame} of user events.
#' @param timeline \code{data.frame} that described the state in one second intervals.
#' @return An updated timeline \code{data.frame} with the connected state set.
#' @export
calc_connected_state <- function(user_events, timeline){
  connect_events <- user_events %>%
    filter(type %in% c("folo-connected", "folo-disconnected")) %>%
    arrange(time) %>%
    collect()

  if(nrow(connect_events)>0){
    if(nrow(connect_events)>1){
      for (i in 1:(nrow(connect_events)-1))
      {
        is_connected <- ifelse(connect_events$type[i] =="folo-connected", TRUE, FALSE)
        if(nrow(timeline[timeline$time >=connect_events$time[i] & timeline$time < connect_events$time[i+1] ,])){
          timeline[timeline$time >=connect_events$time[i] & timeline$time < connect_events$time[i+1] ,]$connected = is_connected
        }
      }
    }

    # fix the last elements. we don't have an if to check if it's the last element
    # in the loop so that it doesn't get evaluated all the time
    i <- nrow(connect_events)
    is_connected <- ifelse(connect_events$type[i] =="folo-connected", TRUE, FALSE)
    if(nrow(timeline[timeline$time >=connect_events$time[i] ,])){
      timeline[timeline$time >=connect_events$time[i] ,]$connected = is_connected
    }
  }
  timeline
}

#' Calculates the visible state.
#'
#' Given a \code{data.frame} of user event and an associated timeline, for each
#' second of the timeline it calculates if the user window was visible or not.
#' @param user_events \code{data.frame} of user events.
#' @param timeline \code{data.frame} that described the state in one second intervals.
#' @return An updated timeline \code{data.frame} with the visible state set.
#' @export
calc_visible_state <- function(user_events, timeline){
  visible_events <- user_events %>%
    filter(type %in% c("tabhidden", "tabvisible")) %>%
    arrange(time) %>%
    collect()

  out_of_focus_event_types <- c("tabhidden")


  if(nrow(visible_events)>0){
    if(nrow(visible_events)>1){
      for (i in 1:(nrow(visible_events)-1))
      {
        is_visible <- ifelse(visible_events$type[i] == "tabhidden", FALSE, TRUE)
        if(nrow(timeline[timeline$time >=visible_events$time[i] & timeline$time < visible_events$time[i+1] ,])){
          timeline[timeline$time >=visible_events$time[i] & timeline$time < visible_events$time[i+1] ,]$visible = is_visible
        }
      }
    }

    # fix the last elements_ we don't have an if to check if it's the last element
    # in the loop so that it doesn't get evaluated all the time
    i <- nrow(visible_events)
    is_visible <- ifelse(visible_events$type[i] == "tabhidden", FALSE, TRUE)
    if(nrow(timeline[timeline$time >=visible_events$time[i] ,])){
      timeline[timeline$time >=visible_events$time[i] ,]$visible = is_visible
    }
  }

  timeline
}

#' Calculates the focus state.
#'
#' Given a \code{data.frame} of user event and an associated timeline, for each
#' second of the timeline it calculates if the user window had focus or not.
#' @param user_events \code{data.frame} of user events.
#' @param timeline \code{data.frame} that described the state in one second intervals.
#' @return An updated timeline \code{data.frame} with the focus state set.
#' @export
calc_focus_state <- function(user_events, timeline){
  focus_related_event_types <- c("tabhidden",
                                 "tabvisible",
                                 "windowblur",
                                 "windowfocus",
                                 "focusin",
                                 "focusout",
                                 "exercisefocus",
                                 "exerciseblur",
                                 "input",
                                 "questioninput")
  focus_events <- user_events %>%
    filter(type %in% focus_related_event_types) %>%
    arrange(time) %>%
    collect()

  out_of_focus_event_types <- c("tabhidden", "windowblur", "focusout", "exerciseblur")


  if(nrow(focus_events)>0){
    if(nrow(focus_events)>1){
      for (i in 1:(nrow(focus_events)-1))
      {
        has_focus <- ifelse(focus_events$type[i] %in% out_of_focus_event_types, FALSE, TRUE)
        if(nrow(timeline[timeline$time >=focus_events$time[i] & timeline$time < focus_events$time[i+1] ,])){
          timeline[timeline$time >=focus_events$time[i] & timeline$time < focus_events$time[i+1] ,]$focus = has_focus
        }
      }
    }

    # fix the last elements_ we don't have an if to check if it's the last element
    # in the loop so that it doesn't get evaluated all the time
    i <- nrow(focus_events)
    has_focus <- ifelse(focus_events$type[i] %in% out_of_focus_event_types, FALSE, TRUE)
    if(nrow(timeline[timeline$time >=focus_events$time[i] ,])){
      timeline[timeline$time >=focus_events$time[i] ,]$focus = has_focus
    }
  }

  timeline
}

#' Calculates the idle state.
#'
#' Given a \code{data.frame} of user event and an associated timeline, for each
#' second of the timeline it calculates if the user was idle or not.
#' @param user_events \code{data.frame} of user events.
#' @param timeline \code{data.frame} that described the state in one second intervals.
#' @return An updated timeline \code{data.frame} with the idle state set.
#' @export
calc_idle_state <- function(user_events, timeline){
  idle_related_event_types <- c("tabhidden",
                                          "tabvisible",
                                          "windowblur",
                                          "windowfocus",
                                          "focusin",
                                          "focusout",
                                          "exercisefocus",
                                          "exerciseblur",
                                          "input",
                                          "questioninput",
                                          "viewer-idle")

  idle_related_events <- user_events %>%
    filter(type %in% idle_related_event_types) %>%
    arrange(time) %>%
    collect()

  if(nrow(idle_related_events)>0){
    if(nrow(idle_related_events)>1){
      for (i in 1:(nrow(idle_related_events)-1))
      {
        # only the `viewer-idle` event makes the user idle.
        is_idle <- ifelse(idle_related_events$type[i] == "viewer-idle", TRUE, FALSE)
        if(nrow(timeline[timeline$time >=idle_related_events$time[i] & timeline$time < idle_related_events$time[i+1] ,])){
          timeline[timeline$time >=idle_related_events$time[i] & timeline$time < idle_related_events$time[i+1] ,]$idle = is_idle
        }
      }
    }

    # fix the last elements. we don't have an if to check if it's the last element
    # in the loop so that it doesn't get evaluated all the time
    i <- nrow(idle_related_events)
    is_idle <- ifelse(idle_related_events$type[i] == "viewer-idle", TRUE, FALSE)
    if(nrow(timeline[timeline$time >=idle_related_events$time[i] ,])){
      timeline[timeline$time >=idle_related_events$time[i] ,]$idle = is_idle
    }
  }
  timeline
}

#' Calculates the input state.
#'
#' Given a \code{data.frame} of user event and an associated timeline, for each
#' second of the timeline it calculates if the user was giving input or not.
#' @param user_events \code{data.frame} of user events.
#' @param timeline \code{data.frame} that described the state in one second intervals.
#' @return An updated timeline \code{data.frame} with the input state set.
#' @export
calc_input_state <- function(user_events, timeline){
  input_events <- user_events %>%
    filter(type %in% c("input","questioninput")) %>%
    arrange(time) %>%
    collect()

  timeline %<>% mutate(input = ifelse(time %in% input_events$time, TRUE, input))  %>% collect()
  as.data.table(timeline)
}

findExercise <- function(data){
  as_character(fromJSON(data)$exerciseSubmission$`$oid`)
}

#' Calculates the submitted state.
#'
#' Given a \code{data.frame} of user event and an associated timeline, for each
#' second of the timeline it calculates if the user was submitted at least once or not.
#' @param user_events \code{data.frame} of user events.
#' @param timeline \code{data.frame} that described the state in one second intervals.
#' @return An updated timeline \code{data.frame} with the submitted state set.
#' @export
calc_submittedonce_state <- function(user_events, timeline){
  submitted_events <- user_events %>%
    filter(type == "exercise-submit") %>%
    collect()

  if(nrow(submitted_events)<1) return(timeline)

  submitted_events %<>%
    arrange(time) %>%
    collect()

  apply(submitted_events, 1, function(ev){
    timeline[time >= ev["time"] &
               slide == ev["slide"], submitted:= T ]
  })
  timeline
}


#' Calculates the total state from state indicators.
#'
#' Creates a 7 bit number according to the value
#' of the 7 vars: \code{hasExercise}, \code{visible}, \code{connected},
#' \code{focus}, \code{idle}, \code{input} and \code{submitted}.
#'
#' @param hasExercise \code{Logical} whether there is an exercise or not.
#' @param connected \code{Logical} whether the user is connected or not.
#' @param visible \code{Logical} whether the ASQ window is visible or not
#' @param focus \code{Logical} whether the ASQ window has focus or not
#' @param idle \code{Logical} whether the user is idle or not.
#' @param input \code{Logical} whether the user is giving input or not.
#' @param submitted \code{Logical} whether the user has submitted or not.
#' @return An updated timeline \code{data.frame} with the submitted state set.
#' @export
calc_state_from_indicators <- function(hasExercise, connected, visible, focus, idle, input, submitted){
  bit_1 = ifelse(hasExercise == FALSE, 0, 1)
  bit_2 = ifelse(is.na(connected) | connected == FALSE, 0, 2)
  bit_3 = ifelse(is.na(visible) | visible == FALSE, 0, 4)
  bit_4 = ifelse(is.na(focus) | focus == FALSE, 0, 8)
  bit_5 = ifelse(is.na(idle) | idle == FALSE, 0, 16)
  bit_6 = ifelse(is.na(input) | input == FALSE, 0, 32)
  bit_7 = ifelse(is.na(submitted) | submitted == FALSE, 0, 64)

  bit_1 + bit_2 + bit_3 + bit_4 + bit_5 + bit_6 + bit_7
}

#' Fixes inconsistencies between states.
#'
#' Goes through the 7 states: \code{hasExercise}, \code{connected}, \code{visible},
#' \code{focus}, \code{idle}, \code{input} and \code{submitted} and repairs
#' any inconsistencies. For example, if the user is not connected the rest of
#' the states should be \code{NA}.
#'
#' @param state_timeline_dt \code{data.table} the state timeline data.table.
#' @return An updated timeline \code{table} with the consistent states.
#' @export
fix_inconsistencies <- function(state_timeline_dt){
  state_timeline_dt <- state_timeline_dt %>%
    # when not connected focus and idle should be NA
    mutate(visible = ifelse(connected==TRUE, visible, NA),
           focus = ifelse(connected==TRUE, focus, NA),
           focus = ifelse(visible==FALSE, FALSE, focus),
           idle = ifelse(connected==TRUE, idle, NA),
           # we may get some input when there's no exercise
           # because of the initilization of the question elements
           input = ifelse(hasExercise==FALSE, FALSE, input),
           input = ifelse(connected==TRUE, input, NA)) %>%
    mutate(visible = ifelse(input==TRUE, TRUE, visible),
           focus = ifelse(input==TRUE, TRUE, focus),
           idle = ifelse(input==TRUE, FALSE, idle)) %>%
    collect()

  if (nrow(state_timeline_dt %>% filter(hasExercise==FALSE, submitted == TRUE)) > 0){
    warning("found submissions where there were no exercise_ Tried to repair it_");
    state_timeline_dt <- state_timeline_dt %>%
      mutate(submitted = ifelse(hasExercise == FALSE, FALSE, submitted))
  }
  state_timeline_dt
}

#' Calculate the user state from user events.
#'
#' Given a \code{date.frame} timeline where each subsequent row corresponds to
#' the next a second of a lecture and the events for a \strong{single} user, it
#' calculates the state of the user for each second of the lecture as 6
#' different indicators:
#'
#' \code{hasExercise}, \code{connected}, \code{visible}, \code{focus},
#' \code{idle}, \code{input} and \code{submitted}.
#' @param timeline \code{date.frame} Each row represents a second in the lecture.
#' @param user_events \code{date.frame} Events from a \strong{single} user during the lecture.
#' @param from \code{POSIXct} pick user events that happend since the value of this variable.
#' @param to \code{POSIXct} pick user events that happend up to the value of this variable.
#' @return An updated timeline \code{data.table} with the user states.
#' @export
calc_user_state <- function(timeline, user_events, from, to){

  # we only care for events within the session time limits
  user_events %<>% filter(time>=from & time<=to) %>%collect()

  timeline <- calc_connected_state(user_events, timeline)
  timeline <- calc_visible_state(user_events, timeline)
  timeline <- calc_focus_state(user_events, timeline)
  timeline <- calc_idle_state(user_events, timeline)
  timeline <- calc_input_state(user_events, timeline)
  timeline <- calc_submittedonce_state(user_events, timeline)

  timeline2 <- fix_inconsistencies(timeline)

  # calculate state
  timeline2 <- timeline2 %>%
    mutate(state = calc_state_from_indicators(hasExercise, connected, visible, focus, idle, input, submitted))

  # reorder factor levels
  # timeline2$state <- factor(timeline2$state,c("0", "1", "9", "3", "11", "27", "19", "25", "17", "16"))
  # timeline2 <- timeline2[ order(timeline2$state), ]
  timeline2
}

#' Create an empty timeline
#'
#' Goes over the \code{events} and \code{slide_transitions} to produce a
#' timeline with one second intervals from \code{startDate} to \code{endDate}
#' that holds 6 state indicators: \code{hasExercise}, \code{connected},
#' \code{focus}, \code{idle}, \code{input} and \code{submitted}.
#'
#' @param from \code{POSIXct} First timestamp of the timeline.
#' @param to \code{POSIXct} Second timestamp of the timeline.
#' @param slide_transitions \code{data.frame} Contains slide transitions.
#' @return A \code{data.table} with the concatenated timelinesof all the users.
#' @export
create_time_datatable <- function(from, to, slide_transitions){
  if(anyNA(slide_transitions)) stop("argument slide_transitions is NA")
  if(is.null(slide_transitions)) stop("argument slide_transitions is NULL")
  if(length(slide_transitions)==0) stop("argument slide_transitions has 0 length")

  time_dt <- as.data.table(seq(from=from, to=to, by=1))
  setnames(time_dt,"x","time")
  time_dt[, time2 := time]
  setkey(time_dt, time, time2)

  ranges_dt <- as.data.table(slide_transitions)
  setkey(ranges_dt, time_start, time_end)
  ov <-foverlaps(time_dt, ranges_dt, by.x = c("time", "time2"),
                 by.y= c("time_start", "time_end"), type="within", mult="first", nomatch=0L)
  time_dt<- ov[, c("time", "slide", "hasExercise"), with = FALSE]

  # initialize values for states
  time_dt$connected = FALSE
  time_dt[, c("visible", "focus", "idle", "input", "submitted") ] = NA
  time_dt[hasExercise == T, submitted:= F ]
  time_dt[hasExercise == T, input:= F ]
  time_dt
}

#' Calculate user state for a bunch of users.
#'
#' Goes over the \code{events} and \code{slide_transitions} to produce a
#' timeline with one second intervals from \code{startDate} to \code{endDate}
#' that holds 6 state indicators: \code{hasExercise}, \code{connected},
#' \code{focus}, \code{idle}, \code{input} and \code{submitted}.
#'
#' @param users \code{date.frame} Users from a lecture.
#' @param events \code{date.frame} Events from \strong{single} users during the lecture.
#' @param startDate \code{POSIXct} Calculate state for timestamps since this date.
#' @param endDate \code{POSIXct} Calculate state for timestamps up to this date.
#' @param slide_transitions \code{data.frame} Contains slide transitions.
#' @return A \code{data.table} with the concatenated timelinesof all the users.
#' @export
calc_user_state_for_users <- function(users, events, startDate, endDate, slide_transitions){
  check_na_null_0length(users);
  check_na_null_0length(slide_transitions);

  proto_tm_dt <- create_time_datatable(startDate, endDate,  slide_transitions);

  l_user_state <-lapply(users, function(u){
    sessionevents_one_user <- events %>% filter(user == u ) %>% collect()
    if(nrow(sessionevents_one_user) == 0) {
      return (NA)
    }

    # copy a timeline
    time_dt <- proto_tm_dt
    dt <- calc_user_state(time_dt, sessionevents_one_user, startDate, endDate)
    dt %<>% mutate(user = u) %>%
      select(user, time, slide, hasExercise, connected, visible, focus, idle, input, submitted, state)
  })

  l_user_state <- l_user_state[!is.na(l_user_state)]

  # concatentate viewer state data frames for all users
  # into one data frame
  dt <- bind_rows(l_user_state)
  dt
}
