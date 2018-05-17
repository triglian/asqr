#' group by user and state and add fields for pie charts
#' @export
group_by_user_state <- function(data){
  #         d.ids <- data %>% select(user) %>% distinct() %>% arrange(user)
  #         d.ids$user.name <- paste0("viewer #", seq(1, nrow(d.ids)))


  res <- data %>%
    group_by(user, user_name, state) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(user) %>%
    arrange(state) %>%
    mutate(perc= paste0(round(count/sum(count)*100, 2),"%"),
           csum= cumsum(count),
           at= csum -0.5*count) %>%
    # left_join(d.ids) %>%
    collect()
  res
}

# # group by state. time and slide
# by_user_state_time_dt <- dt %>%
#         group_by(user, state) %>%
#         summarise(count = n()) %>%
#         ungroup() %>%
#         group_by(user) %>%
#         arrange(state) %>%
#         # add percentage, cumulative sum and position for the label on a pie chart
#         mutate(perc= paste0(round(count/sum(count)*100, 2),"%"),
#                csum= cumsum(count),
#                at= csum -0.5*count) %>%
#         collect()
# d.ids <- by_user_state_time_dt %>% select(user) %>% distinct() %>% arrange(user)
# d.ids$user_name <- paste0("viewer #", seq(1, nrow(d.ids)))
# by_user_state_time_dt <- by_user_state_time_dt %>% left_join(d.ids) %>% collect()


#' group by time
#' @export
group_by_time <- function(data){
  res <- data %>%
    group_by(time, hasExercise) %>%
    summarise(
      connected = sum(as.numeric(connected)),
      visible = sum(as.numeric(visible)),
      focus = sum(as.numeric(focus)),
      idle = sum(as.numeric(idle)),
      input = sum(as.numeric(input)),
      submitted = sum(as.numeric(submitted))
    ) %>%
    collect()
  res
}

#' group by state and time
#' @export
group_by_state_time <- function(data){
  res <- data %>%
    group_by(state, time, hasExercise) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    complete(state, nesting(time, hasExercise), fill=list(count=0)) %>%
    group_by(state, time, hasExercise) %>%
    collect()
  res
}

#' group by state. time and slide
#' @export
group_by_state_time_slide <- function(data){
  res <- data %>%
    group_by(state, time, slide) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    complete(state, nesting(time, slide), fill=list(count=0)) %>%
    group_by(state, time, slide) %>%
    collect()
  res
}


#' TODO: document
#'
#'   for each second that there is no observation for, between the min and
#' max time of timestamps in time_dt, inject observations with 0 count.
#' This stops plots from interpolating data.
#' @export
pad_timeline_with_0_count_states <- function(time_dt){
  res <- time_dt
  times <- time_dt %>% ungroup() %>% select(time) %>% distinct() %>% arrange(time) %>% collect()

  states <- time_dt %>% ungroup() %>% select(state) %>% distinct() %>% collect()
  alltimes <- seq(from=min(times$time), to=max(times$time), by=1)

  diffs <- alltimes[! alltimes %in% times$time ]
  if(length(diffs) >0){
    padding <- data.frame(state = rep(states$state, each=length(diffs)),
                          time = rep(diffs, length(states$state)) , count=0)
    res <- res %>% bind_rows(padding)
  }
  res
}

#' insert an entry with zero count for each state for each missing timestamp
#' @export
pad_timeline_with_0_count_states_by_slide <- function(time_dt){
  res <- time_dt
  time_dt %>%
    ungroup %>%
    group_by(slide) %>%
    do({
      sl <- .$slide[1]
      curr <- .
      times <- curr %>% select(time) %>% distinct() %>% arrange(time) %>% collect()

      states <- curr %>% select(state) %>% distinct() %>% collect()
      alltimes <- seq(from=min(times$time), to=max(times$time), by=1)

      diffs <- alltimes[! alltimes %in% times$time ]
      if(length(diffs) >0){
        padding <- data.frame(state = rep(states$state, each=length(diffs)),
                              time = rep(diffs, length(states$state)) , slide=sl , count=0)
        res <<- res %>% bind_rows(padding)
      }
      .
    })
  res
}

#' calculate the y min y max for each group (by state)
#'
#' intended to be used in a ribbon plot
#' @export
calc_y_min_max_for_ribbon_plot <- function(time_dt){
  res <- time_dt
  res$ymax <-res$count
  res$ymin <- 0
  res$state = as.factor(res$state)
  statel <- levels(res$state)
  for ( i in 2:length(statel) ) {
    state_i <- res$state==statel[i]
    state_i_1 <- res$state==statel[i-1]
    res$ymin[state_i] <- res$ymax[state_i_1]
    res$ymax[state_i] <- res$ymin[state_i] + res$ymax[state_i]
  }
  res
}

#' Prepare date for state plots.
#'
#' Utility function that reshapes user state data in the
#' shape needed by the plotting functions. Shapes:
#' \itemize{
#'  \item{\strong{Per user state over time}.}{ Calls \code{\link{group_by_user_state}}.}
#'  \item{\strong{State over time}.}{ Calls \code{\link{group_by_state_time}}.}
#'  \item{\strong{State per slide over time}.}{ Calls \code{\link{group_by_state_time_slide}}.}
#' }
#' @param dt \code{data.table} user state
#' @return  A \code(list) with
#' \itemize{
#'  \item{\code{dt}}{ The original \code{dt}}
#'  \item{\code{by_user_state_time_dt}}{ Per user state over time}
#'  \item{\code{by_state_time_dt}}{ State over time.}
#'  \item{\code{by_state_time_slide_dt}}{ State per slide over time.}
#' }
#' @export
prepare_data_for_state_plots <- function(dt){
  d.ids <- dt %>% select(user) %>% distinct() %>% arrange(user)
  d.ids$user_name <- paste0("viewer #", seq(1, nrow(d.ids)))
  dt <- dt %>% left_join(d.ids)

  by_user_state_time_dt <-group_by_user_state(dt)
  by_state_time_dt <- group_by_state_time(dt)
  by_state_time_slide_dt <- group_by_state_time_slide(dt)

  by_state_time_dt <- pad_timeline_with_0_count_states(by_state_time_dt)
  by_state_time_dt <- calc_y_min_max_for_ribbon_plot(by_state_time_dt)

  by_state_time_slide_dt <- pad_timeline_with_0_count_states_by_slide(by_state_time_slide_dt)
  by_state_time_slide_dt <- calc_y_min_max_for_ribbon_plot(by_state_time_slide_dt)

  by_user_state_time_dt <- by_user_state_time_dt[ order(by_user_state_time_dt$state), ]
  by_state_time_dt <- by_state_time_dt[ order(by_state_time_dt$state), ]
  by_state_time_slide_dt <- by_state_time_slide_dt[ order(by_state_time_slide_dt$state), ]

  list(
    dt = dt,
    by_user_state_time_dt = by_user_state_time_dt,
    by_state_time_dt = by_state_time_dt,
    by_state_time_slide_dt = by_state_time_slide_dt)
}


#################### PLOTTING FUNCTIONS #############################

# my colors
# get_color_scale <- function(){
#         c("0" = "#B7B7B7",
#           "1" = "#666666",
#           "2" = "#F4CCCC",
#           "3" = "#E06666",
#           "6" = "#B6D7A8",
#           "7" = "#FCE5CD",
#           # "10" = "#F4CCCC",
#           # "11" = "#E06666",
#           "14" = "#6AA84F",
#           # "15" = "#FCE5CD",
#           "23" = "#3C78D8",
#           # FFF2CC",
#           "33" = "#434343",
#           "35" = "#990000",
#           "39" = "#F6B26B",
#           # "43" = "990000",
#           # "47" = "#F6B26B",
#           "55" = "#FFD966")
# }

# rbrewer paired colors
# get_color_scale <- function(){
#   c("0" = "#A6CEE3",
#     "1" = "#1F78B4",
#     "2" = "#B2DF8A",
#     "3" = "#33A02C",
#     "6" = "#FB9A99",
#     "7" = "#E31A1C",
#     # "10" = "#F4CCCC",
#     # "11" = "#E06666",
#     "14" = "#FDBF6F",
#     # "15" = "#FCE5CD",
#     "23" = "#FF7F00",
#     # FFF2CC",
#     "33" = "#CAB2D6",
#     "35" = "#6A3D9A",
#     "39" = "#FFFF99",
#     # "43" = "990000",
#     # "47" = "#F6B26B",
#     "55" = "#B15928")
# }

get_color_scale <- function(){
  c("0" = "#C0C0C0",
    "1" = "#999999",
    "2" = "#B2DF8A",
    "3" = "#33A02C",
    "6" = "#A6CEE3",
    "7" = "#519EC8",
    "14" = "#FB9A99",
    "15" = "#E31A1C",
    # "18" = "Distracted (idle)",
    # "19" = "Distracted (on exercise)",
    # "22" = "Slide Visible",
    # "23" = "Question Visible",
    "30" = "#FDBF6F",
    "31" = "#BA1A1C", # custom
    "47" = "#FF7F00",
    "65" = "#CAB2D6",
    "67" = "#6A3D9A",
    "71" = "#1F78B4",
    "79" = "#FFFF99",
    # "83" = "Disengaged (idle)",
    # "87" = "Bored",
    # "95" = "Waiting (idle)",
    "111" = "#B15928")
}

#' TODO: Document
#' @export
# get_state_labels <- function(){
#   c("0" = "Disconnected",
#     "1" = "Disconnected on exercise slide",
#     "2" = "Distracted",
#     "3" = "Searching for a solution",
#     "6" = "Interacting with non-exercise slide",
#     "7" = "Thinking",
#     # "10" = "Distracted",
#     # "11" = "Searching",
#     "14" = "Following",
#     # "15" = "Thinking",
#     "23" = "Working",
#     "33" = "Disconnected",
#     "35" = "Bored",
#     "39" = "Waiting",
#     # "43" = "Bored",
#     # "47" = "Waiting",
#     "55" = "Reworking")
# }

get_state_labels <- function(){
  c("0" = "Disconnected",
    "1" = "Disconnected (on Question)",
    "2" = "Distracted",
    "3" = "Searching for a solution",
    "6" = "Slide Visible",
    "7" = "Question Visible",
    "14" = "Interacting with Slide",
    "15" = "Interacting with Question",
    # "18" = "Distracted (idle)",
    # "19" = "Distracted (on exercise)",
    # "22" = "Slide Visible",
    # "23" = "Question Visible",
    "30" = "Following",
    "31" = "Thinking",
    "47" = "Working",
    "65" = "Disconnected (on Answer)",
    "67" = "Disengaged (not idle)",
    "71" = "Answer Visible",
    "79" = "Waiting",
    # "83" = "Disengaged (idle)",
    # "87" = "Bored",
    # "95" = "Waiting (idle)",
    "111" = "Reworking")
}


#' faceted pie of user activity
#' @export
draw_activity_pie_by_user <- function(dt, state_labels){
  pie <- ggplot(dt, aes( x = factor(1), y=count, fill=state)) +
    geom_bar(width = 1, stat="identity") +
    # scale_fill_brewer(palette = "Set3", labels = state_labels) +
    scale_fill_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    coord_polar(theta = "y") +
    xlab("") +
    ylab("") +
    # annotate(geom = "text", y = at, x = 1, label = perc)+
    geom_text(aes(y = at, x = 1, label = perc, size=3))+
    theme(axis.text.y = element_blank(),
          axis.ticks.y=element_blank())+
    facet_wrap(~ user_name, ncol=12)
  pie
}


#' faceted bar of user activity
#' @export
draw_activity_bar_by_user <- function(dt, state_labels){
  bar <- ggplot(dt, aes( x = state, y=count, fill=state)) +
    geom_bar(stat="identity") +
    # scale_fill_brewer(name="state", palette = "Set3", labels = state_labels) +
    scale_fill_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    xlab("state") +
    ylab("count") +
    scale_x_discrete(labels = state_labels) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
    facet_wrap(~ user_name, ncol=12)
  bar
}



#' stacked bar of user activity as total events
#' @export
draw_activity_bar <- function(dt, state_labels){
  names_dt <- dt %>% select(user, user_name) %>% distinct(user_name)
  user_names <- names_dt$user_name
  stacked_bar <- ggplot(dt, aes( x = user, y=count, fill=state)) +
    geom_bar(stat="identity") +
    # scale_fill_brewer(name="state", palette = "Set3", labels = state_labels) +
    scale_fill_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    xlab("student") +
    ylab("count") +
    scale_x_discrete(labels = user_names) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1))
  stacked_bar
}


#' stacked bar of user activity over time
#' @export
draw_activity_bar_time <- function(dt, slide_transitions_with_ex, slide_transitions_without_ex, state_labels){
  names <- dt %>% select(user_name) %>% distinct()
  user_names <- names$user_name
  stacked_bar <- ggplot(dt, aes( x = time, y=user, fill=state, color=state))

  if(nrow(slide_transitions_with_ex) > 0){
    stacked_bar <- stacked_bar +
      # geom_rect(data=slide_transitions_with_ex,
      #                                aes(xmin=time_start, xmax=time_end, ymin=-Inf, ymax=Inf),
      #                                fill="lightblue", alpha=0.3, inherit.aes= FALSE) +
      geom_vline(data=slide_transitions_with_ex,
                 aes(xintercept = as.numeric(time_start)),
                 colour="lightblue", linetype=1, size=0.4, alpha=0.5)
  }

  stacked_bar <- stacked_bar + geom_vline(data=slide_transitions_without_ex,
                                          aes(xintercept = as.numeric(time_start)),
                                          colour="#cacaca", linetype=1, size=0.4, alpha=0.5) +
    geom_point(aes(color=state), shape=15) +
    # scale_fill_brewer(name="state", palette = "Set3", labels = state_labels) +
    # scale_colour_brewer(name="state", palette = "Set3", labels = state_labels) +
    scale_colour_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    scale_fill_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    ylab("student") +
    xlab("count") +
    scale_y_discrete(labels = user_names) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec")) )+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1),
          # legend.direction = "horizontal",
          legend.position = "top",
          legend.box = "horizontal")
  stacked_bar
}


#' stacked bar of user activity over time
#' @export
draw_activity_bar_time_faceted <- function(dt, slide_transitions_with_ex, slide_transitions_without_ex, state_labels){
  r_labels <- c("normal" = "without question",
                "exercise" = "with question",
                "break" = "break")

  r_scale <- c("exercise" = "#CD66E0",
               "normal" = "#FFFFFF",
               "break" = "#000000")

  names <- dt %>% select(user_name) %>% distinct()
  user_names <- names$user_name
  stacked_bar <- ggplot(dt, aes( x = time, y=0.5)) +
    geom_rect(aes(xmin=time,xmax=time+1,ymin=-0.05 ,ymax=0, fill=hasExercise))

  if(nrow(slide_transitions_with_ex) > 0){
    stacked_bar <- stacked_bar +
      geom_vline(data=slide_transitions_with_ex,
                 aes(xintercept = as.numeric(time_start)),
                 colour="#555555", linetype=1, size=0.4, alpha=0.5)
  }

  stacked_bar <- stacked_bar + geom_vline(data=slide_transitions_without_ex,
                                          aes(xintercept = as.numeric(time_start)),
                                          colour="#cacaca", linetype=1, size=0.4, alpha=0.5) +
    geom_line(aes(color=state, group=1), size=5) +
    # scale_colour_manual(values = r_scale,  name="slide", labels = r_labels) +
    # scale_fill_brewer(name="state", palette = "Paired", labels = state_labels) +
    # scale_colour_brewer(name="state", palette = "Paired", labels = state_labels) +
    scale_colour_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    scale_fill_manual(values = r_scale,  name="slide", labels = r_labels) +
    ylab("student") +
    xlab("time") +

    scale_x_datetime(labels =  date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec")) )+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.direction = "horizontal",
          legend.position = "top") +
    ylim(c(-0.05, 1)) +
    guides(col = guide_legend(override.aes = list(shape = 15, size = 8)))+
    coord_fixed(ratio = 100) +
    facet_grid(user_name~., scales="free")
  stacked_bar
}



#' stacked ribbon plot
#' @export
draw_activity_stacked_ribbon_by_slide <- function(dt, state_labels){
  stacked.ribbon <- ggplot(dt, aes(x=time,ymax=ymax,ymin=ymin, fill=state)) +
    geom_ribbon()+
    # scale_colour_brewer(palette = "Set3", name="state", labels = state_labels) +
    # scale_fill_brewer(palette = "Set3", name="state", labels = state_labels) +
    scale_colour_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    scale_fill_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    scale_x_datetime(labels = date_format("%H:%M:%S"),
                     breaks = date_breaks(paste(300, "sec")))+
    ylab("count(~= num of students)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),

          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    facet_wrap(~ slide, scales="free_x",  ncol=6)
  stacked.ribbon
}

#' TODO: document
#' @export
draw_activity_stacked_area_by_slide <- function(dt, state_labels){

  stacked_area_per_slide <- ggplot(dt, aes( x=time, y=count)) +
    geom_area(aes(fill= state), position = 'stack') +
    scale_colour_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    scale_fill_manual(values = get_color_scale(),  name="state", labels = state_labels) +

    # scale_colour_brewer(palette = "Set3", name="state", labels = state_labels) +
    # scale_fill_brewer(palette = "Set3", name="state", labels = state_labels) +
    scale_x_datetime(labels = date_format("%H:%M:%S"),
                     breaks = date_breaks(paste(10, "sec")))+
    ylab("count(~= num of students)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),

          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    facet_wrap(~ slide, scales="free_x", ncol=4)
  stacked_area_per_slide
}


#' stacked area plot
#' @export
draw_activity_stacked_area <- function(dt, slide_transitions_with_ex, slide_transitions_without_ex,
                                       state_labels, color_scale = get_color_scale(), num_students = 0,
                                       show_exercise_slide_overlay=TRUE, draw_states=TRUE,
                                       date_format="%H:%M", date_break_secs=300){
  r_labels <- c("normal" = "without question",
                "exercise" = "with question",
                "break" = "break")

  r_scale <- c("exercise" = "#CD66E0",
               "normal" = "#FFFFFF",
               "break" = "#000000")
  stacked_area <- ggplot(dt, aes( x=time, y=count)) +
    geom_segment(aes(x=time,xend=time+1,y=-0.02 * max(dt$count),yend=0, colour=hasExercise))

  if(num_students <= 0){
    num_students <- max(dt$ymax)
  }

  if(draw_states == TRUE){
    stacked_area <- stacked_area + geom_area(aes( fill= state), position = 'stack')
  }

  #         stacked_area <- ggplot(dt, aes(x=time,ymax=ymax,ymin=ymin, fill=state)) +
  #                 geom_ribbon()
  if(nrow(slide_transitions_with_ex) > 0){
    if(show_exercise_slide_overlay == TRUE){
      stacked_area <- stacked_area +
        geom_rect(data=slide_transitions_with_ex,
                  aes(xmin=time_start, xmax=time_end, ymin=-Inf, ymax=Inf),
                  fill="lightblue", alpha=0.3, inherit.aes= FALSE)
    }
    stacked_area <- stacked_area +
      geom_vline(data=slide_transitions_with_ex,
                 aes(xintercept = as.numeric(time_start)),
                 colour="lightblue", linetype=1, size=0.4, alpha=0.5)
  }

  stacked_area <- stacked_area +
    geom_vline(data=slide_transitions_without_ex,
               aes(xintercept = as.numeric(time_start)),
               colour="#cacaca", linetype=1, size=0.4, alpha=0.5) +
    # scale_colour_brewer(palette = "Set3", name="state", labels = state_labels) +
    scale_colour_manual(values = r_scale,  name="", labels = r_labels) +
    # scale_fill_brewer(palette = "Paired", name="states", labels = state_labels) +
    scale_fill_manual(values = color_scale,  name="", labels = state_labels) +
    ylim(c(-0.02 * max(dt$count), num_students)) +
    scale_x_datetime(labels = date_format(date_format, tz = "CET"),
                     breaks = date_breaks(paste(date_break_secs, "sec")))+
    ylab("number of students") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom") +
    guides(fill=guide_legend(nrow=3))
  stacked_area
}

#' stacked area plot
#' @export
draw_connected_focus_area <- function(dt, slide_transitions_with_ex, slide_transitions_without_ex,
                                      state_labels, color_scale = get_color_scale()){

  r_labels <- c("normal" = "without question",
                "exercise" = "with question",
                "break" = "break")

  r_scale <- c("exercise" = "#CD66E0",
               "normal" = "#FFFFFF",
               "break" = "#000000")

  stacked_area <- ggplot(dt) +
    geom_area(aes( x=time, y=count, fill= state)) +
    geom_segment(aes(x=time,xend=time+1,y=-0.02 * max(dt$count),yend=0, colour=hasExercise))
  # geom_rect(aes(xmax=time+1, xmin=time, ymax=0, ymin=-2, fill=hasExercise), show.legend = TRUE)

  if(nrow(slide_transitions_with_ex) > 0){
    stacked_area <- stacked_area +
      geom_vline(data=slide_transitions_with_ex,
                 aes(xintercept = as.numeric(time_start)),
                 colour="lightblue", linetype=1, size=0.4, alpha=0.5)
  }

  stacked_area <- stacked_area +
    geom_vline(data=slide_transitions_without_ex,
               aes(xintercept = as.numeric(time_start)),
               colour="#cacaca", linetype=1, size=0.4, alpha=0.5) +
    scale_colour_manual(values = r_scale,  name="slide", labels = r_labels) +
    scale_fill_manual(values = color_scale,  name="state", labels = state_labels) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec")))+
    ylab("number of students") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.direction = "horizontal",
          legend.position = "top",
          legend.box = "horizontal")
  # guides(fill=guide_legend(title.position="top"),
  #        colour=guide_legend(title.position="top"))
  stacked_area
}


#' line plot
#' @export
draw_activity_lines <- function(dt, slide_transitions_with_ex, slide_transitions_without_ex, state_labels){
  line_g <- ggplot(dt, aes( x=time, y=count))

  if(nrow(slide_transitions_with_ex) > 0){
    line_g <- line_g +
      # geom_rect(data=slide_transitions_with_ex,
      #                      aes(xmin=time_start, xmax=time_end, ymin=-Inf, ymax=Inf),
      #                      fill="lightblue", alpha=0.3, inherit.aes= FALSE) +
      geom_vline(data=slide_transitions_with_ex,
                 aes(xintercept = as.numeric(time_start)),
                 colour="lightblue", linetype=1, size=0.4, alpha=0.5)
  }


  line_g <- line_g + geom_vline(data=slide_transitions_without_ex,
                                aes(xintercept = as.numeric(time_start)),
                                colour="#cacaca", linetype=1, size=0.4, alpha=0.5) +
    geom_line(aes(colour = state, colour= state)) +
    # scale_colour_brewer(palette = "Set3", name="state", labels = state_labels) +
    # scale_fill_brewer(palette = "Set3", name="state", labels = state_labels) +
    scale_colour_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    scale_fill_manual(values = get_color_scale(),  name="state", labels = state_labels) +
    scale_x_datetime(labels = date_format("%H:%M:%S"),
                     breaks = date_breaks(paste(300, "sec")) )+
    ylab("count(~= num of students)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),

          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  line_g
}


#' TODO: document
#' @export
draw_attention_focus_variation <- function(dt, slide_transitions_with_ex, slide_transitions_without_ex){

  r_labels <- c("normal" = "without question",
                "exercise" = "with question",
                "break" = "break")

  r_scale <- c("exercise" = "#CD66E0",
               "normal" = "#00FF00",
               "break" = "#000000")

  p <- ggplot(dt, aes( x = time, y=rol.diff.sum, group = 1, colour=hasExercise)) +
    geom_line()

  if(nrow(slide_transitions_with_ex) > 0){

    p <- p +
      geom_vline(data=slide_transitions_with_ex,
                 aes(xintercept = as.numeric(time_start)),
                 colour="#cacaca", linetype=1, size=0.4, alpha=0.5)
  }

  p<- p + geom_vline(data=slide_transitions_without_ex,
                     aes(xintercept = as.numeric(time_start)),
                     colour="#cacaca", linetype=1, size=0.4, alpha=0.5) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec")) )+
    scale_colour_manual(values = r_scale,  name="slide", labels = r_labels) +
    ylab("Attention Focus Variation") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1),
          legend.direction = "horizontal",
          legend.position = "top",
          legend.box = "horizontal")
  p
}

#' Prepare state timeline data for stacked area plotting
#'
#' Groups by state and time, adds entries with count of 0 for the
#' states that don't appear in the data, calculates the height of
#' each ribbon and reorders the states
#' @param dt timeline data.table with state information
#' @return a data.table appropriate for the stacked plot
#' @export
format_timeline_for_state_time_plotting <- function (dt){
  by_state_time_dt <- group_by_state_time(dt)
  by_state_time_dt <- pad_timeline_with_0_count_states(by_state_time_dt)
  by_state_time_dt <- calc_y_min_max_for_ribbon_plot(by_state_time_dt)
  by_state_time_dt <- by_state_time_dt[ order(by_state_time_dt$state), ]
  by_state_time_dt
}
