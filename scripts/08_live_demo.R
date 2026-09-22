# ================================================================
# COPUS Segmentation Framework
# 08_live_demo.R
#
# Purpose:
#   Provide a simple live-demo interface for selecting one
#   de-identified classroom observation by number and running the
#   complete segmentation function.
#
# Poster-demo workflow:
#   1. Open the repository as the RStudio project / working directory.
#   2. Run:
#        source("scripts/08_live_demo.R")
#   3. Ask a visitor to choose a number from 1 to n_demo_observations.
#   4. Run:
#        demo <- run_demo_observation(325)
#      To add ranked alternative-label bars:
#        demo <- run_demo_observation(325, show_alternatives = TRUE)
#
# Main outputs:
#   demo$segments        # final consecutive segments
#   demo$display_table   # final + alternative labels
#   demo$raw_copus       # original COPUS observation
#   demo$labeled_copus   # COPUS intervals joined with final labels
#   demo$plot            # COPUS matrix + final/optional alternative timelines
#   demo$result          # complete detect_segments() result object
# ================================================================

# ---- File locations ----------------------------------------------------------

COPUS_DEMO_FUNCTION_PATH <- "scripts/07_detect_segments_function.R"
COPUS_DEMO_DATA_PATH <- "data/copus_public_deidentified_dataset.csv"


# ---- Required packages -------------------------------------------------------

.demo_required_packages <- c("dplyr", "readr", "tibble", "tidyr", "ggplot2")
.demo_missing_packages <- .demo_required_packages[
  !vapply(.demo_required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(.demo_missing_packages) > 0L) {
  stop(
    "Please install the following required packages before sourcing this script: ",
    paste(.demo_missing_packages, collapse = ", "),
    call. = FALSE
  )
}

rm(.demo_required_packages, .demo_missing_packages)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(ggplot2)
})


# ---- Load the segmentation function -----------------------------------------

# REVISED: Always source the current detector file. This prevents a stale
# detect_segments() already present in the R environment from powering the demo.
if (!file.exists(COPUS_DEMO_FUNCTION_PATH)) {
  stop(
    "Could not find the segmentation function script at:\n  ",
    COPUS_DEMO_FUNCTION_PATH,
    "\nOpen the repository as the working directory or update ",
    "`COPUS_DEMO_FUNCTION_PATH`.",
    call. = FALSE
  )
}

source(COPUS_DEMO_FUNCTION_PATH)


# ---- Initialize demo data ----------------------------------------------------

initialize_copus_demo <- function(
    data_path = COPUS_DEMO_DATA_PATH
) {
  if (!file.exists(data_path)) {
    stop(
      "Could not find the de-identified COPUS dataset at:\n  ",
      data_path,
      "\nOpen the repository as the working directory or update ",
      "`COPUS_DEMO_DATA_PATH`.",
      call. = FALSE
    )
  }
  
  data <- readr::read_csv(
    data_path,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  # REVISED: Validate these columns before referring to them in mutate().
  if (!all(c("id", "time") %in% names(data))) {
    stop(
      "The demo dataset must contain `id` and `time` columns.",
      call. = FALSE
    )
  }
  
  data <- data %>%
    dplyr::mutate(
      id = as.character(.data$id),
      time = as.integer(.data$time)
    ) %>%
    dplyr::arrange(.data$id, .data$time)
  
  if (nrow(data) == 0L) {
    stop("The demo dataset contains no rows.", call. = FALSE)
  }
  
  session_index <- data %>%
    dplyr::distinct(.data$id) %>%
    dplyr::arrange(.data$id) %>%
    dplyr::mutate(
      demo_number = dplyr::row_number()
    ) %>%
    dplyr::select(
      "demo_number",
      "id"
    )
  
  list(
    data = data,
    session_index = session_index
  )
}


# Load the de-identified dataset once when this script is sourced.
# This avoids re-reading the CSV every time a visitor chooses a number.
copus_demo <- initialize_copus_demo()

demo_data <- copus_demo$data
demo_session_index <- copus_demo$session_index
n_demo_observations <- nrow(demo_session_index)


# ---- Validate a visitor-selected observation number -------------------------

validate_demo_number <- function(
    number,
    session_index = demo_session_index
) {
  if (
    length(number) != 1L ||
    !is.numeric(number) ||
    is.na(number) ||
    !is.finite(number) ||
    number %% 1 != 0
  ) {
    stop(
      "`number` must be one whole number between 1 and ",
      nrow(session_index),
      ".",
      call. = FALSE
    )
  }
  
  number <- as.integer(number)
  
  if (number < 1L || number > nrow(session_index)) {
    stop(
      "`number` must be between 1 and ",
      nrow(session_index),
      ". You entered ",
      number,
      ".",
      call. = FALSE
    )
  }
  
  number
}


# ---- Live-demo visualization -------------------------------------------------

COPUS_DEMO_PROFILE_PALETTE <- c(
  "TPS"             = "#009E73",
  "PeerInstruction" = "#E69F00",
  "Clicker"         = "#C44E52",
  "Lecture"         = "#0072B2",
  "PeerLite"        = "#FDB863",
  "ClickerLite"     = "#F4A3A3",
  "StudentWork"     = "#F0E442",
  "Admin"           = "#D65F9E",
  "StudentQA"       = "#6A51A3",
  "InstructorQA"    = "#B39DDB",
  "Transition"      = "#6E6E6E",
  "Unlabeled"       = "#D9D9D9"
)

COPUS_DEMO_INSTRUCTOR_CODES <- c(
  "Instructor.Lec", "Instructor.RtW", "Instructor.FUp", "Instructor.PQ",
  "Instructor.CQ", "Instructor.AnQ", "Instructor.MG", "Instructor.1o1",
  "Instructor.DV", "Instructor.Adm", "Instructor.W", "Instructor.Other"
)

COPUS_DEMO_STUDENT_CODES <- c(
  "Student.L", "Student.Ind", "Student.CG", "Student.WG", "Student.OG",
  "Student.AnQ", "Student.SQ", "Student.WC", "Student.Prd",
  "Student.SP", "Student.TQ", "Student.W", "Student.Other"
)

COPUS_DEMO_CODE_ORDER <- c(
  COPUS_DEMO_INSTRUCTOR_CODES,
  COPUS_DEMO_STUDENT_CODES
)

COPUS_DEMO_CODE_LABELS <- c(
  "Instructor.Lec" = "Lec",
  "Instructor.RtW" = "RtW",
  "Instructor.FUp" = "FUp",
  "Instructor.PQ" = "PQ",
  "Instructor.CQ" = "CQ",
  "Instructor.AnQ" = "AnQ",
  "Instructor.MG" = "MG",
  "Instructor.1o1" = "1o1",
  "Instructor.DV" = "D/V",
  "Instructor.Adm" = "Adm",
  "Instructor.W" = "W",
  "Instructor.Other" = "O",
  "Student.L" = "L",
  "Student.Ind" = "Ind",
  "Student.CG" = "CG",
  "Student.WG" = "WG",
  "Student.OG" = "OG",
  "Student.AnQ" = "AnQ",
  "Student.SQ" = "SQ",
  "Student.WC" = "WC",
  "Student.Prd" = "Prd",
  "Student.SP" = "SP",
  "Student.TQ" = "TQ",
  "Student.W" = "W",
  "Student.Other" = "O"
)


plot_demo_observation <- function(
    demo,
    gap_width = 0.8,
    show_alternatives = isTRUE(demo$show_alternatives),
    max_alternative_lanes = 3L
) {
  required_fields <- c(
    "session_number",
    "id",
    "labeled_copus"
  )
  
  # REVISED: `segments` was previously required but unused. Alternatives are
  # required only when their plot lanes are requested.
  if (isTRUE(show_alternatives)) {
    required_fields <- c(required_fields, "alternatives")
  }
  
  missing_fields <- setdiff(required_fields, names(demo))
  
  if (length(missing_fields) > 0L) {
    stop(
      "`demo` is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (
    length(max_alternative_lanes) != 1L ||
    !is.numeric(max_alternative_lanes) ||
    is.na(max_alternative_lanes) ||
    max_alternative_lanes < 1 ||
    (!is.infinite(max_alternative_lanes) &&
     max_alternative_lanes %% 1 != 0)
  ) {
    stop(
      "`max_alternative_lanes` must be a positive whole number or `Inf`.",
      call. = FALSE
    )
  }
  
  alternative_lane_limit <- if (is.infinite(max_alternative_lanes)) {
    Inf
  } else {
    as.integer(max_alternative_lanes)
  }
  
  missing_codes <- setdiff(
    COPUS_DEMO_CODE_ORDER,
    names(demo$labeled_copus)
  )
  
  if (length(missing_codes) > 0L) {
    stop(
      "The selected COPUS observation is missing plotting column(s): ",
      paste(missing_codes, collapse = ", "),
      call. = FALSE
    )
  }
  
  timeline_base <- demo$labeled_copus %>%
    dplyr::arrange(.data$time) %>%
    dplyr::mutate(
      profile = trimws(.data$Label),
      interval_num = dplyr::row_number(),
      run_id = .data$Segment_ID
    )
  
  segment_df <- timeline_base %>%
    dplyr::group_by(.data$id, .data$run_id, .data$profile) %>%
    dplyr::summarise(
      start_time = min(.data$time),
      end_time = max(.data$time),
      start_interval = min(.data$interval_num),
      end_interval = max(.data$interval_num),
      n_intervals = dplyr::n(),
      minutes = .data$n_intervals * 2L,
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$run_id) %>%
    dplyr::mutate(
      segment_label = paste0("S", dplyr::row_number()),
      start_x = cumsum(dplyr::lag(.data$n_intervals, default = 0L)) +
        (dplyr::row_number() - 1L) * gap_width + 1,
      end_x = .data$start_x + .data$n_intervals - 1,
      xmin = .data$start_x - 0.5,
      xmax = .data$end_x + 0.5,
      xmid = (.data$xmin + .data$xmax) / 2,
      profile = factor(
        .data$profile,
        levels = names(COPUS_DEMO_PROFILE_PALETTE)
      )
    )
  
  timeline_df <- timeline_base %>%
    dplyr::group_by(.data$run_id) %>%
    dplyr::mutate(
      within_segment_index = dplyr::row_number() - 1L
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      segment_df %>%
        dplyr::select(
          "run_id",
          "start_x",
          "segment_label"
        ),
      by = "run_id"
    ) %>%
    dplyr::mutate(
      x_plot = .data$start_x + .data$within_segment_index
    )
  
  # REVISED: Map alternative Start/End times onto the same x_plot coordinates
  # used by final segments and the COPUS matrix. This preserves exact alignment
  # even though visual gaps are inserted between final segments.
  alternative_df <- tibble::tibble()
  n_available_alternative_lanes <- 0L
  alternatives_truncated <- FALSE
  
  if (isTRUE(show_alternatives) && nrow(demo$alternatives) > 0L) {
    required_alternative_columns <- c(
      "Segment_ID",
      "Start",
      "End",
      "Alternative_Label",
      "Alternative_Rank"
    )
    
    missing_alternative_columns <- setdiff(
      required_alternative_columns,
      names(demo$alternatives)
    )
    
    if (length(missing_alternative_columns) > 0L) {
      stop(
        "`demo$alternatives` is missing plotting column(s): ",
        paste(missing_alternative_columns, collapse = ", "),
        call. = FALSE
      )
    }
    
    alternative_source <- demo$alternatives %>%
      dplyr::transmute(
        Segment_ID = as.integer(.data$Segment_ID),
        Start = as.integer(.data$Start),
        End = as.integer(.data$End),
        Alternative_Label = trimws(.data$Alternative_Label),
        Alternative_Rank = as.integer(.data$Alternative_Rank)
      )
    
    if (
      anyNA(alternative_source$Segment_ID) ||
      anyNA(alternative_source$Start) ||
      anyNA(alternative_source$End) ||
      anyNA(alternative_source$Alternative_Rank) ||
      any(alternative_source$Start > alternative_source$End) ||
      any(alternative_source$Alternative_Rank < 1L)
    ) {
      stop(
        "`demo$alternatives` contains invalid segment coordinates or ranks.",
        call. = FALSE
      )
    }
    
    unknown_alternative_labels <- setdiff(
      unique(alternative_source$Alternative_Label),
      names(COPUS_DEMO_PROFILE_PALETTE)
    )
    
    if (length(unknown_alternative_labels) > 0L) {
      stop(
        "No plot color was defined for alternative label(s): ",
        paste(unknown_alternative_labels, collapse = ", "),
        call. = FALSE
      )
    }
    
    n_available_alternative_lanes <- max(
      alternative_source$Alternative_Rank
    )
    
    alternatives_truncated <-
      is.finite(alternative_lane_limit) &&
      n_available_alternative_lanes > alternative_lane_limit
    
    x_lookup <- timeline_df %>%
      dplyr::transmute(
        Segment_ID = as.integer(.data$run_id),
        time = as.integer(.data$time),
        x_plot = .data$x_plot
      )
    
    alternative_df <- alternative_source %>%
      dplyr::filter(
        .data$Alternative_Rank <= alternative_lane_limit
      ) %>%
      dplyr::left_join(
        x_lookup %>%
          dplyr::rename(
            Start = time,
            alternative_start_x = x_plot
          ),
        by = c("Segment_ID", "Start")
      ) %>%
      dplyr::left_join(
        x_lookup %>%
          dplyr::rename(
            End = time,
            alternative_end_x = x_plot
          ),
        by = c("Segment_ID", "End")
      )
    
    if (
      anyNA(alternative_df$alternative_start_x) ||
      anyNA(alternative_df$alternative_end_x)
    ) {
      stop(
        "An alternative segment could not be aligned to the final timeline.",
        call. = FALSE
      )
    }
    
    alternative_df <- alternative_df %>%
      dplyr::mutate(
        xmin = .data$alternative_start_x - 0.5,
        xmax = .data$alternative_end_x + 0.5,
        profile = factor(
          .data$Alternative_Label,
          levels = names(COPUS_DEMO_PROFILE_PALETTE)
        )
      )
  }
  
  if (nrow(alternative_df) == 0L) {
    alternative_df <- tibble::tibble(
      Alternative_Rank = integer(),
      xmin = double(),
      xmax = double(),
      lane_ymin = double(),
      lane_ymax = double(),
      lane_ymid = double(),
      profile = factor(
        character(),
        levels = names(COPUS_DEMO_PROFILE_PALETTE)
      )
    )
  }
  
  raw_code_long <- timeline_df %>%
    dplyr::select(
      "id",
      "time",
      "interval_num",
      "x_plot",
      dplyr::all_of(COPUS_DEMO_CODE_ORDER)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(COPUS_DEMO_CODE_ORDER),
      names_to = "code",
      values_to = "present"
    ) %>%
    dplyr::mutate(
      present = as.numeric(.data$present),
      code_short = unname(COPUS_DEMO_CODE_LABELS[.data$code]),
      role = dplyr::if_else(
        grepl("^Instructor", .data$code),
        "Instructor",
        "Student"
      ),
      tile_fill = dplyr::if_else(
        .data$present == 1,
        "black",
        "white"
      )
    )
  
  instructor_y <- rev(seq(15, 26))
  student_y <- rev(seq(1, 13))
  
  code_y_df <- tibble::tibble(
    code = COPUS_DEMO_CODE_ORDER,
    code_short = unname(
      COPUS_DEMO_CODE_LABELS[COPUS_DEMO_CODE_ORDER]
    ),
    y_pos = c(instructor_y, student_y)
  )
  
  raw_code_long <- raw_code_long %>%
    dplyr::left_join(
      code_y_df,
      by = c("code", "code_short")
    )
  
  instructor_y_mid <- raw_code_long %>%
    dplyr::filter(.data$role == "Instructor") %>%
    dplyr::summarise(
      y = mean(range(.data$y_pos))
    ) %>%
    dplyr::pull(.data$y)
  
  student_y_mid <- raw_code_long %>%
    dplyr::filter(.data$role == "Student") %>%
    dplyr::summarise(
      y = mean(range(.data$y_pos))
    ) %>%
    dplyr::pull(.data$y)
  
  matrix_top_y <- max(code_y_df$y_pos, na.rm = TRUE)
  
  n_alternative_lanes <- if (nrow(alternative_df) == 0L) {
    0L
  } else {
    max(alternative_df$Alternative_Rank)
  }
  
  # REVISED: Alternative_Rank represents a distinct candidate label within
  # each final segment. Disconnected runs of the same label share one lane,
  # while lane ranking restarts within every final Segment_ID.
  if (n_alternative_lanes > 0L) {
    alternative_lane_height <- 0.68
    alternative_lane_step <- 0.88
    alternative_lane_base <- matrix_top_y + 1.2
    
    alternative_df <- alternative_df %>%
      dplyr::mutate(
        lane_ymin = alternative_lane_base +
          (n_alternative_lanes - .data$Alternative_Rank) *
          alternative_lane_step,
        lane_ymax = .data$lane_ymin + alternative_lane_height,
        lane_ymid = (.data$lane_ymin + .data$lane_ymax) / 2
      )
    
    alternative_top_y <- max(alternative_df$lane_ymax)
    segment_label_y <- alternative_top_y + 0.65
    timeline_bar_ymin <- segment_label_y + 0.65
    timeline_bar_ymax <- timeline_bar_ymin + 1.0
    interval_num_y <- timeline_bar_ymax + 1.0
    class_label_y <- interval_num_y + 1.2
  } else {
    # Preserve the original final-only layout when no alternatives are shown.
    timeline_bar_ymin <- matrix_top_y + 4.0
    timeline_bar_ymax <- matrix_top_y + 5.0
    interval_num_y <- matrix_top_y + 6.0
    segment_label_y <- matrix_top_y + 2.9
    class_label_y <- matrix_top_y + 7.2
  }
  
  x_min <- min(timeline_df$x_plot, na.rm = TRUE) - 0.5
  x_max <- max(timeline_df$x_plot, na.rm = TRUE) + 0.5
  
  left_label_x <- x_min - 3.8
  code_label_x <- x_min - 1.1
  
  timeline_row_labels <- tibble::tibble(
    row_label = "Final",
    y = (timeline_bar_ymin + timeline_bar_ymax) / 2
  )
  
  if (n_alternative_lanes > 0L) {
    alternative_row_labels <- alternative_df %>%
      dplyr::distinct(.data$Alternative_Rank, .data$lane_ymid) %>%
      dplyr::arrange(.data$Alternative_Rank) %>%
      dplyr::transmute(
        row_label = paste0("Alt ", .data$Alternative_Rank),
        y = .data$lane_ymid
      )
    
    timeline_row_labels <- dplyr::bind_rows(
      timeline_row_labels,
      alternative_row_labels
    )
  }
  
  # REVISED: Include profiles appearing only as alternatives in the legend.
  present_profiles <- unique(c(
    as.character(segment_df$profile),
    as.character(alternative_df$profile)
  ))
  
  present_profiles <- intersect(
    names(COPUS_DEMO_PROFILE_PALETTE),
    present_profiles
  )
  
  plot_subtitle <- if (n_alternative_lanes > 0L) {
    "Raw COPUS behaviors, final segments, and priority-masked alternatives"
  } else {
    "Raw COPUS behaviors and detected instructional segments"
  }
  
  plot_caption <- if (isTRUE(alternatives_truncated)) {
    paste0(
      "Alternative bars show the top ",
      alternative_lane_limit,
      " ranks; complete alternatives remain in demo$display_table."
    )
  } else {
    NULL
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = raw_code_long,
      ggplot2::aes(
        x = .data$x_plot,
        y = .data$y_pos
      ),
      fill = raw_code_long$tile_fill,
      color = "black",
      linewidth = 0.25,
      width = 1,
      height = 1
    ) +
    ggplot2::geom_text(
      data = code_y_df,
      ggplot2::aes(
        x = code_label_x,
        y = .data$y_pos,
        label = .data$code_short
      ),
      hjust = 1,
      size = 3.5
    ) +
    ggplot2::geom_text(
      data = timeline_row_labels,
      ggplot2::aes(
        x = code_label_x,
        y = .data$y,
        label = .data$row_label
      ),
      hjust = 1,
      size = 3.5,
      fontface = "bold"
    ) +
    ggplot2::geom_rect(
      data = segment_df,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = timeline_bar_ymin,
        ymax = timeline_bar_ymax,
        fill = .data$profile
      ),
      # REVISED: A dark, heavier border distinguishes final from alternatives.
      color = "#1A1A1A",
      linewidth = 0.7
    ) +
    ggplot2::geom_rect(
      data = alternative_df,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$lane_ymin,
        ymax = .data$lane_ymax,
        fill = .data$profile
      ),
      color = "#666666",
      linewidth = 0.25,
      alpha = 1
    ) +
    ggplot2::geom_text(
      data = timeline_df,
      ggplot2::aes(
        x = .data$x_plot,
        y = interval_num_y,
        label = .data$interval_num
      ),
      size = 3.5,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      data = segment_df,
      ggplot2::aes(
        x = .data$xmid,
        y = segment_label_y,
        label = .data$segment_label
      ),
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = x_min,
      y = class_label_y,
      label = "| beginning of class",
      hjust = 0,
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = x_max,
      y = class_label_y,
      label = "end of class |",
      hjust = 1,
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = left_label_x,
      y = instructor_y_mid,
      label = "Instructor COPUS Codes",
      angle = 90,
      fontface = "bold",
      size = 4
    ) +
    ggplot2::annotate(
      "text",
      x = left_label_x,
      y = student_y_mid,
      label = "Student COPUS Codes",
      angle = 90,
      fontface = "bold",
      size = 4
    ) +
    ggplot2::scale_fill_manual(
      values = COPUS_DEMO_PROFILE_PALETTE,
      breaks = present_profiles,
      drop = TRUE,
      name = "Detector"
    ) +
    # REVISED: Keep plot-bar borders, but show clean borderless legend swatches.
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(
          color = NA,
          linewidth = 0,
          alpha = 1
        )
      )
    ) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      limits = c(0, class_label_y + 1),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(left_label_x - 0.5, x_max),
      breaks = NULL,
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = paste0(
        "Observation ",
        demo$session_number,
        " (",
        demo$id,
        ")"
      ),
      subtitle = plot_subtitle,
      caption = plot_caption,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.key.size = grid::unit(0.8, "cm"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        color = "#555555"
      ),
      plot.margin = ggplot2::margin(10, 10, 10, 40)
    )
}


# ---- Run one live demo -------------------------------------------------------

run_demo_observation <- function(
    number,
    show_alternatives = FALSE,
    show_plot = TRUE,
    print_results = TRUE,
    data = demo_data,
    session_index = demo_session_index,
    max_alternative_lanes = 3L
) {
  number <- validate_demo_number(
    number = number,
    session_index = session_index
  )
  
  selected_id <- session_index$id[
    session_index$demo_number == number
  ]
  
  dat <- data %>%
    dplyr::filter(.data$id == selected_id) %>%
    dplyr::arrange(.data$time)
  
  if (nrow(dat) == 0L) {
    stop(
      "No COPUS rows were found for demo observation ",
      number,
      " (",
      selected_id,
      ").",
      call. = FALSE
    )
  }
  
  segmentation_start <- proc.time()[["elapsed"]]
  
  segmentation_result <- detect_segments(dat)
  
  segmentation_runtime_seconds <- unname(
    proc.time()[["elapsed"]] - segmentation_start
  )
  
  labeled_copus <- dat %>%
    dplyr::left_join(
      segmentation_result$intervals %>%
        dplyr::select(
          "id",
          "time",
          "Segment_ID",
          "Label"
        ),
      by = c("id", "time")
    )
  
  demo_result <- list(
    session_number = number,
    id = selected_id,
    n_observations_available = nrow(session_index),
    n_intervals = nrow(dat),
    runtime_seconds = segmentation_runtime_seconds,
    segmentation_runtime_seconds = segmentation_runtime_seconds,
    raw_copus = dat,
    labeled_copus = labeled_copus,
    segments = segmentation_result$segments,
    display_table = segmentation_result$display_table,
    alternatives = segmentation_result$alternatives,
    result = segmentation_result,
    show_alternatives = isTRUE(show_alternatives),
    show_plot = isTRUE(show_plot),
    max_alternative_lanes = max_alternative_lanes
  )
  
  class(demo_result) <- c(
    "copus_live_demo_result",
    "list"
  )
  
  plot_start <- proc.time()[["elapsed"]]
  
  # NOTE: The plot is deliberately constructed even when show_plot = FALSE so
  # callers can still inspect or save demo_result$plot after a silent run.
  demo_result$plot <- plot_demo_observation(
    demo = demo_result,
    show_alternatives = demo_result$show_alternatives,
    max_alternative_lanes = demo_result$max_alternative_lanes
  )
  
  demo_result$plot_runtime_seconds <- unname(
    proc.time()[["elapsed"]] - plot_start
  )
  
  demo_result$total_runtime_seconds <-
    demo_result$segmentation_runtime_seconds +
    demo_result$plot_runtime_seconds
  
  if (isTRUE(print_results)) {
    print(
      demo_result,
      show_alternatives = show_alternatives,
      show_plot = show_plot
    )
  }
  
  invisible(demo_result)
}

# ---- Print method for a clean poster presentation ----------------------------

print.copus_live_demo_result <- function(
    x,
    ...,
    show_alternatives = x$show_alternatives,
    show_plot = x$show_plot
) {
  cat("\n")
  cat("COPUS live segmentation demo\n")
  cat("----------------------------\n")
  cat(
    "Selected observation: ",
    x$session_number,
    " of ",
    x$n_observations_available,
    "\n",
    sep = ""
  )
  cat("De-identified ID: ", x$id, "\n", sep = "")
  cat("COPUS intervals: ", x$n_intervals, "\n", sep = "")
  cat(
    "Segmentation runtime: ",
    format(
      round(x$segmentation_runtime_seconds, 3),
      nsmall = 3
    ),
    " seconds\n",
    sep = ""
  )
  cat(
    "Plot construction runtime: ",
    format(
      round(x$plot_runtime_seconds, 3),
      nsmall = 3
    ),
    " seconds\n\n",
    sep = ""
  )
  
  if (isTRUE(show_alternatives)) {
    cat("Final segments and priority-masked alternatives:\n")
    print(x$display_table, ...)
  } else {
    cat("Final instructional segments:\n")
    print(x$segments, ...)
  }
  
  if (isTRUE(show_plot)) {
    print(x$plot)
  }
  
  invisible(x)
}

# ---- Optional convenience helpers -------------------------------------------

show_demo_index <- function(
    first = 1L,
    last = min(first + 19L, n_demo_observations)
) {
  first <- as.integer(first)
  last <- as.integer(last)
  
  if (
    is.na(first) ||
    is.na(last) ||
    first < 1L ||
    last > n_demo_observations ||
    first > last
  ) {
    stop(
      "`first` and `last` must define a valid range between 1 and ",
      n_demo_observations,
      ".",
      call. = FALSE
    )
  }
  
  demo_session_index %>%
    dplyr::filter(
      .data$demo_number >= first,
      .data$demo_number <= last
    )
}


sample_demo_number <- function(seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  sample.int(
    n = n_demo_observations,
    size = 1L
  )
}


# ---- Ready message -----------------------------------------------------------

message(
  "COPUS live demo is ready: ",
  n_demo_observations,
  " de-identified observations available."
)
message(
  "Run, for example: demo <- run_demo_observation(325)"
)
message(
  "When you run a demo, the final segment table will appear in the ",
  "Console and the COPUS timeline will appear in the Plots pane."
)
