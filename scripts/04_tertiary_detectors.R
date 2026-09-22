# ==================================================
# COPUS Segmentation Framework
# 04_tertiary_detectors.R
# Purpose: Run tertiary detectors
# ==================================================
source("scripts/00_setup.R")

# ==== LOAD PRIMARY/SECONDARY LABEL CACHE ======================================

cache_02_path <- "cache/02_primary_secondary_labels.rds"

if (!file.exists(cache_02_path)) {
  stop(
    "Missing cache file: ",
    cache_02_path,
    "\nRun scripts/01_primary_secondary_detectors.R and ",
    "scripts/02_label_precedence_residual.R first.",
    call. = FALSE
  )
}

cache_02 <- readRDS(cache_02_path)

required_cache_02_objects <- c(
  "master_data",
  "unlabeled_segments"
)

missing_cache_02_objects <- setdiff(
  required_cache_02_objects,
  names(cache_02)
)

if (length(missing_cache_02_objects) > 0L) {
  stop(
    "Cache 02 is missing required object(s): ",
    paste(missing_cache_02_objects, collapse = ", "),
    "\nRe-run scripts/01_primary_secondary_detectors.R and ",
    "scripts/02_label_precedence_residual.R.",
    call. = FALSE
  )
}

master_data <- cache_02$master_data
unlabeled_segments <- cache_02$unlabeled_segments

rm(
  cache_02,
  cache_02_path,
  required_cache_02_objects,
  missing_cache_02_objects
)

message(
  "Loaded primary/secondary label cache: ",
  "cache/02_primary_secondary_labels.rds"
)

# ==== TERTIARY DETECTORS ====
# Helper for Tertiary Detectors --------------------------------
# Used by Instructor QA, Student QA, and Transition detectors
#
# Brief description:
# When no segment is detected, return a standardized empty result
# with the same columns and data types as the regular detector output.

empty_tertiary_segments <- function() {
  tibble::tibble(
    id          = character(),
    start_time  = integer(),
    end_time    = integer(),
    n_intervals = integer(),
    minutes     = integer(),
    type        = character()
  )
}

# Instructor QA ----------------
# Logic:
#   Instructor QA begins with an instructor-posed question (PQ)
#   accompanied by student answering or listening.
#
#  Anchor: Instructor.PQ AND (Student.AnQ OR Student.L)
#  Continuation: Instructor.PQ/FUp/RtW/AnQ  AND  Student.AnQ/SQ/L
#
# Allows:
#   A single 2-minute Instructor QA interval
#   Multiple contiguous QA intervals

# 1) Build Instructor QA flags on one residual block

build_instructorQA_flags <- function(df_block) {
  df_block %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      
      # REVISED:
      # Instructor QA must be initiated by Instructor.PQ; FUp can no longer serve as an anchor.
      anchor_instructorQA =
        (`Instructor.PQ` == 1) & ((`Student.AnQ` == 1) | (`Student.L` == 1)),
      
      cont_instructorQA = ((`Instructor.PQ` == 1)  | (`Instructor.FUp` == 1) |
            (`Instructor.RtW` == 1) | (`Instructor.AnQ` == 1)) & 
            ((`Student.AnQ` == 1) | (`Student.SQ` == 1)  | (`Student.L` == 1)))
}


# 2) Scan one residual unlabeled block

scan_instructorQA_one_block <- function(dd_block,
                                        min_len = 1) {
  
  stopifnot(all(c("id","time","anchor_instructorQA","cont_instructorQA") %in% names(dd_block)))
  
  dd_block <- dd_block %>%
    dplyr::arrange(time)
  
  n   <- nrow(dd_block)
  i   <- 1
  out <- list()
  
  while (i <= n) {
    
    ## (A) Find the next valid Instructor QA anchor
    j <- i
    
    while (
      j <= n &&
      !dd_block$anchor_instructorQA[j]
    ) {
      j <- j + 1
    }
    
    if (j > n) break
    
    seg_start_idx <- j
    
    
    ## (B) Extend through contiguous continuation intervals
    
    # The anchor condition is a subset of the continuation condition,
    # so scanning can begin with the interval after the anchor.
    k <- seg_start_idx + 1
    
    while (
      k <= n &&
      dd_block$cont_instructorQA[k] &&
      
      # REVISED:
      # Require actual chronological adjacency.
      dd_block$time[k] == dd_block$time[k - 1] + 1
    ) {
      k <- k + 1
    }
    
    seg_end_idx <- k - 1
    
    
    ## (C) Calculate segment length using the actual number of rows
    
    # REVISED:
    # Row count is safer than end_time - start_time + 1,
    # especially when the data contain missing time bins.
    segment_length <- seg_end_idx - seg_start_idx + 1
    

    ## (D) Record segment when minimum length is satisfied
    
    if (segment_length >= min_len) {
      
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = segment_length,
        minutes     = segment_length * 2,
        type        = "InstructorQA"
      )
    }
    
    
    ## (E) Move to the first interval after this segment
    
    i <- seg_end_idx + 1
  }
  
  
  # Return standardized empty output when no segment is detected
  if (length(out) == 0) {
    empty_tertiary_segments()
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply detector across all residual unlabeled blocks 

detect_instructorQA_from_unlabeled <- function(master_data,
                                               unlabeled_segments,
                                               min_len = 1) {
  
  # No residual unlabeled blocks are available
  if (nrow(unlabeled_segments) == 0) {
    return(empty_tertiary_segments())
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>%
      dplyr::select(id, start_time, end_time),
    
    function(id, start_time, end_time) {
      
      # Extract the corresponding residual block
      block <- master_data %>%
        dplyr::filter(
          .data$id == !!id,
          .data$time >= !!start_time,
          .data$time <= !!end_time
        ) %>%
        dplyr::arrange(time)
      
      # Safety check for an empty residual block
      if (nrow(block) == 0) {
        return(empty_tertiary_segments())
      }
      
      # Build flags and scan the residual block
      dd <- build_instructorQA_flags(block)
      
      scan_instructorQA_one_block(
        dd_block = dd,
        min_len  = min_len
      )
    }
  ) %>%
    dplyr::arrange(id, start_time)
}


# 4) Run Instructor QA detector 

instructorQA_segments <- detect_instructorQA_from_unlabeled(
  master_data         = master_data,
  unlabeled_segments  = unlabeled_segments,
  min_len             = 1
)

instructorQA_segments

# Student QA ----------------------
# Logic:
#   Student QA begins with Student.SQ AND Instructor.AnQ. 
#
#   Anchor: Student.SQ AND Instructor.AnQ
#   Continuation: Instructor.AnQ/FUp/RtW AND Student.SQ/AnQ/L
#
# Allows:
#   A single 2-min Student QA interval
#   Multiple contiguous QA intervals

# 1) Build Student QA flags on one residual block 

build_studentQA_flags <- function(df_block) {
  
  df_block %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      
      # REVISED:
      # Student QA must begin with a student question; FUp can no longer serve as part of the anchor.
      anchor_studentQA =
        (`Student.SQ` == 1) &
        (`Instructor.AnQ` == 1),
      
      cont_studentQA =
        ((`Instructor.AnQ` == 1) | (`Instructor.FUp` == 1) | (`Instructor.RtW` == 1)) &
        ((`Student.SQ` == 1)  | (`Student.AnQ` == 1) | (`Student.L` == 1)))
}


# 2) Scan one residual unlabeled block 

scan_studentQA_one_block <- function(dd_block,
                                     min_len = 1) {
  
  stopifnot(
    all(c("id","time","anchor_studentQA","cont_studentQA") %in% names(dd_block)))
  
  dd_block <- dd_block %>%
    dplyr::arrange(time)
  
  n   <- nrow(dd_block)
  i   <- 1
  out <- list()
  
  while (i <= n) {
    
    ## (A) Find the next valid Student QA anchor
    
    j <- i
    
    while (
      j <= n &&
      !dd_block$anchor_studentQA[j]
    ) {
      j <- j + 1
    }
    
    if (j > n) break
    
    seg_start_idx <- j
    
    
    ## (B) Extend through contiguous continuation intervals
    
    # REVISED:
    # The anchor condition is a subset of the continuation condition,
    # so begin checking from the interval after the anchor.
    k <- seg_start_idx + 1
    
    while (
      k <= n &&
      dd_block$cont_studentQA[k] &&
      
      # REVISED:
      # Require actual chronological adjacency.
      dd_block$time[k] == dd_block$time[k - 1] + 1
    ) {
      k <- k + 1
    }
    
    seg_end_idx <- k - 1
    
    
    ## (C) Calculate segment length using actual row count
    
    # REVISED:
    # This avoids overestimating duration when time bins are missing.
    segment_length <- seg_end_idx - seg_start_idx + 1
    
    
    ## (D) Record the segment
    
    if (segment_length >= min_len) {
      
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = segment_length,
        minutes     = segment_length * 2,
        type        = "StudentQA"
      )
    }
    
    
    ## (E) Advance cursor
    
    i <- seg_end_idx + 1
  }
  
  
  # REVISED:
  # Use the shared empty-output helper.
  if (length(out) == 0) {
    empty_tertiary_segments()
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply detector across all residual unlabeled blocks

detect_studentQA_from_unlabeled <- function(master_data,
                                            unlabeled_segments,
                                            min_len = 1) {
  
  # REVISED:
  # Use the shared empty-output helper when no residual blocks exist.
  if (nrow(unlabeled_segments) == 0) {
    return(empty_tertiary_segments())
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>%
      dplyr::select(id, start_time, end_time),
    
    function(id, start_time, end_time) {
      
      # Extract the corresponding residual block
      block <- master_data %>%
        dplyr::filter(
          .data$id == !!id,
          .data$time >= !!start_time,
          .data$time <= !!end_time
        ) %>%
        dplyr::arrange(time)
      
      # Safety check for an empty residual block
      if (nrow(block) == 0) {
        return(empty_tertiary_segments())
      }
      
      # Build flags and scan the residual block
      dd <- build_studentQA_flags(block)
      
      scan_studentQA_one_block(
        dd_block = dd,
        min_len  = min_len
      )
    }
  ) %>%
    dplyr::arrange(id, start_time)
}


# 4) Run Student QA detector

studentQA_segments <- detect_studentQA_from_unlabeled(
  master_data        = master_data,
  unlabeled_segments = unlabeled_segments,
  min_len            = 1
)

studentQA_segments

# Transition ----------
# Logic: (Instructor.W OR Instructor.Other) AND (Student.W OR Student.Other)
#
# Allows:
#   A single 2-min Transition interval
#   Multiple contiguous Transition intervals

# 1) Build Transition flag on one residual block 

build_transition_flags <- function(df_block) {
  
  df_block %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      
      # REVISED:
      # No additional instructional-code exclusions are applied.
      phase_transition =
        ((`Instructor.W` == 1) | (`Instructor.Other` == 1)) &
        ((`Student.W` == 1) | (`Student.Other` == 1)))
}


# 2) Scan one residual unlabeled block 

scan_transition_one_block <- function(dd_block,
                                      min_len = 1) {
  
  stopifnot(all(c("id","time","phase_transition") %in% names(dd_block)))
  
  dd_block <- dd_block %>%
    dplyr::arrange(time)
  
  n   <- nrow(dd_block)
  i   <- 1
  out <- list()
  
  while (i <= n) {
    
    ## (A) Find the next Transition interval
    
    j <- i
    
    while (
      j <= n &&
      !dd_block$phase_transition[j]
    ) {
      j <- j + 1
    }
    
    if (j > n) break
    
    seg_start_idx <- j
    
    
    ## (B) Extend the contiguous Transition run
    
    k <- seg_start_idx + 1
    
    while (
      k <= n &&
      dd_block$phase_transition[k] &&
      
      # REVISED:
      # Require actual chronological adjacency.
      dd_block$time[k] == dd_block$time[k - 1] + 1
    ) {
      k <- k + 1
    }
    
    seg_end_idx <- k - 1
    
    
    ## (C) Calculate segment length using actual row count
    
    # REVISED:
    # Row count avoids overestimating duration if time bins are missing.
    segment_length <- seg_end_idx - seg_start_idx + 1
    
    
    ## (D) Record the Transition segment
    
    if (segment_length >= min_len) {
      
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = segment_length,
        minutes     = segment_length * 2,
        type        = "Transition"
      )
    }
    
    
    ## (E) Advance cursor
    
    i <- seg_end_idx + 1
  }
  
  
  # REVISED:
  # Use the shared empty-output helper.
  if (length(out) == 0) {
    empty_tertiary_segments()
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply detector across all residual unlabeled blocks

detect_transition_from_unlabeled <- function(master_data,
                                             unlabeled_segments,
                                             min_len = 1) {
  
  # REVISED:
  # Use the shared helper when no residual blocks exist.
  if (nrow(unlabeled_segments) == 0) {
    return(empty_tertiary_segments())
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>%
      dplyr::select(id, start_time, end_time),
    
    function(id, start_time, end_time) {
      
      # Extract the corresponding residual block
      block <- master_data %>%
        dplyr::filter(
          .data$id == !!id,
          .data$time >= !!start_time,
          .data$time <= !!end_time
        ) %>%
        dplyr::arrange(time)
      
      # Safety check for an empty residual block
      if (nrow(block) == 0) {
        return(empty_tertiary_segments())
      }
      
      # Build flag and scan the residual block
      dd <- build_transition_flags(block)
      
      scan_transition_one_block(
        dd_block = dd,
        min_len  = min_len
      )
    }
  ) %>%
    dplyr::arrange(id, start_time)
}


# 4) Run Transition detector

transition_segments <- detect_transition_from_unlabeled(
  master_data        = master_data,
  unlabeled_segments = unlabeled_segments,
  min_len            = 1
)

transition_segments

# ==== OPTIONAL DETECTOR-LEVEL CHECKS ===========================================

# These checks are useful for manually inspecting example segments, but they
# are not required for the detector pipeline or downstream scripts.
RUN_DETECTOR_CHECKS <- FALSE

inspect_segment <- function(
    df,
    id,
    start_time,
    end_time,
    cols = c(
      "Instructor.Lec",
      "Instructor.CQ",
      "Instructor.PQ",
      "Instructor.FUp",
      "Student.L",
      "Student.Ind",
      "Student.CG",
      "Student.OG",
      "Student.WG",
      "Student.AnQ"
    )
) {
  df %>%
    dplyr::filter(
      .data$id == !!id,
      .data$time >= !!start_time,
      .data$time <= !!end_time
    ) %>%
    dplyr::select(
      "id",
      "time",
      dplyr::any_of(cols)
    )
}

if (isTRUE(RUN_DETECTOR_CHECKS)) {
  if (nrow(instructorQA_segments) > 0L) {
    s <- instructorQA_segments[1, ]
    
    print(
      inspect_segment(
        master_data,
        s$id,
        s$start_time,
        s$end_time,
        cols = c(
          "Instructor.PQ",
          "Instructor.FUp",
          "Instructor.RtW",
          "Student.L",
          "Student.AnQ"
        )
      )
    )
  } else {
    message("No InstructorQA segments available for manual inspection.")
  }
  
  if (nrow(studentQA_segments) > 0L) {
    s <- studentQA_segments[1, ]
    
    print(
      inspect_segment(
        master_data,
        s$id,
        s$start_time,
        s$end_time,
        cols = c(
          "Instructor.AnQ",
          "Instructor.FUp",
          "Instructor.RtW",
          "Student.SQ",
          "Student.L"
        )
      )
    )
  } else {
    message("No StudentQA segments available for manual inspection.")
  }
  
  if (nrow(transition_segments) > 0L) {
    s <- transition_segments[1, ]
    
    print(
      inspect_segment(
        master_data,
        s$id,
        s$start_time,
        s$end_time,
        cols = c(
          "Instructor.Other",
          "Instructor.W",
          "Student.Other",
          "Student.W",
          "Instructor.Lec",
          "Instructor.PQ",
          "Instructor.CQ",
          "Instructor.FUp",
          "Instructor.AnQ",
          "Student.Ind",
          "Student.CG",
          "Student.OG",
          "Student.WG",
          "Student.SQ",
          "Student.AnQ"
        )
      )
    )
  } else {
    message("No Transition segments available for manual inspection.")
  }
  
  if (exists("s")) {
    rm(s)
  }
} else {
  message(
    "Skipping optional detector-level checks. ",
    "Set RUN_DETECTOR_CHECKS <- TRUE to inspect example segments."
  )
}


# ==== SAVE CACHE FOR DOWNSTREAM SCRIPTS =======================================

# Save only the tertiary segment tables required by scripts 05 and 06.
dir.create(
  "cache",
  showWarnings = FALSE,
  recursive = TRUE
)

cache_04 <- list(
  instructorQA_segments = instructorQA_segments,
  studentQA_segments = studentQA_segments,
  transition_segments = transition_segments
)

saveRDS(
  cache_04,
  "cache/04_tertiary_outputs.rds"
)

message(
  "Saved tertiary detector cache: ",
  "cache/04_tertiary_outputs.rds"
)

rm(cache_04)
