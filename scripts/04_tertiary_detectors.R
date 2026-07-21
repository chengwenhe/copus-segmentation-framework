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
# Instructor QA ----------------
# 1) build flags on residual block
build_instructorQA_flags <- function(df_block) {
  df_block %>%
    arrange(time) %>%
    mutate(
      # anchor: MUST have PQ; students L or AnQ
      anchor_instructorQA =
        ((`Instructor.PQ` == 1) | (`Instructor.FUp` == 1) ) & ( (`Student.AnQ` == 1) | (`Student.L` == 1) ),
      
      # continue: PQ/FUp/RtW/AnQ allowed; students L or AnQ or asking questions
      cont_instructorQA =
        ( (`Instructor.PQ` == 1) | (`Instructor.FUp` == 1) | (`Instructor.RtW` == 1) |  (`Instructor.AnQ` == 1)  ) &
        ( (`Student.AnQ` == 1) | (`Student.SQ` == 1) |  (`Student.L` == 1) )
    )
}

# 2) scanner for one unlabeled block
scan_instructorQA_one_block <- function(dd_block,
                                        min_len = 1) {
  stopifnot(all(c("id","time","anchor_instructorQA","cont_instructorQA") %in% names(dd_block)))
  
  dd_block <- dd_block %>% arrange(time)
  n <- nrow(dd_block)
  i <- 1
  out <- list()
  
  while (i <= n) {
    # find next anchor
    j <- i
    while (j <= n && !dd_block$anchor_instructorQA[j]) j <- j + 1
    if (j > n) break
    
    seg_start_idx <- j
    
    # extend while continuation holds (including the anchor interval)
    k <- seg_start_idx
    while (k <= n && dd_block$cont_instructorQA[k]) k <- k + 1
    seg_end_idx <- k - 1
    
    # enforce min length (in intervals)
    if ((seg_end_idx - seg_start_idx + 1) >= min_len) {
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1,
        minutes     = (dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1) * 2,
        type        = "InstructorQA"
      )
    }
    
    # move cursor to first interval after this segment
    i <- seg_end_idx + 1
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}

# 3) apply across ALL residual unlabeled blocks
detect_instructorQA_from_unlabeled <- function(master_data,
                                               unlabeled_segments,
                                               min_len = 1) {
  
  if (nrow(unlabeled_segments) == 0) {
    return(tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    ))
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>% select(id, start_time, end_time),
    function(id, start_time, end_time) {
      
      block <- master_data %>%
        filter(.data$id == !!id,
               .data$time >= !!start_time,
               .data$time <= !!end_time) %>%
        arrange(time)
      
      # Safety: skip empty blocks
      if (nrow(block) == 0) return(NULL)
      
      dd <- build_instructorQA_flags(block)
      
      scan_instructorQA_one_block(dd, min_len = min_len)
    }
  ) %>%
    arrange(id, start_time)
}

# 4) Run it (Instructor QA only)
instructorQA_segments <- detect_instructorQA_from_unlabeled(
  master_data = master_data,
  unlabeled_segments = unlabeled_segments,
  min_len = 1
)

instructorQA_segments


# Student QA ----------------------
# 1) build flags on a residual block
build_studentQA_flags <- function(df_block) {
  df_block %>%
    arrange(time) %>%
    mutate(
      # anchor: MUST have student question + instructor answer or followup
      anchor_studentQA =
        (`Student.SQ` == 1) & ((`Instructor.AnQ` == 1) | (`Instructor.FUp` == 1)),
      
      # continue: instructor answering/followup/writing + students asking, answering, or listening
      cont_studentQA =
        ( (`Instructor.AnQ` == 1) | (`Instructor.FUp` == 1) | (`Instructor.RtW` == 1) ) &
        ( (`Student.SQ` == 1) |  (`Student.AnQ` == 1) | (`Student.L` == 1) )
    )
}

# 2) scanner for one unlabeled block
scan_studentQA_one_block <- function(dd_block,
                                     min_len = 1) {
  stopifnot(all(c("id","time","anchor_studentQA","cont_studentQA") %in% names(dd_block)))
  
  dd_block <- dd_block %>% arrange(time)
  n <- nrow(dd_block)
  i <- 1
  out <- list()
  
  while (i <= n) {
    # find next anchor
    j <- i
    while (j <= n && !dd_block$anchor_studentQA[j]) j <- j + 1
    if (j > n) break
    
    seg_start_idx <- j
    
    # extend while continuation holds
    k <- seg_start_idx
    while (k <= n && dd_block$cont_studentQA[k]) k <- k + 1
    seg_end_idx <- k - 1
    
    # enforce min length (in intervals)
    if ((seg_end_idx - seg_start_idx + 1) >= min_len) {
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1,
        minutes     = (dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1) * 2,
        type        = "StudentQA"
      )
    }
    
    # advance cursor
    i <- seg_end_idx + 1
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}


# 3) apply across ALL residual unlabeled blocks
detect_studentQA_from_unlabeled <- function(master_data,
                                            unlabeled_segments,
                                            min_len = 1) {
  
  if (nrow(unlabeled_segments) == 0) {
    return(tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    ))
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>% dplyr::select(id, start_time, end_time),
    function(id, start_time, end_time) {
      
      block <- master_data %>%
        dplyr::filter(.data$id == !!id,
                      .data$time >= !!start_time,
                      .data$time <= !!end_time) %>%
        dplyr::arrange(time)
      
      if (nrow(block) == 0) return(NULL)
      
      dd <- build_studentQA_flags(block)
      
      scan_studentQA_one_block(dd, min_len = min_len)
    }
  ) %>%
    dplyr::arrange(id, start_time)
}

# 4) run it
studentQA_segments <- detect_studentQA_from_unlabeled(
  master_data = master_data,
  unlabeled_segments = unlabeled_segments,
  min_len = 1
)

studentQA_segments


# Transition ----------
# 1) build flags on a residual block
build_transition_flags <- function(df_block) {
  
  # exclusion sets
  I_excl <- c("Instructor.Lec","Instructor.FUp","Instructor.PQ","Instructor.CQ",
              "Instructor.AnQ","Instructor.MG","Instructor.1o1")
  
  S_excl <- c("Student.Ind","Student.CG","Student.WG","Student.OG",
              "Student.AnQ","Student.SQ","Student.WC","Student.Prd",
              "Student.SP","Student.TQ")
  
  df_block %>%
    arrange(time) %>%
    mutate(
      # base requirement: in-between behavior only
      base_transition =
        ( (`Instructor.W` == 1) | (`Instructor.Other` == 1) ) &
        ( (`Student.W` == 1)    | (`Student.Other` == 1) ),
      
      # exclusion: no instructional codes present in the same interval
      instr_excluded_I = rowSums(dplyr::across(dplyr::all_of(I_excl))) > 0,
      instr_excluded_S = rowSums(dplyr::across(dplyr::all_of(S_excl))) > 0,
      
      # eligible if base holds and no excluded codes present
      eligible_transition = base_transition & (!instr_excluded_I) & (!instr_excluded_S),
      
      # anchor/continue are identical for this detector
      anchor_transition = eligible_transition,
      cont_transition   = eligible_transition
    )
}

# 2) scanner for one unlabeled block
scan_transition_one_block <- function(dd_block,
                                      min_len = 1) {
  stopifnot(all(c("id","time","anchor_transition","cont_transition") %in% names(dd_block)))
  
  dd_block <- dd_block %>% arrange(time)
  n <- nrow(dd_block)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    # find next anchor
    j <- i
    while (j <= n && !dd_block$anchor_transition[j]) j <- j + 1
    if (j > n) break
    
    seg_start_idx <- j
    
    # extend while continuation holds
    k <- seg_start_idx
    while (k <= n && dd_block$cont_transition[k]) k <- k + 1
    seg_end_idx <- k - 1
    
    # enforce min length
    if ((seg_end_idx - seg_start_idx + 1) >= min_len) {
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1,
        minutes     = (dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1) * 2,
        type        = "Transition"
      )
    }
    
    i <- seg_end_idx + 1
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}


# 3) apply across ALL residual unlabeled blocks
detect_transition_from_unlabeled <- function(master_data,
                                             unlabeled_segments,
                                             min_len = 1) {
  
  if (nrow(unlabeled_segments) == 0) {
    return(tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    ))
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>% dplyr::select(id, start_time, end_time),
    function(id, start_time, end_time) {
      
      block <- master_data %>%
        dplyr::filter(.data$id == !!id,
                      .data$time >= !!start_time,
                      .data$time <= !!end_time) %>%
        dplyr::arrange(time)
      
      if (nrow(block) == 0) return(NULL)
      
      dd <- build_transition_flags(block)
      
      scan_transition_one_block(dd, min_len = min_len)
    }
  ) %>%
    dplyr::arrange(id, start_time)
}


# 4) run it
transition_segments <- detect_transition_from_unlabeled(
  master_data = master_data,
  unlabeled_segments = unlabeled_segments,
  min_len = 1
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
