# ==================================================
# COPUS Segmentation Framework
# 01_primary_secondary_detectors.R
# Purpose: Run primary and secondary detectors
# ==================================================
source("scripts/00_setup.R")
# ensure COPUS code columns numeric ---------------------------
code_cols <- c(
  "Instructor.1o1","Instructor.Adm","Instructor.AnQ","Instructor.CQ","Instructor.DV",
  "Instructor.FUp","Instructor.Lec","Instructor.MG","Instructor.Other","Instructor.PQ",
  "Instructor.RtW","Instructor.W",
  "Student.AnQ","Student.CG","Student.Ind","Student.L","Student.OG",
  "Student.Other","Student.Prd","Student.SP","Student.SQ","Student.TQ",
  "Student.W","Student.WC","Student.WG"
)

master_data <- master_data %>%
  mutate(across(all_of(code_cols), ~ suppressWarnings(as.numeric(.)))) %>%  # "0"/"1" -> 0/1; non-numeric -> NA
  mutate(across(all_of(code_cols), ~ tidyr::replace_na(., 0)))              # NA -> 0

# ==== PRIMARY DETECTORS ====
# Lecture Detector -------------
# Logic: 
#   A Lecture interval requires both Instructor.Lec == 1 & Student.L == 1
#   Consecutive Lecture intervals within the same session are combined into one Lecture segment.
#
# Allows:
#   A single 2-minute Lecture interval
#   Multiple consecutive Lecture intervals
#   Other COPUS codes to coexist with Instructor.Lec and Student.L
#
# Splits a Lecture segment when lec_listen changes from TRUE to FALSE or time is not consecutive

# 1) Create Lecture flags and run IDs

# REVISED:
# Use a separate working object instead of adding generic run variables directly to master_data. 
lecture_flagged <- master_data %>%
  mutate(
    lec_listen =
      (`Instructor.Lec` == 1) &
      (`Student.L` == 1)
  ) %>%
  group_by(id) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    # REVISED:
    # Use Lecture-specific variable names instead of generic names such as new_run and run_id.
    lecture_new_run =
      (row_number() == 1) |
      (time != lag(time) + 1) |
      (lec_listen != lag(lec_listen)),
    
    lecture_run_id =
      cumsum(lecture_new_run)
  ) %>%
  ungroup()


# 2) Collapse contiguous Lecture runs into segments 

lecture_segments <- lecture_flagged %>%
  
  # Keep only intervals containing both Lec and Listen
  filter(lec_listen) %>%
  
  # Each run_id represents one contiguous Lecture segment
  group_by(id, lecture_run_id) %>%
  
  summarise(
    start_time  = min(time),
    end_time    = max(time),
    n_intervals = n(),
    minutes     = n_intervals * 2,
    type        = "Lecture",
    .groups     = "drop"
  ) %>%
  
  arrange(id, start_time)

# View detected Lecture segments
lecture_segments

# Clicker Detector -------------
# Logic:
#   Prompt touches/precedes the LEFT boundary of Student
#   Wrap touches/follows the RIGHT boundary of Student
#
# Allows:
#   Prompt | Prompt + Student | Student | Student + Wrap
#   Prompt + Student + Wrap within the same 2-min interval
#   Multiple overlapping intervals at phase boundaries

# 1) Phase flags
build_clicker_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      prompt_raw  = (`Instructor.CQ` == 1),
      student_raw = (`Student.Ind` == 1) | (`Student.CG` == 1),
      wrap_raw    = (`Instructor.FUp` == 1),
      
      # Phase flags allow overlaps; adjacency is enforced in the scanner
      phase_prompt  = prompt_raw,
      phase_student = student_raw,
      phase_wrap    = wrap_raw
    )
}


# 2) Scanner for one session
scan_clicker_one_id <- function(
    dd,
    min_prompt  = 1,
    min_student = 1,
    min_wrap    = 1,
    max_gap_ps  = 0,  # Prompt→Student allowed gap (2-min bins)
    max_gap_sw  = 0   # Student→Wrap allowed gap (2-min bins)
) {
  
  stopifnot(
    all(
      c(
        "id",
        "time",
        "phase_prompt",
        "phase_student",
        "phase_wrap"
      ) %in% names(dd)
    )
  )
  
  dd <- dd %>%
    dplyr::arrange(time)
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  
  while (i <= n) {
    
    # --------------------------------------------------------------------------
    # (1) Find the first Student block at or after i
    # --------------------------------------------------------------------------
    
    j <- i
    
    while (j <= n && !dd$phase_student[j]) {
      j <- j + 1
    }
    
    if (j > n) break
    
    stu_start <- j
    
    
    # Extend contiguous Student block
    cnt <- 0
    k <- stu_start
    
    while (k <= n && dd$phase_student[k]) {
      cnt <- cnt + 1
      k <- k + 1
    }
    
    if (cnt < min_student) {
      i <- k
      next
    }
    
    stu_end <- k - 1
    
    # --------------------------------------------------------------------------
    # (2) Find nearest Prompt at/before the Student left boundary
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Start directly at stu_start so Prompt and Student can overlap
    # within the same 2-minute interval.
    p_end <- stu_start
    
    while (p_end >= i && !dd$phase_prompt[p_end]) {
      p_end <- p_end - 1
    }
    
    
    # No Prompt before or at Student
    if (p_end < i) {
      i <- stu_end + 1
      next
    }
    
    
    # Extend backward to obtain the contiguous Prompt block
    p_start <- p_end
    
    while (
      p_start > i &&
      dd$phase_prompt[p_start - 1]
    ) {
      p_start <- p_start - 1
    }
    
    
    # Check minimum Prompt length
    if ((p_end - p_start + 1) < min_prompt) {
      i <- stu_end + 1
      next
    }
    
    
    # Prompt must overlap, be adjacent to, or fall within the allowed gap
    if (stu_start > (p_end + 1 + max_gap_ps)) {
      i <- stu_end + 1
      next
    }
    
    # --------------------------------------------------------------------------
    # (3) Find Wrap at/after the Student right boundary
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Start from stu_end instead of stu_start, so an overlapping Wrap
    # must reach the final interval of the Student block.
    k <- stu_end
    
    while (k <= n && !dd$phase_wrap[k]) {
      k <- k + 1
    }
    
    if (k > n) break
    
    wrap_start <- k
    
    
    # Extend contiguous Wrap block
    cnt <- 0
    m <- wrap_start
    
    while (m <= n && dd$phase_wrap[m]) {
      cnt <- cnt + 1
      m <- m + 1
    }
    
    if (cnt < min_wrap) {
      
      # REVISED:
      # Skip only the current Student block, preserving later Student
      # blocks that might form a valid Clicker sequence.
      i <- stu_end + 1
      next
    }
    
    wrap_end <- m - 1
    
    # Student and Wrap must overlap, be adjacent, or fall within allowed gap
    if (wrap_start > (stu_end + 1 + max_gap_sw)) {
      
      # REVISED:
      # Do not jump past a distant Wrap, because a later Student block
      # may validly pair with it.
      i <- stu_end + 1
      next
    }
    
    # --------------------------------------------------------------------------
    # (4) Record segment from Prompt start to Wrap end
    # --------------------------------------------------------------------------
    
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[wrap_end]
    
    out[[length(out) + 1]] <- tibble::tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "Clicker"
    )
    
    
    # --------------------------------------------------------------------------
    # (5) Advance cursor past the detected segment
    # --------------------------------------------------------------------------
    
    i <- wrap_end + 1
  }
  
  
  if (length(out) == 0) {
    tibble::tibble(
      id          = character(),
      start_time  = integer(),
      end_time    = integer(),
      n_intervals = integer(),
      minutes     = integer(),
      type        = character()
    )
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply the scanner to every class
detect_clicker_segments <- function(
    df,
    min_prompt  = 1,
    min_student = 1,
    min_wrap    = 1,
    max_gap_ps  = 0,
    max_gap_sw  = 0
) {
  
  df2 <- build_clicker_flags(df)
  
  df2 %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(
      ~ scan_clicker_one_id(
        .x,
        min_prompt  = min_prompt,
        min_student = min_student,
        min_wrap    = min_wrap,
        max_gap_ps  = max_gap_ps,
        max_gap_sw  = max_gap_sw
      )
    ) %>%
    dplyr::arrange(id, start_time)
}


# 4) Run Clicker detector
clicker_segments <- detect_clicker_segments(
  master_data,
  min_prompt  = 1,
  min_student = 1,
  min_wrap    = 1,
  max_gap_ps  = 0,  # Student must be adjacent to or overlap Prompt
  max_gap_sw  = 0   # Wrap must be adjacent to or overlap Student
)

# View detected Clicker segments
clicker_segments

# Think-Pair-Share Detector -------------
# Logic:
#   Prompt touches/precedes the LEFT boundary of Individual
#   Group touches/follows the RIGHT boundary of Individual
#   Share touches/follows the RIGHT boundary of Group
#
# Allows:
#   Prompt | Prompt + Individual | Individual + Group | Group | Group + Share | Share
#   Prompt + Individual + Group + Share within the same 2-min interval
#   Multiple overlapping intervals at phase boundaries

# 1) phase flags
build_tps_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      prompt_raw = (`Instructor.PQ` == 1) | (`Instructor.CQ` == 1),
      indiv_raw  = (`Student.Ind`  == 1),
      group_raw  = (`Student.CG`   == 1) | (`Student.OG` == 1) | (`Student.WG` == 1),
      share_raw  = (`Instructor.FUp` == 1) & ( (`Student.L` == 1) | (`Student.AnQ` == 1) ),
      
      # phase flags allow overlaps; adjacency/overlap is enforced in the scanner
      phase_prompt = prompt_raw,
      phase_indiv  = indiv_raw,
      phase_group  = group_raw,
      phase_share  = share_raw
    )
}

# 2) Scanner for one session

scan_tps_one_id <- function(
    dd,
    min_prompt = 1,
    min_indiv  = 1,
    min_group  = 1,
    min_share  = 1,
    max_gap_pi = 0,  # Prompt→Individual allowed gap (2-min bins)
    max_gap_ig = 0,  # Individual→Group allowed gap
    max_gap_gs = 0   # Group→Share allowed gap
) {
  
  stopifnot(
    all(
      c(
        "id",
        "time",
        "phase_prompt",
        "phase_indiv",
        "phase_group",
        "phase_share"
      ) %in% names(dd)
    )
  )
  
  dd <- dd %>%
    dplyr::arrange(time)
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  
  while (i <= n) {
    
    # --------------------------------------------------------------------------
    # (1) Find the first Individual block at or after cursor i
    # --------------------------------------------------------------------------
    
    j <- i
    
    while (j <= n && !dd$phase_indiv[j]) {
      j <- j + 1
    }
    
    if (j > n) break
    
    indiv_start <- j
    
    
    # Extend the contiguous Individual block
    cnt <- 0
    k <- indiv_start
    
    while (k <= n && dd$phase_indiv[k]) {
      cnt <- cnt + 1
      k <- k + 1
    }
    
    if (cnt < min_indiv) {
      i <- k
      next
    }
    
    indiv_end <- k - 1
    
    
    # --------------------------------------------------------------------------
    # (2) Find Prompt at/before the Individual left boundary
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Start directly at indiv_start so Prompt and Individual can
    # overlap within the same 2-minute interval.
    p_end <- indiv_start
    
    while (p_end >= i && !dd$phase_prompt[p_end]) {
      p_end <- p_end - 1
    }
    
    
    # No Prompt before or at Individual
    if (p_end < i) {
      i <- indiv_end + 1
      next
    }
    
    
    # Extend backward to obtain the contiguous Prompt block
    p_start <- p_end
    
    while (
      p_start > i &&
      dd$phase_prompt[p_start - 1]
    ) {
      p_start <- p_start - 1
    }
    
    
    # Check minimum Prompt length
    if ((p_end - p_start + 1) < min_prompt) {
      i <- indiv_end + 1
      next
    }
    
    
    # Prompt must overlap, be adjacent to, or fall within allowed gap
    if (indiv_start > (p_end + 1 + max_gap_pi)) {
      i <- indiv_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (3) Find Group at/after the Individual right boundary
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Start from indiv_end instead of indiv_start, so an overlapping
    # Group phase must reach the final interval of Individual.
    k <- indiv_end
    
    while (k <= n && !dd$phase_group[k]) {
      k <- k + 1
    }
    
    if (k > n) break
    
    group_start <- k
    
    
    # Extend the contiguous Group block
    cnt <- 0
    m <- group_start
    
    while (m <= n && dd$phase_group[m]) {
      cnt <- cnt + 1
      m <- m + 1
    }
    
    
    if (cnt < min_group) {
      
      # REVISED:
      # Skip only the current Individual block so later Individual
      # blocks can still form a valid TPS sequence.
      i <- indiv_end + 1
      next
    }
    
    group_end <- m - 1
    
    
    # Individual and Group must overlap, be adjacent, or fall within the allowed gap
    if (group_start > (indiv_end + 1 + max_gap_ig)) {
      
      # REVISED:
      # Do not jump past a distant Group candidate because a later
      # Individual block may validly pair with it.
      i <- indiv_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (4) Find Share at/after the Group right boundary
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Start from group_end instead of group_start, so an overlapping
    # Share phase must reach the final interval of Group.
    m <- group_end
    
    while (m <= n && !dd$phase_share[m]) {
      m <- m + 1
    }
    
    if (m > n) break
    
    share_start <- m
    
    
    # Extend the contiguous Share block
    cnt <- 0
    q <- share_start
    
    while (q <= n && dd$phase_share[q]) {
      cnt <- cnt + 1
      q <- q + 1
    }
    
    
    if (cnt < min_share) {
      
      # REVISED:
      # Skip only the current Individual block so later candidate
      # TPS sequences are not skipped.
      i <- indiv_end + 1
      next
    }
    
    share_end <- q - 1
    
    
    # Group and Share must overlap, be adjacent, or fall within the allowed gap
    if (share_start > (group_end + 1 + max_gap_gs)) {
      
      # REVISED:
      # Do not jump past a distant Share candidate because later
      # Individual and Group blocks may validly pair with it.
      i <- indiv_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (5) Record segment from Prompt start to Share end
    # --------------------------------------------------------------------------
    
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[share_end]
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "TPS"
    )
    
    
    # --------------------------------------------------------------------------
    # (6) Advance cursor past the detected segment
    # --------------------------------------------------------------------------
    
    i <- share_end + 1
  }
  
  
  # Return an empty tibble when no TPS segments are detected
  if (length(out) == 0) {
    tibble(
      id          = character(),
      start_time  = integer(),
      end_time    = integer(),
      n_intervals = integer(),
      minutes     = integer(),
      type        = character()
    )
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply scanner to every class session

detect_tps_segments <- function(
    df,
    min_prompt = 1,
    min_indiv  = 1,
    min_group  = 1,
    min_share  = 1,
    max_gap_pi = 0,
    max_gap_ig = 0,
    max_gap_gs = 0
) {
  
  df2 <- build_tps_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    map_dfr(
      ~ scan_tps_one_id(
        .x,
        min_prompt = min_prompt,
        min_indiv  = min_indiv,
        min_group  = min_group,
        min_share  = min_share,
        max_gap_pi = max_gap_pi,
        max_gap_ig = max_gap_ig,
        max_gap_gs = max_gap_gs
      )
    ) %>%
    arrange(id, start_time)
}


# 4) Run Think-Pair-Share detector

tps_segments <- detect_tps_segments(
  master_data,
  min_prompt = 1,
  min_indiv  = 1,
  min_group  = 1,
  min_share  = 1,
  max_gap_pi = 0,  # Prompt→Individual must be adjacent or overlapping
  max_gap_ig = 0,  # Individual→Group must be adjacent or overlapping
  max_gap_gs = 0   # Group→Share must be adjacent or overlapping
)


tps_segments

# Peer Instruction Detector -------------
# Logic:
#   Prompt touches/precedes the LEFT boundary of Discussion
#   Wrap touches/follows the RIGHT boundary of Discussion
#
# Allows:
#   Prompt | Prompt + Discussion | Discussion | Discussion + Wrap
#   Prompt + Discussion + Wrap within the same 2-min interval
#   Multiple overlapping intervals at phase boundaries

# 1) Build phase flags
build_pi_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.CQ` == 1) | (`Instructor.PQ` == 1),
      phase_discuss = (`Student.CG` == 1) | (`Student.OG` == 1) | (`Student.WG` == 1),
      phase_wrap    = (`Instructor.FUp` == 1)
    )
}

# 2) scanner for one session
scan_pi_one_id <- function(dd,
                           min_prompt  = 1,
                           min_discuss = 1,
                           min_wrap    = 1,
                           max_gap_pd  = 0,  # max allowed gap between Prompt end and Discuss start
                           max_gap_dw  = 0   # max allowed gap between Discuss end and Wrap start
) {
  stopifnot(all(c("id","time","phase_prompt","phase_discuss","phase_wrap") %in% names(dd)))
  
  dd <- dd %>%
    dplyr::arrange(time)
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    # ----------------------------------------------------------
    # A. Find the first Discussion block at or after cursor i
    # ----------------------------------------------------------
    
    j <- i
    
    while (j <= n && !dd$phase_discuss[j]) {
      j <- j + 1
    }
    
    if (j > n) break
    
    discuss_start <- j
    
    
    # Extend the contiguous Discussion block
    k <- discuss_start
    
    while (k <= n && dd$phase_discuss[k]) {
      k <- k + 1
    }
    
    discuss_end <- k - 1
    discuss_length <- discuss_end - discuss_start + 1
    
    
    # Check minimum Discussion length
    if (discuss_length < min_discuss) {
      i <- discuss_end + 1
      next
    }
    
    
    # ----------------------------------------------------------
    # B. Find Prompt at the LEFT boundary of Discussion
    # ----------------------------------------------------------
    
    # REVISED:
    # Start at discuss_start instead of discuss_start - 1.
    # This allows Prompt and Discussion to occur in the same
    # interval, including a one-interval Peer Instruction cycle.
    p_end <- discuss_start
    
    while (p_end >= i && !dd$phase_prompt[p_end]) {
      p_end <- p_end - 1
    }
    
    
    # No Prompt before or at the Discussion left boundary
    if (p_end < i) {
      i <- discuss_end + 1
      next
    }
    
    
    # Extend backward to find the start of the Prompt block
    p_start <- p_end
    
    while (
      p_start > i &&
      dd$phase_prompt[p_start - 1]
    ) {
      p_start <- p_start - 1
    }
    
    prompt_length <- p_end - p_start + 1
    
    
    # Check minimum Prompt length
    if (prompt_length < min_prompt) {
      i <- discuss_end + 1
      next
    }
    
    
    # Prompt must overlap, be adjacent to, or fall within the
    # permitted gap before Discussion.
    if (
      discuss_start >
      (p_end + 1 + max_gap_pd)
    ) {
      i <- discuss_end + 1
      next
    }
    
    
    # ----------------------------------------------------------
    # C. Find Wrap at the RIGHT boundary of Discussion
    # ----------------------------------------------------------
    
    # REVISED:
    # Start at discuss_end instead of discuss_start.
    # Therefore, a Wrap inside Discussion is accepted only when
    # it reaches the final Discussion interval.
    k <- discuss_end
    
    while (k <= n && !dd$phase_wrap[k]) {
      k <- k + 1
    }
    
    
    # No Wrap exists after the Discussion right boundary.
    # Because there are no later Wrap intervals, scanning can stop.
    if (k > n) break
    
    wrap_start <- k
    
    
    # Extend forward to find the end of the Wrap block
    m <- wrap_start
    
    while (m <= n && dd$phase_wrap[m]) {
      m <- m + 1
    }
    
    wrap_end <- m - 1
    wrap_length <- wrap_end - wrap_start + 1
    
    
    # Check minimum Wrap length
    if (wrap_length < min_wrap) {
      
      # REVISED:
      # Skip only the current Discussion block.
      # Do not skip later Discussion blocks that might validly
      # pair with a subsequent Wrap.
      i <- discuss_end + 1
      next
    }
    
    
    # Wrap must overlap, be adjacent to, or fall within the
    # permitted gap after Discussion.
    if (
      wrap_start >
      (discuss_end + 1 + max_gap_dw)
    ) {
      
      # REVISED:
      # Skip only the current Discussion block rather than
      # jumping past the distant Wrap candidate.
      i <- discuss_end + 1
      next
    }
    
    
    # ----------------------------------------------------------
    # D. Record the Peer Instruction segment
    # ----------------------------------------------------------
    
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[wrap_end]
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "PeerInstruction"
    )
    
    
    # ----------------------------------------------------------
    # E. Advance cursor past the detected segment
    # ----------------------------------------------------------
    
    i <- wrap_end + 1
  }
  
  
  # Return an empty tibble if no PI segments were detected
  if (length(out) == 0) {
    tibble(
      id          = character(),
      start_time  = integer(),
      end_time    = integer(),
      n_intervals = integer(),
      minutes     = integer(),
      type        = character()
    )
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply scanner to every class session

detect_pi_segments <- function(
    df,
    min_prompt  = 1,
    min_discuss = 1,
    min_wrap    = 1,
    max_gap_pd  = 0,
    max_gap_dw  = 0
) {
  
  df2 <- build_pi_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    map_dfr(
      ~ scan_pi_one_id(
        .x,
        min_prompt  = min_prompt,
        min_discuss = min_discuss,
        min_wrap    = min_wrap,
        max_gap_pd  = max_gap_pd,
        max_gap_dw  = max_gap_dw
      )
    ) %>%
    arrange(id, start_time)
}


# 4) Run Peer Instruction detector 

pi_segments <- detect_pi_segments(
  master_data,
  
  # One 2-minute interval is sufficient for each phase
  min_prompt  = 1,
  min_discuss = 1,
  min_wrap    = 1,
  
  # Phase boundaries must overlap or be directly adjacent
  max_gap_pd  = 0,
  max_gap_dw  = 0
)

# View detected Peer Instruction segments
pi_segments

# ====SECONDARY DETECTORS ====
# Peer-Lite Detector -------------
# Logic:
#   Discussion is the anchor phase
#   Prompt touches/precedes the LEFT boundary of Discussion
#   No Instructor.FUp occurs during Discussion or immediately after it
#
# Allows:
#   Prompt | Prompt + Discussion | Discussion
#   Prompt + Discussion within the same 2-min interval
#   Multiple overlapping intervals at the Prompt–Discussion boundary
#
# Excludes:
#   Instructor.FUp during any Discussion interval
#   Instructor.FUp in the interval immediately after Discussion

# 1) Phase flags
build_peerlite_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.PQ` == 1) | (`Instructor.CQ` == 1),
      phase_discuss = (`Student.OG` == 1) | (`Student.WG` == 1) | (`Student.CG` == 1),
      phase_FUp     = (`Instructor.FUp` == 1)
    )
}

# 2) Scanner for one class session

scan_peerlite_one_id <- function(
    dd,
    min_prompt  = 1,
    min_discuss = 1,
    max_gap_pd  = 0  # Prompt→Discussion allowed gap in 2-min bins
) {
  
  stopifnot(all(c("id","time","phase_prompt","phase_discuss","phase_FUp") %in% names(dd)))
  
  dd <- dd %>%
    arrange(time)
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    # --------------------------------------------------------------------------
    # (1) Find the first Discussion block at or after cursor i
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Peer-Lite is now Discussion-anchored, matching the structure
    # of the Peer Instruction detector.
    j <- i
    
    while (j <= n && !dd$phase_discuss[j]) {
      j <- j + 1
    }
    
    if (j > n) break
    
    discuss_start <- j
    
    
    # Extend the contiguous Discussion block
    k <- discuss_start
    
    while (k <= n && dd$phase_discuss[k]) {
      k <- k + 1
    }
    
    discuss_end <- k - 1
    discuss_length <- discuss_end - discuss_start + 1
    
    
    # Check minimum Discussion length
    if (discuss_length < min_discuss) {
      i <- discuss_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (2) Find Prompt at/before the Discussion left boundary
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Begin at discuss_start so Prompt and Discussion may overlap
    # in the same 2-minute interval.
    p_end <- discuss_start
    
    while (p_end >= i && !dd$phase_prompt[p_end]) {
      p_end <- p_end - 1
    }
    
    
    # No Prompt before or at the Discussion left boundary
    if (p_end < i) {
      i <- discuss_end + 1
      next
    }
    
    
    # Extend backward to obtain the contiguous Prompt block
    p_start <- p_end
    
    while (
      p_start > i &&
      dd$phase_prompt[p_start - 1]
    ) {
      p_start <- p_start - 1
    }
    
    
    # Check minimum Prompt length
    if ((p_end - p_start + 1) < min_prompt) {
      i <- discuss_end + 1
      next
    }
    
    
    # Prompt must overlap, be adjacent to, or fall within the allowed gap
    if (discuss_start > (p_end + 1 + max_gap_pd)) {
      i <- discuss_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (3) Exclude Discussion blocks with immediate Instructor.FUp
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Peer-Lite requires no Instructor.FUp anywhere during the
    # contiguous Discussion block.
    if (any(dd$phase_FUp[discuss_start:discuss_end])) {
      i <- discuss_end + 1
      next
    }
    
    
    # REVISED:
    # Also exclude Instructor.FUp in the interval immediately
    # following the Discussion block.
    if (
      discuss_end < n &&
      dd$phase_FUp[discuss_end + 1]
    ) {
      i <- discuss_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (4) Record segment from Prompt start to Discussion end
    # --------------------------------------------------------------------------
    
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[discuss_end]
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "PeerLite"
    )
    
    
    # --------------------------------------------------------------------------
    # (5) Advance cursor past the Discussion block
    # --------------------------------------------------------------------------
    
    # This prevents the same contiguous Discussion run from being
    # captured as multiple short Peer-Lite episodes.
    i <- discuss_end + 1
  }
  
  
  # Return an empty tibble when no Peer-Lite segments are detected
  if (length(out) == 0) {
    tibble(
      id          = character(),
      start_time  = integer(),
      end_time    = integer(),
      n_intervals = integer(),
      minutes     = integer(),
      type        = character()
    )
  } else {
    dplyr::bind_rows(out) %>%
      arrange(id, start_time)
  }
}


# 3) Apply scanner across all class sessions

detect_peer_lite_segments <- function(
    df,
    min_prompt  = 1,
    min_discuss = 1,
    max_gap_pd  = 0
) {
  
  df2 <- build_peerlite_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    purrr::map_dfr(
      ~ scan_peerlite_one_id(
        .x,
        min_prompt  = min_prompt,
        min_discuss = min_discuss,
        max_gap_pd  = max_gap_pd
      )
    ) %>%
    arrange(id, start_time)
}


# 4) Run Peer-Lite detector 

peer_lite_segments <- detect_peer_lite_segments(
  master_data,
  min_prompt  = 1,
  min_discuss = 1,
  max_gap_pd  = 0  # Prompt and Discussion must overlap or be adjacent
)


peer_lite_segments

# Clicker-Lite Detector -------------
# Logic:
#   Student response is the anchor phase
#   CQ Prompt touches/precedes the LEFT boundary of Student response
#   No Instructor.FUp occurs during Student response or immediately after it
#
# Allows:
#   CQ | CQ + Student | Student
#   CQ + Student within the same 2-min interval
#   Multiple overlapping intervals at the Prompt–Student boundary
#   A contiguous Student phase containing Ind, CG, or an Ind→CG transition
#
# Excludes:
#   Instructor.FUp during any Student interval
#   Instructor.FUp in the interval immediately after Student response

# 1) Phase flags
build_clickerlite_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.CQ` == 1),
      phase_student = (`Student.Ind` == 1) | (`Student.CG` == 1),
      phase_FUp     = (`Instructor.FUp` == 1)
    )
}

# 2) Scanner for one class session 

scan_clickerlite_one_id <- function(
    dd,
    min_prompt  = 1,
    min_student = 1,
    max_gap_ps  = 0  # Prompt→Student allowed gap in 2-min bins
) {
  
  stopifnot(all(c("id","time","phase_prompt","phase_student","phase_FUp") %in% names(dd)))
  
  dd <- dd %>%
    arrange(time)
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    # --------------------------------------------------------------------------
    # (1) Find the first Student block at or after cursor i
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Clicker-Lite is now Student-anchored, matching the structure
    # of the full Clicker detector.
    j <- i
    
    while (j <= n && !dd$phase_student[j]) {
      j <- j + 1
    }
    
    if (j > n) break
    
    student_start <- j
    
    
    # Extend the contiguous Student block
    k <- student_start
    
    while (k <= n && dd$phase_student[k]) {
      k <- k + 1
    }
    
    student_end <- k - 1
    student_length <- student_end - student_start + 1
    
    
    # Check minimum Student phase length
    if (student_length < min_student) {
      i <- student_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (2) Find CQ Prompt at/before the Student left boundary
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Begin at student_start so CQ and Student response may overlap
    # within the same 2-minute interval.
    p_end <- student_start
    
    while (p_end >= i && !dd$phase_prompt[p_end]) {
      p_end <- p_end - 1
    }
    
    
    # No CQ Prompt before or at the Student left boundary
    if (p_end < i) {
      i <- student_end + 1
      next
    }
    
    
    # Extend backward to obtain the contiguous CQ Prompt block
    p_start <- p_end
    
    while (
      p_start > i &&
      dd$phase_prompt[p_start - 1]
    ) {
      p_start <- p_start - 1
    }
    
    
    # Check minimum Prompt length
    if ((p_end - p_start + 1) < min_prompt) {
      i <- student_end + 1
      next
    }
    
    
    # REVISED:
    # CQ Prompt must overlap, be adjacent to, or fall within the
    # explicitly allowed Prompt→Student gap.
    if (student_start > (p_end + 1 + max_gap_ps)) {
      i <- student_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (3) Exclude Student blocks with immediate Instructor.FUp
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Clicker-Lite requires no Instructor.FUp anywhere during the
    # contiguous Student-response block.
    if (any(dd$phase_FUp[student_start:student_end])) {
      i <- student_end + 1
      next
    }
    
    
    # REVISED:
    # Also exclude Instructor.FUp in the interval immediately
    # following the Student-response block.
    if (
      student_end < n &&
      dd$phase_FUp[student_end + 1]
    ) {
      i <- student_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (4) Record segment from CQ Prompt start to Student end
    # --------------------------------------------------------------------------
    
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[student_end]
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "ClickerLite"
    )
    
    
    # --------------------------------------------------------------------------
    # (5) Advance cursor past the Student block
    # --------------------------------------------------------------------------
    
    # This prevents the same contiguous Student-response run from
    # being captured as multiple short Clicker-Lite episodes.
    i <- student_end + 1
  }
  
  
  # Return an empty tibble when no Clicker-Lite segments are detected
  if (length(out) == 0) {
    tibble(
      id          = character(),
      start_time  = integer(),
      end_time    = integer(),
      n_intervals = integer(),
      minutes     = integer(),
      type        = character()
    )
  } else {
    dplyr::bind_rows(out) %>%
      arrange(id, start_time)
  }
}


# 3) Apply scanner across all class sessions

detect_clicker_lite_segments <- function(
    df,
    min_prompt  = 1,
    min_student = 1,
    max_gap_ps  = 0
) {
  
  df2 <- build_clickerlite_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    purrr::map_dfr(
      ~ scan_clickerlite_one_id(
        .x,
        min_prompt  = min_prompt,
        min_student = min_student,
        max_gap_ps  = max_gap_ps
      )
    ) %>%
    arrange(id, start_time)
}


# 4) Run Clicker-Lite detector

clicker_lite_segments <- detect_clicker_lite_segments(
  master_data,
  min_prompt  = 1,
  min_student = 1,
  max_gap_ps  = 0  # CQ Prompt and Student must overlap or be adjacent
)


clicker_lite_segments

# Admin Detector -------------
# Logic:
#   An Admin interval requires: Instructor.Adm == 1
#   Consecutive Admin intervals within the same session are combined into one Admin segment.
#
# Allows:
#   A single 2-minute Admin interval
#   Multiple consecutive Admin intervals
#   Other Instructor and Student COPUS codes to coexist with Admin
#
# Splits an Admin segment when: Instructor.Adm changes from 1 to 0 or time is not consecutive

# 1) Phase flags 

build_admin_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_admin = (`Instructor.Adm` == 1)
    )
}


# 2) Scanner for one class session

scan_admin_one_id <- function(
    dd,
    min_admin = 1
) {
  
  stopifnot(all(c("id","time","phase_admin") %in% names(dd)))
  
  dd <- dd %>%
    arrange(time)
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  
  while (i <= n) {
    
    # --------------------------------------------------------------------------
    # (1) Find the first Admin interval at or after cursor i
    # --------------------------------------------------------------------------
    
    j <- i
    
    while (j <= n && !dd$phase_admin[j]) {
      j <- j + 1
    }
    
    if (j > n) break
    
    admin_start <- j
    
    
    # --------------------------------------------------------------------------
    # (2) Extend the contiguous Admin block
    # --------------------------------------------------------------------------
    
    # REVISED:
    # A block continues only when the next row is also Admin AND
    # its time value is exactly one interval after the previous row.
    k <- admin_start + 1
    
    while (
      k <= n &&
      dd$phase_admin[k] &&
      dd$time[k] == dd$time[k - 1] + 1
    ) {
      k <- k + 1
    }
    
    admin_end <- k - 1
    
    
    # REVISED:
    # Calculate segment length using the actual number of rows in the detected contiguous Admin block.
    admin_length <- admin_end - admin_start + 1
    
    
    # Check minimum Admin length
    if (admin_length < min_admin) {
      i <- admin_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (3) Record Admin segment
    # --------------------------------------------------------------------------
    
    seg_start_time <- dd$time[admin_start]
    seg_end_time   <- dd$time[admin_end]
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      
      # REVISED:
      # Use the actual number of detected interval rows instead of
      # calculating length only from start_time and end_time.
      n_intervals = admin_length,
      minutes     = admin_length * 2,
      
      type        = "Admin"
    )
    
    
    # --------------------------------------------------------------------------
    # (4) Advance cursor past the detected Admin block
    # --------------------------------------------------------------------------
    
    i <- admin_end + 1
  }
  
  
  # Return an empty tibble when no Admin segments are detected
  if (length(out) == 0) {
    tibble(
      id          = character(),
      start_time  = integer(),
      end_time    = integer(),
      n_intervals = integer(),
      minutes     = integer(),
      type        = character()
    )
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply scanner across all class sessions 

detect_admin_segments <- function(
    df,
    min_admin = 1
) {
  
  df2 <- build_admin_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    map_dfr(
      ~ scan_admin_one_id(
        .x,
        min_admin = min_admin
      )
    ) %>%
    arrange(id, start_time)
}


# 4) Run Admin detector

admin_segments <- detect_admin_segments(
  master_data,
  min_admin = 1  # A single 2-minute Admin interval is allowed
)

admin_segments

# Student Work Detector -------------
# Logic:
#   A Student Work interval requires: (Instructor.MG OR Instructor.1o1) & (Student.Ind OR Student.OG OR Student.WG).
#   Consecutive Student Work intervals are combined into one segment.
#
# Allows:
#   A single 2-minute Student Work interval
#   Multiple consecutive Student Work intervals
#   Instructor.PQ or Instructor.CQ during or before Student Work
#   Other Instructor and Student COPUS codes to coexist
#   Raw overlap with phase-based strategy detectors

# 1) Build flag 

build_student_work_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_work =
        ((`Instructor.MG` == 1) | (`Instructor.1o1` == 1)) &
        ((`Student.Ind` == 1) | (`Student.OG` == 1) | (`Student.WG` == 1)))
}

# 2) Scanner for one class session 

scan_student_work_one_id <- function(
    dd,
    min_work = 1
) {
  
  stopifnot(
    all(c("id","time","phase_work") %in% names(dd)))
  
  dd <- dd %>%
    arrange(time)
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    # --------------------------------------------------------------------------
    # (1) Find the first Student Work interval at or after cursor i
    # --------------------------------------------------------------------------
    
    # REVISED:
    # Student Work can start in any phase_work interval, 
    # regardless of the presence of PQ/CQ during or immediately after the block.
    j <- i
    
    while (j <= n && !dd$phase_work[j]) {
      j <- j + 1
    }
    
    if (j > n) break
    
    work_start <- j
    
    
    # --------------------------------------------------------------------------
    # (2) Extend the contiguous Student Work block
    # --------------------------------------------------------------------------
    
    # REVISED:
    # A block continues only when the next row is also Student Work
    # and its time value is exactly one interval after the previous row.
    k <- work_start + 1
    
    while (
      k <= n &&
      dd$phase_work[k] &&
      dd$time[k] == dd$time[k - 1] + 1
    ) {
      k <- k + 1
    }
    
    work_end <- k - 1
    
    
    # REVISED:
    # Calculate length using the actual number of interval rows.
    work_length <- work_end - work_start + 1
    
    
    # Check minimum Student Work length
    if (work_length < min_work) {
      i <- work_end + 1
      next
    }
    
    
    # --------------------------------------------------------------------------
    # (3) Record Student Work segment
    # --------------------------------------------------------------------------
    
    seg_start_time <- dd$time[work_start]
    seg_end_time   <- dd$time[work_end]
    
    out[[length(out) + 1]] <- tibble::tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = work_length,
      minutes     = work_length * 2,
      type        = "StudentWork"
    )
    
    
    # --------------------------------------------------------------------------
    # (4) Advance cursor past the Student Work block
    # --------------------------------------------------------------------------
    
    i <- work_end + 1
  }
  
  
  # Return an empty tibble when no Student Work segments are detected
  if (length(out) == 0) {
    tibble::tibble(
      id          = character(),
      start_time  = integer(),
      end_time    = integer(),
      n_intervals = integer(),
      minutes     = integer(),
      type        = character()
    )
  } else {
    dplyr::bind_rows(out)
  }
}


# 3) Apply scanner across all class sessions 

detect_student_work_segments <- function(
    df,
    min_work = 1
) {
  
  df2 <- build_student_work_flags(df)
  
  df2 %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(
      ~ scan_student_work_one_id(
        .x,
        min_work = min_work
      )
    ) %>%
    dplyr::arrange(id, start_time)
}


# 4) Run Student Work detector 

student_work_segments <- detect_student_work_segments(
  master_data,
  min_work = 1  # A single 2-minute Student Work interval is allowed
)

student_work_segments


# ==== OPTIONAL DETECTOR-LEVEL CHECKS ===========================================

# Print a concise detector summary by default. Detailed heatmap inspections are
# retained for manual review but are not required for the detector pipeline.
RUN_DETECTOR_CHECKS <- FALSE

# Preferred COPUS code order for manual inspection
code_order <- c(
  "Instructor.Lec", "Instructor.CQ", "Instructor.PQ", "Instructor.FUp",
  "Instructor.1o1", "Instructor.Adm", "Instructor.AnQ", "Instructor.DV",
  "Instructor.MG", "Instructor.Other", "Instructor.RtW", "Instructor.W",
  "Student.L", "Student.Ind", "Student.CG", "Student.OG", "Student.WG",
  "Student.AnQ", "Student.Other", "Student.Prd", "Student.SP", "Student.SQ",
  "Student.TQ", "Student.W", "Student.WC"
)

# Heatmap inspector ------------------------------------------------------------

inspect_segment_heat <- function(
    df,
    id,
    start_time,
    end_time,
    plot = FALSE,
    profile = NULL,
    cols = code_order
) {
  df_f <- df %>%
    dplyr::filter(
      .data$id == !!id,
      .data$time >= !!start_time,
      .data$time <= !!end_time
    ) %>%
    dplyr::select(
      "time",
      dplyr::any_of(cols)
    )
  
  long <- df_f %>%
    tidyr::pivot_longer(
      cols = -"time",
      names_to = "Code",
      values_to = "Present"
    ) %>%
    dplyr::mutate(
      Present = suppressWarnings(as.numeric(.data$Present))
    ) %>%
    tidyr::replace_na(
      list(Present = 0)
    ) %>%
    dplyr::mutate(
      Code = factor(
        .data$Code,
        levels = cols
      )
    )
  
  if (isTRUE(plot)) {
    p1 <- ggplot2::ggplot(
      long,
      ggplot2::aes(
        x = .data$time,
        y = .data$Code,
        fill = factor(.data$Present)
      )
    ) +
      ggplot2::geom_tile(
        color = "grey85"
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "0" = "white",
          "1" = "grey20"
        ),
        guide = "none"
      ) +
      ggplot2::labs(
        title = paste0(
          ifelse(
            is.null(profile),
            "",
            paste0(profile, " — ")
          ),
          "Codes over time: ",
          id,
          " (",
          start_time,
          "–",
          end_time,
          ")"
        ),
        x = "Time (2-min bins)",
        y = NULL
      ) +
      ggplot2::theme_classic(
        base_size = 11
      )
    
    print(p1)
  }
  
  invisible(long)
}


# Inspect the first detected segment from one detector --------------------------

inspect_first <- function(
    seg_table,
    profile_name
) {
  if (nrow(seg_table) == 0L) {
    message(
      profile_name,
      ": No segments detected."
    )
    
    return(invisible(NULL))
  }
  
  inspect_segment_heat(
    df = master_data,
    id = seg_table$id[1],
    start_time = seg_table$start_time[1],
    end_time = seg_table$end_time[1],
    profile = profile_name,
    plot = TRUE
  )
}


# Concise detector summary -----------------------------------------------------

summarize_detector <- function(
    seg_table,
    detector_name
) {
  tibble::tibble(
    detector = detector_name,
    n_segments = nrow(seg_table),
    median_minutes = if (
      nrow(seg_table) > 0L &&
      "minutes" %in% names(seg_table)
    ) {
      stats::median(
        seg_table$minutes,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
  )
}

detector_summary <- dplyr::bind_rows(
  summarize_detector(
    lecture_segments,
    "Lecture"
  ),
  summarize_detector(
    clicker_segments,
    "Clicker"
  ),
  summarize_detector(
    tps_segments,
    "TPS"
  ),
  summarize_detector(
    pi_segments,
    "PeerInstruction"
  ),
  summarize_detector(
    peer_lite_segments,
    "PeerLite"
  ),
  summarize_detector(
    clicker_lite_segments,
    "ClickerLite"
  ),
  summarize_detector(
    admin_segments,
    "Admin"
  ),
  summarize_detector(
    student_work_segments,
    "StudentWork"
  )
)

print(detector_summary)


# Optional heatmap inspections -------------------------------------------------

if (isTRUE(RUN_DETECTOR_CHECKS)) {
  inspect_first(
    lecture_segments,
    "Lecture"
  )
  
  inspect_first(
    clicker_segments,
    "Clicker"
  )
  
  inspect_first(
    tps_segments,
    "TPS"
  )
  
  inspect_first(
    pi_segments,
    "Peer Instruction"
  )
  
  inspect_first(
    peer_lite_segments,
    "PeerLite"
  )
  
  inspect_first(
    clicker_lite_segments,
    "ClickerLite"
  )
  
  inspect_first(
    admin_segments,
    "Admin"
  )
  
  inspect_first(
    student_work_segments,
    "Student Work"
  )
} else {
  message(
    "Skipping optional detector-level plots. ",
    "Set RUN_DETECTOR_CHECKS <- TRUE to inspect example segments."
  )
}


# ==== SAVE CACHE FOR DOWNSTREAM SCRIPTS =======================================

# Save only the objects required by downstream scripts.
# This avoids saving the entire global environment with save.image().
dir.create(
  "cache",
  showWarnings = FALSE,
  recursive = TRUE
)

cache_01 <- list(
  code_cols = code_cols,
  master_data = master_data,
  lecture_segments = lecture_segments,
  clicker_segments = clicker_segments,
  pi_segments = pi_segments,
  tps_segments = tps_segments,
  peer_lite_segments = peer_lite_segments,
  clicker_lite_segments = clicker_lite_segments,
  admin_segments = admin_segments,
  student_work_segments = student_work_segments
)

saveRDS(
  cache_01,
  "cache/01_primary_secondary_outputs.rds"
)

message(
  "Saved primary/secondary detector cache: ",
  "cache/01_primary_secondary_outputs.rds"
)

rm(cache_01)
