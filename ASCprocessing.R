########################################
######### PROCESSING ASC ###############
########################################


########################################
# LOG CHANGES #
# 2/05/2022
# Implement testit to check that all epochs are correctly created
# Delete parts of blinks and saccades as we are not using them in analysis
# ifelse for information about epochs for filler and critical trials
# added epoch 'audio' for filler trials
# collapse epoch information

# LIBRARIES

library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(testit)

# SET WD

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen=999) # avoid e notation
`%!in%` <- Negate(`%in%`) #use %!in%

# DATA

# Things for separation

events_namesFIX <- c("EFIX", "SFIX", "ESACC", "SSACC", "EBLINK", "SBLINK")
events_namesEVENTS <- c(events_namesFIX, "MSG", "**", ">>>>>>>", "PUPIL", "PRESCALER", "VPRESCALER", "EVENTS", "SAMPLES")

# Read all files in folder

temp <- list.files(patter = "*.asc")

for(i in temp){
  
  print(paste(
    "## PROCESSING", i, "##" 
  ))
  
  options(readr.num_columns = 0) # to not print column specification
  
  oldw <- getOption("warn") # silence parsing file
  options(warn = -1)
  
  ppt_rawdata <- read_table(i, col_names = FALSE)
  
  options(warn = oldw) # unmute warnings
  
  filename = paste(i, "_processed.csv", sep = "")
  
  # separate looks from msgs
  
  # identifier
  
  ppt_rawdata$number_sample <- seq(nrow(ppt_rawdata))
  
  # separate
  
  pptMSG <- ppt_rawdata %>% # messages (trial information)
    filter(X1 == "MSG")
  
  print(paste("MSG created for", i))
  
  pptFSB <- ppt_rawdata %>% # fixations, saccades and blinks
    filter(X1 %in% events_namesFIX)
  
  print(paste("FSB created for", i))
  
  pptEYEEVENTS <- ppt_rawdata %>% # individual eye events (samples)
  filter(! X1 %in% events_namesEVENTS)
  
  print(paste("EYEVENTS created", i))
  
  ## MSG DATA
  
  print("--------- INFORMATION ABOUT MESSAGES ---------")
  
  MSGS <- pptMSG %>%
    filter(X3 %in% c("start_trial", "stop_trial")) %>%
    summarise(
      X2 = X2,
      trial = rep(1:sum(X3=="start_trial"), each = 2)
    )
  
  print("Trial time filtered")
  
  MSGS <- merge(pptMSG, MSGS, by = c("X2"), all.x = TRUE)
  
  MSGS <- MSGS %>%
    fill(trial) %>%
    mutate(
      DISTRACTOR = ifelse(grepl("dis\\>", X5), X6, NA),
      TARGET = ifelse(grepl("ref", X5), X6, NA),
      t_pos = ifelse(grepl("t_pos", X5), X6, NA),
      d_pos = ifelse(grepl("d_pos", X5), X6, NA),
      fluency = ifelse(grepl("fluency", X5), X6, NA),
      frequency = ifelse(grepl("frequency", X5), X6, NA),
      trialtype =ifelse(grepl("trialtype", X5), X6, NA),
      nativeness = ifelse(grepl("nativeness", X5), X6, NA),
      CARRIER_F =ifelse(grepl("CARRIER_F", X5), X6, NA),
      CARRIER_D = ifelse(grepl("CARRIER_D", X5), X6, NA),
      clicked = ifelse(grepl("clicked", X5), X6, NA),
      time_start = ifelse(X3 == "start_trial", X2, NA),
      time_end = ifelse(X3 == "stop_trial", X2, NA),
      audio_onset = ifelse(X3 == "audio_onset", X2, NA),
      target_onset = ifelse(X3 == "target_onset", X2, NA),
      end_audio = ifelse(X3 == "end_audio", X2, NA)
    ) 
  
  print("Variables attached")
  print(paste("Df has", ncol(MSGS)))
  print(names(MSGS))
  
  MSGS <- MSGS %>%
    dplyr::group_by(trial) %>%
    fill(c("DISTRACTOR", "TARGET", "t_pos", "d_pos", "fluency", "frequency", "trialtype",
           "nativeness", "CARRIER_F", "CARRIER_D", "clicked", "time_start", "time_end",
           "audio_onset", "target_onset", "end_audio"), .direction = "downup") %>%
    ungroup() %>%
    drop_na(trial)
  
  print("Information about trials attached")
  
  # keep only one row per trial
  
  MSGS <-MSGS %>%
    dplyr::group_by(trial) %>%
    filter(X2 == time_start) %>%
    ungroup()
  
  MSGS <- MSGS[!duplicated(MSGS$X2),]
  MSGS <- MSGS %>%
    select(X2, number_sample, trial, DISTRACTOR, TARGET, t_pos, d_pos, fluency, frequency, trialtype, nativeness,
           CARRIER_F, CARRIER_D, clicked, time_start, time_end, audio_onset, target_onset,
           end_audio)
  
  print("Work on MSGS done")
  
  # 2. FIXATIONS
  
  print("--------- INFORMATION ABOUT FIXATIONS ---------")
  
  FIXATIONS <- pptFSB %>%
    filter(X1 == "EFIX") %>%
    summarise(
      startf = X3,
      endf = X4,
      pos_x = X6,
      pos_y = X7,
      number_sample = number_sample,
      timestamp = X3) %>%
    mutate(
      fix_pos = case_when(
        pos_x >= 106 & pos_x <= 406 & pos_y >= 234 & pos_y <=534 ~ "(-256,",
        pos_x >= 618 & pos_x <= 918 & pos_y >= 234 & pos_y <=534 ~ "(256,",
        TRUE ~ "out"
      )
    )
  
  
  # 3. SACCADESS
  
  # print("--------- INFORMATION ABOUT SACCADES ---------")
  # 
  # SACCADES <- pptFSB %>%
  #   filter(X1 == "ESACC") %>%
  #   summarise(
  #     starts = X3,
  #     ends = X4,
  #     pos_x_s = X6,
  #     pos_y_s = X7,
  #     pos_x_e = X8,
  #     pos_y_e = X9,
  #     number_sample = number_sample,
  #     timestamp = X3
  #   ) %>%
  #   mutate(
  #     sac_pos = case_when(#value is where the eye ended
  #       pos_x_e >= 106 & pos_x_e <= 406 & pos_y_e >= 234 & pos_y_e <=534 ~ "(-256,",
  #       pos_x_e >= 618 & pos_x_e <= 918 & pos_y_e >= 234 & pos_y_e <=534 ~ "(256,",
  #       TRUE ~ "out" 
  #     )
  #   )
  
  
  # 4. BLINKS
  # 
  # print("--------- INFORMATION ABOUT BLINKS ---------")
  # 
  # BLINKS <- pptFSB %>%
  #   filter(X1 == "EBLINK") %>%
  #   summarise(
  #     startb = X3,
  #     endb = X4,
  #     number_sample = number_sample,
  #     timestamp = X3
  #   )
  # 
  # 
  
  print("--------- INFORMATION DIVIDED ---------")
  
  # 3. MERGE EVERYTHING
  
  # 1. Clean pptEYEEVENTS
  
  # remove first rows without eye events
  
  pptEYEEVENTS <- pptEYEEVENTS %>%
    filter(!X5 < 100)
  
  # remove empty columns
  
  pptEYEEVENTS <- droplevels(pptEYEEVENTS)
  
  pptEYEEVENTS <- pptEYEEVENTS %>%
    remove_empty(which = "cols")
  
  # What unifies all files is the timestamp, so first we need to check that it has the same name in all df
  
  colnames(pptEYEEVENTS)[colnames(pptEYEEVENTS) == 'X1'] <- 'timestamp'
  colnames(MSGS)[colnames(MSGS) == 'X2'] <- 'timestamp'
  
  # 2. Bring information about trials (e.g., conditions, times)
  
  # MERGE EYEEVENTS WITH MSGs
  
  # NB nrow(EYEEVENTS) > nrow(MSGs)
  # Tell R to leave the other rows where there is no match as NA
  # Problem is that trials such as stop_trial their timestamp is not in EYEVENTS
  # by using merge and all = TRUE, we match the rows that have the same timestamp 
  # (i.e. start_trial) but we also have rows for stop_trial
  
  DATA <- merge(pptEYEEVENTS, MSGS, by = c("timestamp"), all = TRUE)
  
  # mark end and beginning of trial (we are deleting everything between trials and the time between start trial and the msg being sent to the eye tracker)
  # this still includes the preview window 
  # and the time after audio offset until ppt performs a click (stop_trial)
  
  DATA <- DATA %>%
    fill(trial, time_end) %>%
    mutate(
      trial = ifelse(time_end < timestamp, NA, trial)
    ) %>%
    drop_na(trial) %>%
    fill(DISTRACTOR, TARGET, t_pos, d_pos, fluency, frequency, trialtype, nativeness, CARRIER_F, CARRIER_D, clicked,
         time_start, time_end, audio_onset, target_onset, end_audio)
  
  print("Eye samples and information about trials merged.")
  
  # 3. Bring information about fixations
  # NB all rows have information about eye fixations per sample (i.e., per every 2 ms)
  # FIXATIONS is the average information of each fixation
  # What is important of that df is the endf (end of fixation), as we decide whether to count a fixation on
  # a given epoch depending on when it started and ended
  # NB for this we thus assume that the avg pos_x and pos_y are informative
  
  
  # do it the same way trial information was done
  
  DATA <- left_join(DATA, FIXATIONS, by ="timestamp")
  
  # NB because FIXATIONS df also contains information about rows that we don't have anymore
  # e.g. time elapsed between trials start_ and stop_, there will be empty rows
  # we just need to delete those
  # it's when trials are empty 
  

  DATA <- DATA %>%
    drop_na(trial)
  
  # we have merged them from the beginning of the fixation, we need to copy these values until the end of the fixation
  # watch out because in contrast to trial info, we don't want NAs
  
  DATA <- DATA %>%
    fill(startf, endf, pos_x, pos_y, fix_pos) %>%
    mutate(
      pos_x = ifelse(endf < timestamp, NA, pos_x),
      pos_y = ifelse(endf < timestamp, NA, pos_y),
      fix_pos = ifelse(endf < timestamp, NA, fix_pos)
    )
  
  print("Eye samples and fixations merged.")
  
  # now we have information about the average position of each fixation between the times between that fixation
  # happened
  
  # 4. Bring information about saccaddes
  # Same as before with fixations
  
  # DATA <- left_join(DATA, SACCADES, by = "timestamp")
  # 
  # DATA <- DATA %>%
  #   drop_na(trial)
  # 
  # 
  # DATA <- DATA %>%
  #   fill(pos_x_s, pos_x_e, pos_y_s, pos_y_e, sac_pos, starts, ends) %>%
  #   mutate(
  #     pos_x_s = ifelse(ends < timestamp, NA, pos_x_s),
  #     pos_x_e = ifelse(ends < timestamp, NA, pos_x_e),
  #     pos_y_s = ifelse(ends < timestamp, NA, pos_y_s),
  #     pos_y_e = ifelse(ends < timestamp, NA, pos_y_e),
  #     sac_pos = ifelse(ends < timestamp, NA, sac_pos)
  #   )
  # 
  # print("Eye samples and saccades merged.")
  # 
  # 5. Bring information about blinks
  
  # DATA <- left_join(DATA, BLINKS, by = "timestamp")
  # 
  # DATA <- DATA %>%
  #   drop_na(trial)
  # 
  # DATA <- DATA %>%
  #   fill(startb, endb) %>%
  #   mutate(
  #     blink = ifelse(endb < timestamp, "no", "yes")
  #   )
  # 
  # print("Eye samples and blinks merged.")
  
  # 6. Epochs
  
  # add information about epochs onset
  # NB we do know click onset (because it's technically audio_onset) and noun onset (target_onset)
  # the fact that the times are a bit off kinda hints that audios should be rechecked in order to input the appropiate times
  
  DATA <- DATA %>%
    dplyr::group_by(trial) %>%
    dplyr::mutate(
      click_onset = case_when(
        nativeness == 0 & fluency == 0 & CARRIER_F == "F4.wav"  ~ as.numeric(audio_onset) + 1, # fluent F4 native speaker
        nativeness == 0 & fluency == 0 & CARRIER_F == "F6.wav" ~ as.numeric(audio_onset) + 0, # fluent F6 native speaker
        nativeness == 0 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 0, # fluent F7 native speaker
        nativeness == 1 & fluency == 0 & CARRIER_F == "F3.wav" ~ as.numeric(audio_onset) + 3,# fluent F3 non-native speaker
        nativeness == 1 & fluency == 0 & CARRIER_F == "F5.wav" ~ as.numeric(audio_onset) + 7, # fluent F5 non-native speaker
        nativeness == 1 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 3, # fluent F7 non-native speaker
        nativeness == 0 & fluency == 1 & CARRIER_D == "D2.wav" ~ as.numeric(audio_onset) + 13, # disfluent D2 native speaker
        nativeness == 0 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 4, # disfluent D3 native speaker
        nativeness == 0 & fluency == 1 & CARRIER_D == "D4.wav" ~ as.numeric(audio_onset) + 24, # disfluent D4 native speaker
        nativeness == 1 & fluency == 1 & CARRIER_D == "D1.wav" ~ as.numeric(audio_onset) + 11, # disfluent D1 non-native speaker
        nativeness == 1 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 3, # disfluent D3 non-native speaker
        nativeness == 1 & fluency == 1 & CARRIER_D == "D5.wav" ~ as.numeric(audio_onset) + 10 # disfluent D5 non-native speaker
      ),
      on_onset = case_when(
        (nativeness == 0 & fluency == 0 & CARRIER_F == "F4.wav" ~ as.numeric(audio_onset) + 207),
        nativeness == 0 & fluency == 0 & CARRIER_F == "F6.wav" ~ as.numeric(audio_onset) + 204,
        nativeness == 0 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 143,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F3.wav" ~ as.numeric(audio_onset) + 385,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F5.wav" ~ as.numeric(audio_onset) + 319,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 415,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D2.wav" ~ as.numeric(audio_onset) + 202,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 174,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D4.wav" ~ as.numeric(audio_onset) + 187,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D1.wav" ~ as.numeric(audio_onset) + 405,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 308,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D5.wav" ~ as.numeric(audio_onset) + 254
      ),
      the_onset = case_when(
        (nativeness == 0 & fluency == 0 & CARRIER_F == "F4.wav" ~ as.numeric(audio_onset) + 332),
        nativeness == 0 & fluency == 0 & CARRIER_F == "F6.wav" ~ as.numeric(audio_onset) + 311 ,
        nativeness == 0 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 251,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F3.wav" ~ as.numeric(audio_onset) + 581,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F5.wav" ~ as.numeric(audio_onset) + 523,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 620,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D2.wav" ~ as.numeric(audio_onset) + 317,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 330,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D4.wav" ~ as.numeric(audio_onset) + 351,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D1.wav" ~ as.numeric(audio_onset) + 647,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 545,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D5.wav" ~ as.numeric(audio_onset) + 461
      ),
      um_onset = case_when(
        # nativeness == 0 & fluency == 0 & CARRIER_F == "F4.wav" ~ NA,
        # nativeness == 0 & fluency == 0 & CARRIER_F == "F6.wav" ~ NA ,
        # nativeness == 0 & fluency == 0 & CARRIER_F == "F7.wav" ~ NA,
        # nativeness == 1 & fluency == 0 & CARRIER_F == "F3.wav" ~ NA,
        # nativeness == 1 & fluency == 0 & CARRIER_F == "F5.wav" ~ NA,
        # nativeness == 1 & fluency == 0 & CARRIER_F == "F7.wav" ~ NA,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D2.wav" ~ as.numeric(audio_onset) + 452,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 502,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D4.wav" ~ as.numeric(audio_onset) + 525,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D1.wav" ~ as.numeric(audio_onset) + 979,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 903,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D5.wav" ~ as.numeric(audio_onset) + 828
      ),
      noun_onset = case_when(
        nativeness == 0 & fluency == 0 & CARRIER_F == "F4.wav" ~ as.numeric(audio_onset) + 467,
        nativeness == 0 & fluency == 0 & CARRIER_F == "F6.wav" ~ as.numeric(audio_onset) + 410 ,
        nativeness == 0 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 424,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F3.wav" ~ as.numeric(audio_onset) + 706,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F5.wav" ~ as.numeric(audio_onset) + 658,
        nativeness == 1 & fluency == 0 & CARRIER_F == "F7.wav" ~ as.numeric(audio_onset) + 749,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D2.wav" ~ as.numeric(audio_onset) + 1054,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 1297,
        nativeness == 0 & fluency == 1 & CARRIER_D == "D4.wav" ~ as.numeric(audio_onset) + 1103,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D1.wav" ~ as.numeric(audio_onset) + 1531,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D3.wav" ~ as.numeric(audio_onset) + 1478,
        nativeness == 1 & fluency == 1 & CARRIER_D == "D5.wav" ~ as.numeric(audio_onset) + 1396
      )
    ) %>%
    fill(click_onset, on_onset, the_onset, noun_onset) %>%
    ungroup()
  
  print("Information about epochs done")
  
  # we need to check that onsets and timestamp are in the same scale
  # because timestamp goes 2 by 2, but maybe the onset is in the middle
  
  DATA <- DATA %>%
    dplyr::group_by(trial) %>%
    mutate(
      click_onset = case_when(
        as.numeric(timestamp) %% 2 == 0 & click_onset %% 2 == 0 ~ click_onset,
        as.numeric(timestamp) %% 2 == 0 & click_onset %% 2 != 0 ~ click_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & click_onset %% 2 == 0 ~ click_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & click_onset %% 2 != 0 ~ click_onset
      ),
      on_onset = case_when(
        as.numeric(timestamp) %% 2 == 0 & on_onset %% 2 == 0 ~ on_onset,
        as.numeric(timestamp) %% 2 == 0 & on_onset %% 2 != 0 ~ on_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & on_onset %% 2 == 0 ~ on_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & on_onset %% 2 != 0 ~ on_onset
      ),
      the_onset = case_when(
        as.numeric(timestamp) %% 2 == 0 & the_onset %% 2 == 0 ~ the_onset,
        as.numeric(timestamp) %% 2 == 0 & the_onset %% 2 != 0 ~ the_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & the_onset %% 2 == 0 ~ the_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & the_onset %% 2 != 0 ~ the_onset
      ),
      um_onset = case_when(
        as.numeric(timestamp) %% 2 == 0 & um_onset %% 2 == 0 ~ um_onset,
        as.numeric(timestamp) %% 2 == 0 & um_onset %% 2 != 0 ~ um_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & um_onset %% 2 == 0 ~ um_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & um_onset %% 2 != 0 ~ um_onset
      ),
      noun_onset = case_when(
        as.numeric(timestamp) %% 2 == 0 & noun_onset %% 2 == 0 ~ noun_onset,
        as.numeric(timestamp) %% 2 == 0 & noun_onset %% 2 != 0 ~ noun_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & noun_onset %% 2 == 0 ~ noun_onset + 1,
        as.numeric(timestamp) %% 2 != 0 & noun_onset %% 2 != 0 ~ noun_onset
      )
    )
  
  print("Time scaled")
  
  # 7. Say what is each epoch
  # NB samples are every two seconds, meaning that we may not having the exact number
  # i.e. timeonset may be between one sample
  # we move it one ms behind
  # NB we haven't moved windows by 200 ms.
  
  DATA <- DATA %>%
    dplyr::group_by(trial) %>%
    mutate(
      epoch = ifelse(trialtype == 1, 
                     case_when( # for critical trials
                       as.numeric(time_start) == timestamp | as.numeric(time_start) + 1  == timestamp  ~ "preview",
                       click_onset == timestamp ~ "click",
                       on_onset == timestamp ~ "on",
                       the_onset == timestamp ~ "the",
                       um_onset == timestamp ~ "um",
                       noun_onset == timestamp ~ "noun",
                       as.numeric(end_audio) == timestamp | as.numeric(end_audio) + 1 == timestamp  ~ "end_audio",
                       TRUE ~ NA_character_
                     ),
                     case_when( # for filler trials
                       as.numeric(time_start) == timestamp | as.numeric(time_start) + 1 == timestamp  ~ "preview",
                       as.numeric(audio_onset) == timestamp | as.numeric(audio_onset) + 1 == timestamp  ~ "audio",
                       as.numeric(end_audio) == timestamp | as.numeric(end_audio) + 1 == timestamp~ "end_audio",
                       TRUE ~ NA_character_
                     )),
      epoch_200 = ifelse(trialtype == 1, 
                         case_when( # for critical trials
                           as.numeric(time_start) + 200 == timestamp | as.numeric(time_start) + 201  == timestamp  ~ "preview",
                           click_onset + 200 == timestamp ~ "click",
                           on_onset + 200 == timestamp ~ "on",
                           the_onset + 200 == timestamp ~ "the",
                           um_onset + 200 == timestamp ~ "um",
                           noun_onset + 200 == timestamp ~ "noun",
                           as.numeric(end_audio) == timestamp | as.numeric(end_audio) + 201 == timestamp  ~ "end_audio",
                           TRUE ~ NA_character_
                         ),
                         case_when( # for filler trials
                           as.numeric(time_start) == timestamp | as.numeric(time_start) + 201 == timestamp  ~ "preview",
                           as.numeric(audio_onset) == timestamp | as.numeric(audio_onset) + 201 == timestamp  ~ "audio",
                           as.numeric(end_audio) == timestamp | as.numeric(end_audio) + 201 == timestamp~ "end_audio",
                           TRUE ~ NA_character_
                         ))
    ) %>%
    fill(epoch, epoch_200) %>%
    ungroup() 
  
  print("Epoch column created")
  
  ############ TEST IT EPOCHS ##############
  
  testit_epochs <- DATA %>%
    group_by(trial) %>%
    drop_na(epoch) %>%
    summarise(
      levels = length(levels(as.factor(epoch))),
      labels = as.vector(levels(as.factor(epoch)))
    ) %>%ungroup()
  
  assert(
    'test that epochs were correctly created',
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_76.asc", "sub_79.asc"), length(levels(as.factor(testit_epochs$trial))) == 72, length(levels(as.factor(testit_epochs$trial))) != 72), # 72 trials except sub 26 and 79 who didnt finish
    ifelse(i %!in% c("sub_26.asc", "sub_76.asc", "sub_79.asc"), table(testit_epochs$levels)[1] == 120,table(testit_epochs$levels)[1] != 120), # 120 fillers
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$levels)[2] == 96,table(testit_epochs$levels)[2] != 192), # critical trials for fluent trials
    ifelse(i %!in% c("sub_26.asc", "sub_76.asc", "sub_79.asc"), table(testit_epochs$labels)[1] == 40,table(testit_epochs$labels)[1] != 40), # audio start msg is only for filler so 40
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[2] == 32,table(testit_epochs$labels)[2] != 32), # click msg is only for critical so 32
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_76.asc", "sub_79.asc"), table(testit_epochs$labels)[3] == 72,table(testit_epochs$labels)[3] != 72), # end_audio is for all so 72
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[4] == 32,table(testit_epochs$labels)[4] != 32), # noun is for critical so 32
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[5] == 32,table(testit_epochs$labels)[5] != 32), # on is for critical so 32
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_76.asc", "sub_79.asc"), table(testit_epochs$labels)[6] == 72,table(testit_epochs$labels)[6] != 72), # preview is for all so 72
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[7] == 32,table(testit_epochs$labels)[7] != 32) # the for critical so 32
  )
  
  # Print information
  
  print("Tables with number of levels of epochs")
  
  print(table(testit_epochs$levels)) # should be 120 and 192
  print(table(testit_epochs$labels)) # 7 levels
  print(length(levels(as.factor(testit_epochs$trial)))) # 72 trials
  
  # WINDOW MOVED 200 MS
  
  print("Work on levels of epochs time window + 200")
  
  #  For this time window is harder, the important bit is that we have 32 for critical trials
  
  testit_epochs <- DATA %>%
    group_by(trial) %>%
    drop_na(epoch_200) %>%
    summarise(
      levels = length(levels(as.factor(epoch_200))),
      labels = as.vector(levels(as.factor(epoch_200)))
    ) %>%ungroup()
  
  assert(
    'test that epochs were correctly created for 200',
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_76.asc", "sub_79.asc") , length(levels(as.factor(testit_epochs$trial))) == 72, length(levels(as.factor(testit_epochs$trial))) != 72), # 72 trials
    ifelse(i %!in% c("sub_26.asc", "sub_76.asc", "sub_79.asc"), table(testit_epochs$labels)[1] == 40,table(testit_epochs$labels)[1] != 40), # audio start msg is only for filler so 40
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[2] == 32,table(testit_epochs$labels)[2] != 32), # click msg is only for critical so 32
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[4] == 32,table(testit_epochs$labels)[4] != 32), # noun is for critical so 32
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[5] == 32,table(testit_epochs$labels)[5] != 32), # on is for critical so 32
    ifelse(i %!in% c("sub_26.asc","sub_30.asc", "sub_79.asc"), table(testit_epochs$labels)[7] == 32,table(testit_epochs$labels)[7] != 32) # the for critical so 32
  )
  
  # Print information
  
  print("Tables with number of levels of epochs for time window +200")
  
  print(table(testit_epochs$levels)) # should be 120 and 192
  print(table(testit_epochs$labels)) # 7 levels
  print(length(levels(as.factor(testit_epochs$trial)))) # 72 trials
  
  
  
  # 8. Decide whether there is a fixation there in that epoch
  # Same approach as with trial?
  
  DATA <- DATA %>%
    group_by(trial) %>%
    mutate( # create a column that says when the epoch begins to then use it for fixations
      time_start_epoch = case_when(
        epoch == "click"  ~ click_onset,
        epoch == "on" ~  on_onset,
        epoch == "the" ~ the_onset,
        epoch == "um" ~ um_onset,
        epoch == "noun" ~ noun_onset,
        epoch == "preview" ~ as.numeric(time_start),
        epoch == "end_audio" ~ as.numeric(end_audio),
        epoch == "audio" ~ as.numeric(audio_onset)
      ),
      time_start_epoch200 = case_when(
        epoch_200 == "click"  ~ click_onset + 200,
        epoch_200 == "on" ~  on_onset + 200,
        epoch_200 == "the" ~ the_onset + 200,
        epoch_200 == "um" ~ um_onset + 200,
        epoch_200 == "noun" ~ noun_onset + 200,
        epoch_200 == "preview" ~ as.numeric(time_start) + 200,
        epoch_200 == "end_audio" ~ as.numeric(end_audio) + 200,
        epoch_200 == "audio" ~ as.numeric(audio_onset) + 200
      )) %>% fill(time_start_epoch, time_start_epoch200) %>%
    ungroup()
  
  print("time_start_epoch created")
  
  # this code skips a fixation if it was initiated in the previous epoch
  
  DATA <- DATA %>%
    group_by(trial) %>%
    group_by(epoch) %>%
    mutate(
      fixation_epoch = case_when(
        time_start_epoch <= startf  & 
          as.numeric(timestamp) >= startf  & 
          as.numeric(timestamp) <= endf  ~ fix_pos
      )
    ) %>%
    ungroup()
  
  DATA <- DATA %>%
    group_by(trial) %>%
    group_by(epoch_200) %>%
    mutate(
      fixation_epoch200 = case_when(
        time_start_epoch200 <= startf  & 
          as.numeric(timestamp) >= startf  & 
          as.numeric(timestamp) <= endf  ~ fix_pos
      )
    ) %>%
    ungroup()
  
  print("Fixations per epoch calculated.")
  
  # Arguably, this is it (assuming we are not going to work with saccades or blinks)
  
  print(names(DATA))
  
  # 9. Collapse information by epoch
  # We only need the summarise of fixations per area by epoch and trial 
  # What we have now is a row every 2 scs. Given that we're not doing timecourse analysis, it's not necessary
  
  DATA <- DATA %>%
    mutate( # work on epoch normal
      fix_leftEP = ifelse(fixation_epoch == "(-256,", 1, NA),
      fix_rightEP = ifelse(fixation_epoch == "(256,", 1, NA),
      fix_outEP = ifelse(fixation_epoch == "out", 1, NA)
    ) %>%
    mutate(
      unique_fix_leftEP = ifelse(fix_leftEP == 1, paste(startf, fix_leftEP, sep = "_"), NA),
      unique_fix_rightEP = ifelse(fix_rightEP == 1, paste(startf, fix_rightEP, sep = "_"), NA),
      unique_fix_outEP = ifelse(fix_outEP == 1, paste(startf, fix_outEP, sep = "_"), NA)
    ) %>%
    dplyr::group_by(trial, epoch) %>%
    mutate(
      count_rightEP = length(levels(as.factor(unique_fix_rightEP))),
      count_leftEP = length(levels(as.factor(unique_fix_leftEP))),
      count_outEP = length(levels(as.factor(unique_fix_outEP)))
    ) %>% ungroup() %>%
    mutate( # work on epoch + 200
      fix_left200 = ifelse(fixation_epoch200 == "(-256,", 1, NA),
      fix_right200 = ifelse(fixation_epoch200 == "(256,", 1, NA),
      fix_out200 = ifelse(fixation_epoch200 == "out", 1, NA)
    ) %>%
    mutate(
      unique_fix_left200 = ifelse(fix_left200 == 1, paste(startf, fix_left200, sep = "_"), NA),
      unique_fix_right200 = ifelse(fix_right200 == 1, paste(startf, fix_right200, sep = "_"), NA),
      unique_fix_out200 = ifelse(fix_out200 == 1, paste(startf, fix_out200, sep = "_"), NA)
    ) %>%
    dplyr::group_by(trial, epoch) %>%
    mutate(
      count_right200 = length(levels(as.factor(unique_fix_right200))),
      count_left200 = length(levels(as.factor(unique_fix_left200))),
      count_out200 = length(levels(as.factor(unique_fix_out200)))
    ) %>% ungroup()
  
  print("Collapsed done")
  
  # Keep only the first row
  
  DATA <- DATA %>%
    drop_na(epoch_200) %>%
    group_by(trial, epoch_200) %>%
    filter(row_number()==1) %>%
    ungroup()
  
  # TESTIT
  
  # 10. Clean the df and write a .csv
  
  
  FINAL <- DATA %>%
    select(
      timestamp, X2, X3, trial, DISTRACTOR, TARGET, t_pos, d_pos, fluency, frequency, trialtype, 
      nativeness, CARRIER_F, CARRIER_D, clicked, time_start, time_end, audio_onset, target_onset, startf, endf, pos_x, pos_y,
      fix_pos, epoch, epoch_200, time_start_epoch, time_start_epoch200, fixation_epoch, fixation_epoch200, 
      count_right200, count_left200, count_out200, count_rightEP, count_leftEP, count_outEP
    )
  
  # Write subject number
  
  FINAL <- FINAL %>%
    mutate(
      subject_nr = rep(i, nrow(FINAL))
    )
  
  write.csv(FINAL, filename)
  
}
