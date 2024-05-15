# Load R packages
lapply(c("tidyverse", "filesstrings", "expss", "readxl", "writexl"), 
       require, character.only = TRUE)



# Set the folder containing current R script as working directory
setwd(".")
print(paste("Current working dir:", getwd()))
if (dir.exists("original_files")==FALSE){
  dir.create("original_files")
}
if (dir.exists("cache")==FALSE){
  dir.create("cache")
}



# Import index
index <- read_excel("./MEA_analysis_index.xlsx")
index$Folder_path <- paste0("./", index$Folder, "/")
index$File_path <- paste0("./", index$Folder, "/", index$File)

## folders
folders <- unique(index$Folder_path)

## file refs
index$Folder_file <- paste0(index$Folder, "_", index$File)
files <- unique(index[c("Folder_file", "File_path")])

## well refs
index$Folder_file_well <- paste0(index$Folder, "_", index$File, "_", index$Well_number)
index$UWI <- paste0(index$Recording_date, "_", 
                    index$Genotype, "_", 
                    index$Days_in_vitro, "_", 
                    index$Treatment, "_", 
                    index$Treatment_duration, "_", 
                    index$Plate_number, "_", 
                    index$Well_number)
well_refs <- unique(index[c("Folder_file_well", "UWI", "Genotype", "Treatment", "Treatment_duration", "File_path")])

## electrode ref
electrode_refs <- c()
for (i in 1:nrow(index)) {
  electrode_ref <- index[i,] %>%
    slice(rep(row_number(), each = 64))
  electrode_ref$electrode <- c("_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", 
                               "_21", "_22", "_23", "_24", "_25", "_26", "_27", "_28", 
                               "_31", "_32", "_33", "_34", "_35", "_36", "_37", "_38", 
                               "_41", "_42", "_43", "_44", "_45", "_46", "_47", "_48", 
                               "_51", "_52", "_53", "_54", "_55", "_56", "_57", "_58", 
                               "_61", "_62", "_63", "_64", "_65", "_66", "_67", "_68", 
                               "_71", "_72", "_73", "_74", "_75", "_76", "_77", "_78", 
                               "_81", "_82", "_83", "_84", "_85", "_86", "_87", "_88")
  electrode_ref$electrode <- paste0(electrode_ref$Well_number, electrode_ref$electrode)
  electrode_ref$Folder_file_well_electrode <- paste0(electrode_ref$Folder, "_", electrode_ref$File, "_", electrode_ref$electrode)
  electrode_ref$UEI <- paste0(electrode_ref$Recording_date, "_", 
                              electrode_ref$Genotype, "_", 
                              electrode_ref$Days_in_vitro, "_", 
                              electrode_ref$Treatment, "_", 
                              electrode_ref$Treatment_duration, "_", 
                              electrode_ref$Plate_number, "_", 
                              electrode_ref$electrode)
  electrode_ref <- electrode_ref[c("Folder_file_well_electrode", "UEI", "UWI", "Genotype", "Treatment", "Treatment_duration", "File_path")]
  electrode_refs <- rbind(electrode_refs, electrode_ref)
}
rm("electrode_ref", "i", "index")



# Pre-process data

## Electrodes
electrode_dataset <- c()
for (i in 1:nrow(files)) {
  filename <- as.character(files[i,"Folder_file"])
  filename_e <- paste0(filename, ".e")
  filepath <- as.character(files[i,"File_path"])
  lines <- readLines(filepath)[168:186]
  writeLines(lines, filename_e)
  electrode_data <- read.csv(filename_e, header = TRUE, row.names = 1)
  electrode_data <- as.data.frame(t(electrode_data))
  write.table(electrode_data, file=filename_e, sep=",", row.names = TRUE)
  electrode_data <- read.csv(filename_e, header = TRUE, row.names = NULL)
  colnames(electrode_data) <- c("Electrode",
                                "Number_of_Spikes",
                                "Mean_Firing_Rate_(Hz)",
                                "ISI_Coefficient_of_Variation",
                                "Number_of_Bursts",
                                "Burst_Duration_Avg_(s)",
                                "Burst_Duration_Std_(s)",
                                "Number_of_Spikes_per_Burst_Avg",
                                "Number_of_Spikes_per_Burst_Std",
                                "Mean_ISI_within_Burst_Avg",
                                "Mean_ISI_within_Burst_Std",
                                "Median_ISI_within_Burst_Avg",
                                "Median_ISI_within_Burst_Std",
                                "Inter-Burst_Interval_Avg_(s)",
                                "Inter-Burst_Interval_Std_(s)",
                                "Burst_Frequency_(Hz)",
                                "IBI_Coefficient_of_Variation",
                                "Normalized_Duration_IQR",
                                "Burst_Percentage")
  electrode_data$Folder_file <- paste0(filename)
  electrode_data$Folder_file_well_electrode <- paste0(filename, "_", electrode_data$Electrode)
  electrode_data <- electrode_data %>%
    filter(!grepl(".csv_X", Folder_file_well_electrode))
  electrode_data <- add_columns(electrode_data, electrode_refs, by="Folder_file_well_electrode")
  electrode_dataset <- rbind (electrode_dataset, electrode_data)
}
file.move(list.files(pattern='*.csv.e'), "./cache/electrode_data", overwrite=TRUE)
rm("electrode_data", "filename", "filename_e", "filepath", "lines", "i", "electrode_refs")

## Wells
well_dataset <- c()
for (i in 1:nrow(files)) {
  filename <- as.character(files[i,"Folder_file"])
  filename_w <- paste0(filename, ".w")
  filepath <- as.character(files[i,"File_path"])
  lines <- readLines(filepath)[121:166]
  writeLines(lines, filename_w)
  well_data <- read.csv(filename_w, header = TRUE, row.names = 1)
  well_data <- as.data.frame(t(well_data))
  write.table(well_data, file=filename_w, sep=",", row.names = TRUE)
  well_data <- read.csv(filename_w, header = TRUE, row.names = NULL)
  colnames(well_data) <- c("Well",
                           "Treatment_ID",
                           "Number_of_Spikes",
                           "Mean_Firing_Rate_(Hz)",
                           "Number_of_Active_Electrodes",
                           "Weighted_Mean_Firing_Rate_(Hz)",
                           "ISI_Coefficient_of_Variation_Avg",
                           "Number_of_Bursts",
                           "Number_of_Bursting_Electrodes",
                           "Burst_Duration_Avg(s)",
                           "Burst_Duration_Std(s)",
                           "Number_of_Spikes_per_Burst_Avg",
                           "Number_of_Spikes_per_Burst_Std",
                           "Mean_ISI_within_Burst_Avg",
                           "Mean_ISI_within_Burst_Std",
                           "Median_ISI_within_Burst_Avg",
                           "Median_ISI_within_Burst_Std",
                           "Inter_Burst_Interval_Avg(s)",
                           "Inter_Burst_Interval_Std(s)",
                           "Burst_Frequency_Avg(Hz)",
                           "Burst_Frequency_Std(Hz)",
                           "Normalized_Duration_IQR_Avg",
                           "Normalized_Duration_IQR_Std",
                           "IBI_Coefficient_of_Variation_Avg",
                           "IBI_Coefficient_of_Variation_Std",
                           "Burst_Percentage_Avg",
                           "Burst_Percentage_Std",
                           "Number_of_Network_Bursts",
                           "Network_Burst_Frequency(Hz)",
                           "Network_Burst_Duration_Avg(sec)",
                           "Network_Burst_Duration_Std(sec)",
                           "Number_of_Spikes_per_Network_Burst_Avg",
                           "Number_of_Spikes_per_Network_Burst_Std",
                           "Number_of_Elecs_Participating_in_Burst_Avg",
                           "Number_of_Elecs_Participating_in_Burst_Std",
                           "Number_of_Spikes_per_Network_Burst_per_Channel_Avg",
                           "Number_of_Spikes_per_Network_Burst_per_Channel_Std",
                           "Network_Burst_Percentage",
                           "Network_IBI_Coefficient_of_Variation",
                           "Network_ISI_Coefficient_of_Variation",
                           "Network_Normalized_Duration_IQR",
                           "Area_Under_Normalized_Cross_Correlation",
                           "Area_Under_Cross_Correlation",
                           "Width_at_Half_Height_of_Normalized_Cross_Correlation",
                           "Width_at_Half_Height_of_Cross_Correlation",
                           "Synchrony_Index")
  well_data$Folder_file <- paste0(filename)
  well_data$Folder_file_well <- paste0(filename, "_", well_data$Well)
  well_data <- well_data %>%
    filter(!grepl("*.csv_X", Folder_file_well))
  well_data <- add_columns(well_data, well_refs, by="Folder_file_well")
  well_dataset <- rbind (well_dataset, well_data)
}
file.move(list.files(pattern='*.csv.w'), "./cache/well_data", overwrite=TRUE)
rm("well_data", "filename", "filename_w", "filepath", "lines", "i", "well_refs")

## Parameters
parameter_dataset <- c()
for (i in 1:nrow(files)) {
  filename <- as.character(files[i,"Folder_file"])
  filename_p <- paste0(filename, ".p")
  filepath <- as.character(files[i,"File_path"])
  lines <- readLines(filepath)[c(2,57,62,76)]
  writeLines(lines, filename_p)
  parameter_data <- read.csv(filename_p, header = TRUE, row.names = 1)
  parameter_data <- as.data.frame(t(parameter_data))
  write.table(parameter_data, file=filename_p, sep=",", row.names = TRUE)
  parameter_data <- read.csv(filename_p, header = TRUE, row.names = NULL)
  colnames(parameter_data) <- c("Recording_Name",
                                "Synchrony_Window",
                                "Minimum_Spike_Rate_for_Active_Electrode",
                                "Analysis_Duration(s)")
  parameter_data$Recording_Name <- gsub('Recording.Name..', '', parameter_data$Recording_Name)
  parameter_data <- parameter_data %>%
    filter(!grepl("*X.", Recording_Name))
  parameter_data$Folder_file <- paste0(filename)
  parameter_data <- parameter_data[c("Folder_file",
                                     "Recording_Name",
                                     "Synchrony_Window",
                                     "Minimum_Spike_Rate_for_Active_Electrode",
                                     "Analysis_Duration(s)")]
  parameter_data <- add_columns(parameter_data, files, by="Folder_file")
  parameter_dataset <- rbind (parameter_dataset, parameter_data)
}
file.move(list.files(pattern='*.csv.p'), "./cache/parameter_data", overwrite=TRUE)
rm("parameter_data", "filename", "filename_p", "filepath", "lines", "i", "files")

## cleanup
electrode_dataset <- add_columns(electrode_dataset, parameter_dataset, by="Folder_file")
electrode_dataset <- electrode_dataset[c("UEI",
                                         "UWI",
                                         "Genotype",
                                         "Treatment",
                                         "Treatment_duration",
                                         "Number_of_Spikes",
                                         "Mean_Firing_Rate_(Hz)",
                                         "ISI_Coefficient_of_Variation",
                                         "Number_of_Bursts",
                                         "Burst_Duration_Avg_(s)",
                                         "Burst_Duration_Std_(s)",
                                         "Number_of_Spikes_per_Burst_Avg",
                                         "Number_of_Spikes_per_Burst_Std",
                                         "Mean_ISI_within_Burst_Avg",
                                         "Mean_ISI_within_Burst_Std",
                                         "Median_ISI_within_Burst_Avg",
                                         "Median_ISI_within_Burst_Std",
                                         "Inter-Burst_Interval_Avg_(s)",
                                         "Inter-Burst_Interval_Std_(s)",
                                         "Burst_Frequency_(Hz)",
                                         "IBI_Coefficient_of_Variation",
                                         "Normalized_Duration_IQR",
                                         "Burst_Percentage",
                                         "Synchrony_Window",
                                         "Minimum_Spike_Rate_for_Active_Electrode",
                                         "Analysis_Duration(s)")]
well_dataset <- add_columns(well_dataset, parameter_dataset, by="Folder_file")
well_dataset <- well_dataset[c("UWI",
                               "Genotype",
                               "Treatment",
                               "Treatment_duration",
                               "Treatment_ID",
                               "Number_of_Spikes",
                               "Mean_Firing_Rate_(Hz)",
                               "Number_of_Active_Electrodes",
                               "Weighted_Mean_Firing_Rate_(Hz)",
                               "ISI_Coefficient_of_Variation_Avg",
                               "Number_of_Bursts",
                               "Number_of_Bursting_Electrodes",
                               "Burst_Duration_Avg(s)",
                               "Burst_Duration_Std(s)",
                               "Number_of_Spikes_per_Burst_Avg",
                               "Number_of_Spikes_per_Burst_Std",
                               "Mean_ISI_within_Burst_Avg",
                               "Mean_ISI_within_Burst_Std",
                               "Median_ISI_within_Burst_Avg",
                               "Median_ISI_within_Burst_Std",
                               "Inter_Burst_Interval_Avg(s)",
                               "Inter_Burst_Interval_Std(s)",
                               "Burst_Frequency_Avg(Hz)",
                               "Burst_Frequency_Std(Hz)",
                               "Normalized_Duration_IQR_Avg",
                               "Normalized_Duration_IQR_Std",
                               "IBI_Coefficient_of_Variation_Avg",
                               "IBI_Coefficient_of_Variation_Std",
                               "Burst_Percentage_Avg",
                               "Burst_Percentage_Std",
                               "Number_of_Network_Bursts",
                               "Network_Burst_Frequency(Hz)",
                               "Network_Burst_Duration_Avg(sec)",
                               "Network_Burst_Duration_Std(sec)",
                               "Number_of_Spikes_per_Network_Burst_Avg",
                               "Number_of_Spikes_per_Network_Burst_Std",
                               "Number_of_Elecs_Participating_in_Burst_Avg",
                               "Number_of_Elecs_Participating_in_Burst_Std",
                               "Number_of_Spikes_per_Network_Burst_per_Channel_Avg",
                               "Number_of_Spikes_per_Network_Burst_per_Channel_Std",
                               "Network_Burst_Percentage",
                               "Network_IBI_Coefficient_of_Variation",
                               "Network_ISI_Coefficient_of_Variation",
                               "Network_Normalized_Duration_IQR",
                               "Area_Under_Normalized_Cross_Correlation",
                               "Area_Under_Cross_Correlation",
                               "Width_at_Half_Height_of_Normalized_Cross_Correlation",
                               "Width_at_Half_Height_of_Cross_Correlation",
                               "Synchrony_Index",
                               "Synchrony_Window",
                               "Minimum_Spike_Rate_for_Active_Electrode",
                               "Analysis_Duration(s)")]
for (folder in folders) {
  dest <- gsub("./", "", folder)
  dest <- paste0("./original_files/", dest, "/")
  file.rename(folder,dest)
}
file.move("MEA_analysis_index.xlsx", "./original_files/", overwrite=TRUE)
rm("folder", "dest", "folders", "parameter_dataset")



# data QC

## well_dataset qc
well_qc_dataset <- filter(well_dataset, Number_of_Active_Electrodes >= 20)
UWI_qc <- unique(as.character(well_qc_dataset$UWI))
well_qc_dataset <- well_qc_dataset[c("UWI",
                                     "Genotype",
                                     "Treatment",
                                     "Treatment_duration",
                                     "Network_Burst_Frequency(Hz)",
                                     "Network_Burst_Duration_Avg(sec)",
                                     "Network_Burst_Duration_Std(sec)",
                                     "Network_IBI_Coefficient_of_Variation",
                                     "Number_of_Spikes_per_Network_Burst_per_Channel_Avg",
                                     "Number_of_Spikes_per_Network_Burst_per_Channel_Std",
                                     "Network_ISI_Coefficient_of_Variation",
                                     "Area_Under_Normalized_Cross_Correlation",
                                     "Width_at_Half_Height_of_Normalized_Cross_Correlation",
                                     "Synchrony_Index",
                                     "Number_of_Active_Electrodes",
                                     "Number_of_Bursting_Electrodes",
                                     "Network_Burst_Percentage")]
well_qc_dataset <- mutate(well_qc_dataset, `well_within-network-bursts_firing-rate-avg` = `Number_of_Spikes_per_Network_Burst_per_Channel_Avg` / `Network_Burst_Duration_Avg(sec)`)
well_qc_dataset <- mutate(well_qc_dataset, `well_within-network-bursts_firing-rate-std` = `Number_of_Spikes_per_Network_Burst_per_Channel_Std` / `Network_Burst_Duration_Avg(sec)`)
well_qc_dataset <- mutate(well_qc_dataset, `well_percentage_bursting_electrodes` = `Number_of_Bursting_Electrodes` / `Number_of_Active_Electrodes` *100)

### well group
well_WT_DMSO_000 <- filter(well_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "000min" & Treatment == "DMSO")
well_WT_DMSO_000$Group <- rep("WT_DMSO_Pre",nrow(well_WT_DMSO_000))
well_WT_DHPG_000 <- filter(well_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "000min" & Treatment == "DHPG")
well_WT_DHPG_000$Group <- rep("WT_DHPG_Pre",nrow(well_WT_DHPG_000))
well_WT_MPEP_000 <- filter(well_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "000min" & Treatment == "MPEP")
well_WT_MPEP_000$Group <- rep("WT_MPEP_Pre",nrow(well_WT_MPEP_000))
well_WT_DMSO_060 <- filter(well_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "060min" & Treatment == "DMSO")
well_WT_DMSO_060$Group <- rep("WT_DMSO_60min",nrow(well_WT_DMSO_060))
well_WT_DHPG_060 <- filter(well_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "060min" & Treatment == "DHPG")
well_WT_DHPG_060$Group <- rep("WT_DHPG_60min",nrow(well_WT_DHPG_060))
well_WT_MPEP_060 <- filter(well_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "060min" & Treatment == "MPEP")
well_WT_MPEP_060$Group <- rep("WT_MPEP_60min",nrow(well_WT_MPEP_060))
well_HET_DMSO_000 <- filter(well_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "000min" & Treatment == "DMSO")
well_HET_DMSO_000$Group <- rep("HET_DMSO_Pre",nrow(well_HET_DMSO_000))
well_HET_DHPG_000 <- filter(well_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "000min" & Treatment == "DHPG")
well_HET_DHPG_000$Group <- rep("HET_DHPG_Pre",nrow(well_HET_DHPG_000))
well_HET_MPEP_000 <- filter(well_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "000min" & Treatment == "MPEP")
well_HET_MPEP_000$Group <- rep("HET_MPEP_Pre",nrow(well_HET_MPEP_000))
well_HET_DMSO_060 <- filter(well_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "060min" & Treatment == "DMSO")
well_HET_DMSO_060$Group <- rep("HET_DMSO_60min",nrow(well_HET_DMSO_060))
well_HET_DHPG_060 <- filter(well_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "060min" & Treatment == "DHPG")
well_HET_DHPG_060$Group <- rep("HET_DHPG_60min",nrow(well_HET_DHPG_060))
well_HET_MPEP_060 <- filter(well_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "060min" & Treatment == "MPEP")
well_HET_MPEP_060$Group <- rep("HET_MPEP_60min",nrow(well_HET_MPEP_060))
well_qc_dataset <- rbind(well_WT_DMSO_000,
                         well_WT_DHPG_000,
                         well_WT_MPEP_000,
                         well_WT_DMSO_060,
                         well_WT_DHPG_060,
                         well_WT_MPEP_060,
                         well_HET_DMSO_000,
                         well_HET_DHPG_000,
                         well_HET_MPEP_000,
                         well_HET_DMSO_060,
                         well_HET_DHPG_060,
                         well_HET_MPEP_060)
well_qc_dataset$Group <- factor(well_qc_dataset$Group, levels = c("WT_DMSO_Pre",
                                                                  "WT_DHPG_Pre",
                                                                  "WT_MPEP_Pre",
                                                                  "WT_DMSO_60min",
                                                                  "WT_DHPG_60min",
                                                                  "WT_MPEP_60min",
                                                                  "HET_DMSO_Pre",
                                                                  "HET_DHPG_Pre",
                                                                  "HET_MPEP_Pre",
                                                                  "HET_DMSO_60min",
                                                                  "HET_DHPG_60min",
                                                                  "HET_MPEP_60min"))
rm("well_WT_DMSO_000",
   "well_WT_DHPG_000",
   "well_WT_MPEP_000",
   "well_WT_DMSO_060",
   "well_WT_DHPG_060",
   "well_WT_MPEP_060",
   "well_HET_DMSO_000",
   "well_HET_DHPG_000",
   "well_HET_MPEP_000",
   "well_HET_DMSO_060",
   "well_HET_DHPG_060",
   "well_HET_MPEP_060")

## electrode_dataset qc
electrode_qc_dataset <- c()
for (uwi in UWI_qc) {
  electrode_subset <- filter(electrode_dataset, UWI == uwi)
  analysis_duration <- mean(electrode_subset$`Analysis_Duration(s)`, na.rm = TRUE)
  active_threshold <- analysis_duration * 5 / 60
  electrode_subset <- filter(electrode_subset, Number_of_Spikes >= active_threshold)
  electrode_qc_dataset <- rbind(electrode_qc_dataset, electrode_subset)
}
electrode_qc_dataset <- electrode_qc_dataset[c("UEI",
                                               "UWI",
                                               "Genotype",
                                               "Treatment",
                                               "Treatment_duration",
                                               "Mean_Firing_Rate_(Hz)",
                                               "ISI_Coefficient_of_Variation",
                                               "Burst_Frequency_(Hz)",
                                               "Burst_Duration_Avg_(s)",
                                               "Burst_Duration_Std_(s)",
                                               "IBI_Coefficient_of_Variation",
                                               "Number_of_Spikes_per_Burst_Avg",
                                               "Number_of_Spikes_per_Burst_Std",
                                               "Mean_ISI_within_Burst_Avg",
                                               "Mean_ISI_within_Burst_Std",
                                               "Burst_Percentage")]
electrode_qc_dataset <- mutate(electrode_qc_dataset, `electrode_within-bursts_firing-rate-avg` = `Number_of_Spikes_per_Burst_Avg` / `Burst_Duration_Avg_(s)`)
electrode_qc_dataset <- mutate(electrode_qc_dataset, `electrode_within-bursts_firing-rate-std` = `Number_of_Spikes_per_Burst_Std` / `Burst_Duration_Avg_(s)`)
electrode_qc_dataset <- mutate(electrode_qc_dataset, `electrode_within-bursts_ISI-CoV` = `Mean_ISI_within_Burst_Std` / `Mean_ISI_within_Burst_Avg`)
rm("electrode_subset", "analysis_duration", "active_threshold", "uwi", "UWIs", "UWI_qc")

### electrode group
elec_WT_DMSO_000 <- filter(electrode_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "000min" & Treatment == "DMSO")
elec_WT_DMSO_000$Group <- rep("WT_DMSO_Pre",nrow(elec_WT_DMSO_000))
elec_WT_DHPG_000 <- filter(electrode_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "000min" & Treatment == "DHPG")
elec_WT_DHPG_000$Group <- rep("WT_DHPG_Pre",nrow(elec_WT_DHPG_000))
elec_WT_MPEP_000 <- filter(electrode_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "000min" & Treatment == "MPEP")
elec_WT_MPEP_000$Group <- rep("WT_MPEP_Pre",nrow(elec_WT_MPEP_000))
elec_WT_DMSO_060 <- filter(electrode_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "060min" & Treatment == "DMSO")
elec_WT_DMSO_060$Group <- rep("WT_DMSO_60min",nrow(elec_WT_DMSO_060))
elec_WT_DHPG_060 <- filter(electrode_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "060min" & Treatment == "DHPG")
elec_WT_DHPG_060$Group <- rep("WT_DHPG_60min",nrow(elec_WT_DHPG_060))
elec_WT_MPEP_060 <- filter(electrode_qc_dataset, Genotype == "ATF4WT" & Treatment_duration == "060min" & Treatment == "MPEP")
elec_WT_MPEP_060$Group <- rep("WT_MPEP_60min",nrow(elec_WT_MPEP_060))
elec_HET_DMSO_000 <- filter(electrode_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "000min" & Treatment == "DMSO")
elec_HET_DMSO_000$Group <- rep("HET_DMSO_Pre",nrow(elec_HET_DMSO_000))
elec_HET_DHPG_000 <- filter(electrode_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "000min" & Treatment == "DHPG")
elec_HET_DHPG_000$Group <- rep("HET_DHPG_Pre",nrow(elec_HET_DHPG_000))
elec_HET_MPEP_000 <- filter(electrode_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "000min" & Treatment == "MPEP")
elec_HET_MPEP_000$Group <- rep("HET_MPEP_Pre",nrow(elec_HET_MPEP_000))
elec_HET_DMSO_060 <- filter(electrode_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "060min" & Treatment == "DMSO")
elec_HET_DMSO_060$Group <- rep("HET_DMSO_60min",nrow(elec_HET_DMSO_060))
elec_HET_DHPG_060 <- filter(electrode_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "060min" & Treatment == "DHPG")
elec_HET_DHPG_060$Group <- rep("HET_DHPG_60min",nrow(elec_HET_DHPG_060))
elec_HET_MPEP_060 <- filter(electrode_qc_dataset, Genotype == "ATF4HET" & Treatment_duration == "060min" & Treatment == "MPEP")
elec_HET_MPEP_060$Group <- rep("HET_MPEP_60min",nrow(elec_HET_MPEP_060))
electrode_qc_dataset <- rbind(elec_WT_DMSO_000,
                              elec_WT_DHPG_000,
                              elec_WT_MPEP_000,
                              elec_WT_DMSO_060,
                              elec_WT_DHPG_060,
                              elec_WT_MPEP_060,
                              elec_HET_DMSO_000,
                              elec_HET_DHPG_000,
                              elec_HET_MPEP_000,
                              elec_HET_DMSO_060,
                              elec_HET_DHPG_060,
                              elec_HET_MPEP_060)
electrode_qc_dataset$Group <- factor(electrode_qc_dataset$Group, levels = c("WT_DMSO_Pre",
                                                                            "WT_DHPG_Pre",
                                                                            "WT_MPEP_Pre",
                                                                            "WT_DMSO_60min",
                                                                            "WT_DHPG_60min",
                                                                            "WT_MPEP_60min",
                                                                            "HET_DMSO_Pre",
                                                                            "HET_DHPG_Pre",
                                                                            "HET_MPEP_Pre",
                                                                            "HET_DMSO_60min",
                                                                            "HET_DHPG_60min",
                                                                            "HET_MPEP_60min"))
rm("elec_WT_DMSO_000",
   "elec_WT_DHPG_000",
   "elec_WT_MPEP_000",
   "elec_WT_DMSO_060",
   "elec_WT_DHPG_060",
   "elec_WT_MPEP_060",
   "elec_HET_DMSO_000",
   "elec_HET_DHPG_000",
   "elec_HET_MPEP_000",
   "elec_HET_DMSO_060",
   "elec_HET_DHPG_060",
   "elec_HET_MPEP_060")



# Export spreadsheet
now <- Sys.time()
filename <- paste0(format(now, "%Y-%m-%d_"), "MEAsy_aggregate.xlsx")
sheets <- list("Electrode_dataset" = electrode_dataset,
               "Well_dataset" = well_dataset,
               "Electrode_qc_dataset" = electrode_qc_dataset,
               "Well_qc_dataset" = well_qc_dataset)
write_xlsx(sheets, filename)
rm("now", "filename", "sheets")



# Plot

# 1.electrode_spikes_firing-rate
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = log(`Mean_Firing_Rate_(Hz)`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Firing Rate (Hz)",
       x = "",
       y = "log(Mean Firing Rate)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("1.electrode_spikes_firing-rate.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 2.electrode_spikes_ISI-CoV
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = `ISI_Coefficient_of_Variation`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Inter-Spike Interval Coefficient of Variation (lower value indicates higher regularity)",
       x = "",
       y = "ISI Coefficient of Variation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2.electrode_spikes_ISI-CoV.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 3.electrode_bursts_burst-freq
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = log(`Burst_Frequency_(Hz)`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Burst Frequency (Hz)",
       x = "",
       y = "log(Burst Frequency)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("3.electrode_bursts_burst-freq.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 4.electrode_bursts_burst-duration-avg
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = log(`Burst_Duration_Avg_(s)`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Burst Duration Avg (s)",
       x = "",
       y = "log(Burst Duration Avg)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("4.electrode_bursts_burst-duration-avg.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 5.electrode_bursts_IBI-CoV
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = `IBI_Coefficient_of_Variation`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Inter-Burst Interval Coefficient of Variation (lower value indicates higher regularity)",
       x = "",
       y = "IBI Coefficient of Variation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("5.electrode_bursts_IBI-CoV.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 6.electrode_within-bursts_firing-rate
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = log(`Number_of_Spikes_per_Burst_Avg`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Number of Spikes per Burst Avg",
       x = "",
       y = "log(Number of Spikes per Burst Avg)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("6.electrode_within-bursts_firing-rate.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 7.electrode_within-bursts_ISI-CoV
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = `electrode_within-bursts_ISI-CoV`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Inter-Spike Interval Coefficient of Variation within Bursts(lower value indicates higher regularity)",
       x = "",
       y = "ISI Coefficient of Variation within bursts") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("7.electrode_within-bursts_ISI-CoV.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 8.electrode_percentage_bursting_spikes
p <- ggplot(electrode_qc_dataset, aes(x = Group, y = `Burst_Percentage`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Electrode dataset: Bursting Spikes Percentage (%)",
       x = "",
       y = "Burst percentage (%)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("8.electrode_percentage_bursting_spikes.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 9.well_network-bursts_burst-freq
p <- ggplot(well_qc_dataset, aes(x = Group, y = log(`Network_Burst_Frequency(Hz)`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Network Burst Frequency (Hz)",
       x = "",
       y = "log(Network Burst Frequency)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("9.well_network-bursts_burst-freq.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 10.well_network-bursts_burst-duration-avg
p <- ggplot(well_qc_dataset, aes(x = Group, y = log(`Network_Burst_Duration_Avg(sec)`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Network Burst Duration Avg (s)",
       x = "",
       y = "log(Network Burst Duration Avg)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("10.well_network-bursts_burst-duration-avg.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 11.well_network-bursts_IBI-CoV
p <- ggplot(well_qc_dataset, aes(x = Group, y = `Network_IBI_Coefficient_of_Variation`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Network Inter-Burst Interval Coefficient of Variation (lower value indicates higher regularity)",
       x = "",
       y = "Network IBI Coefficient of Variation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("11.well_network-bursts_IBI-CoV.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 12.well_within-network-bursts_firing-rate-avg
p <- ggplot(well_qc_dataset, aes(x = Group, y = log(`Number_of_Spikes_per_Network_Burst_per_Channel_Avg`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Number of Spikes per Network Burst per Channel Avg",
       x = "",
       y = "log(Number of Spikes per Network Burst per Channel Avg)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("12.well_within-network-bursts_firing-rate-avg.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 13.well_within-network-bursts_ISI-CoV
p <- ggplot(well_qc_dataset, aes(x = Group, y = `Network_ISI_Coefficient_of_Variation`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Network Inter-Spike Interval Coefficient of Variation (lower value indicates higher regularity)",
       x = "",
       y = "Network ISI Coefficient of Variation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("13.well_within-network-bursts_ISI-CoV.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 14.well_synchrony_cross-correlation-area
p <- ggplot(well_qc_dataset, aes(x = Group, y = log(`Area_Under_Normalized_Cross_Correlation`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Area Under Normalized Cross Correlation (higher value indicates greater synchrony)",
       x = "",
       y = "log(Area Under Normalized Cross Correlation)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("14.well_synchrony_cross-correlation-area.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 15.well_synchrony_cross-correlation-width
p <- ggplot(well_qc_dataset, aes(x = Group, y = log(`Width_at_Half_Height_of_Normalized_Cross_Correlation`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Width at Half Height of Normalized Cross Correlation (lower value indicates greater synchrony)",
       x = "",
       y = "log(Width at Half Height of Normalized Cross Correlation)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("15.well_synchrony_cross-correlation-width.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 16.well_synchrony_synchrony-index
p <- ggplot(well_qc_dataset, aes(x = Group, y = log(`Synchrony_Index`), fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Synchrony Index (higher value indicates greater synchrony)",
       x = "",
       y = "log(Synchrony Index)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("16.well_synchrony_synchrony-index.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 17.well_percentage_bursting_electrodes
p <- ggplot(well_qc_dataset, aes(x = Group, y = `well_percentage_bursting_electrodes`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Bursting Electrodes Percentage (%)",
       x = "",
       y = "Bursting Electrodes Percentage") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("17.well_percentage_bursting_electrodes.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")

# 18.well_percentage_network_bursting_spikes
p <- ggplot(well_qc_dataset, aes(x = Group, y = `Network_Burst_Percentage`, fill = Group)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.3) +
  labs(title = "Well dataset: Network Burst Percentage (%)",
       x = "",
       y = "Network Burst Percentage") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("18.well_percentage_network_bursting_spikes.pdf", plot = p, width = 10, height = 6, units = "in", dpi = 1200)
rm("p")



# exit
rm(list = ls())
