#!/bin/bash
# This script combines all MEA data from each csv file in each subfolder (excluding "_spike_counts.csv" filies)

# set terminal font color and initialize
TEXT_YELLOW='\e[1;33m'
TEXT_GREEN='\e[1;32m'
TEXT_RESET='\e[0m'
unset folder
unset folder_name
unset format_folder_name
unset combined_file


# set working directory to where this script is
cd "${0%/*}"
echo -e "\nThe current folder is: ""${TEXT_YELLOW}"${0%/*}"${TEXT_RESET}\n"


# ask whether to proceed
read -n1 -s -r -p "$(echo -e 'Would you like to combine all MEA data in this folder and the subfolders? '$TEXT_YELLOW'[y/n/c]'$TEXT_RESET)"$'\n' choice
case "$choice" in

  y|Y ) # notify start
        echo -e "\n${TEXT_YELLOW}Combining all MEA data...${TEXT_RESET}\n" && sleep 1

            # find all subfolders
            find . -mindepth 1 -maxdepth 5 -type d -print0 | while read -d $'\0' folder
            do

                  # check whethere there is any MEA original data
                  if ls "$folder"/*"(000).csv" &> /dev/null; then

                        # create a empty auto_combined.csv file
                        folder_name=$(basename "$folder")
                        format_folder_name=${folder_name// /_}
                        combined_file="$folder"/#-"$format_folder_name"-auto_combined.csv
                        [ ! -f "$combined_file" ] && touch "$combined_file"
                        echo -e "Data combined from:,[""$folder_name""] folder" > "$combined_file"  # this line will overwrite existing files

                        # copy desired lines from each original file
                        for file in "$folder"/*"(000).csv"; do

                              echo -e "\n" >> "$combined_file"  # for adding a gap
                              sed -n "2p" "$file" >> "$combined_file"  # for "Recording Name"
                              sed -n "121p" "$file" >> "$combined_file"  # for "Well Averages"
                              sed -n "123p" "$file" >> "$combined_file"  # for "Number of Spikes"
                              sed -n "125p" "$file" >> "$combined_file"  # for "Number of Active Electrodes"
                              sed -n "128p" "$file" >> "$combined_file"  # for "Number of Bursts"
                              sed -n "130p" "$file" >> "$combined_file"  # for "Burst Duration - Avg (s)"
                              sed -n "132p" "$file" >> "$combined_file"  # for "Number of Spikes per Burst - Avg"
                              sed -n "140p" "$file" >> "$combined_file"  # for "Burst Frequency - Avg (Hz)"
                              sed -n "166p" "$file" >> "$combined_file"  # for "Synchrony Index"
                              #sed -n "Np" "$file" >> "$combined_file"  # where "N" is your desired line number

                        done

                        # notify the finishing of data processing in each folder
                        echo -e "- Data in ""${TEXT_GREEN}"$folder/"${TEXT_RESET}"" processed."
                  fi

            done

        # notify end
        echo -e " \n${TEXT_GREEN}MEA data combined!${TEXT_RESET}\n" && sleep 1;;

  * )   # notify cancellation
        echo -e " \n${TEXT_YELLOW}MEA data not combined.${TEXT_RESET}\n" && sleep 1;;

esac
