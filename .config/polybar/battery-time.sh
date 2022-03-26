#!/usr/bin/env bash
set -euo pipefail

# can also just go through /sys/class/power_supply/CMB0
# for direct info rather than extracting from acpi

remain=$(acpi | head -n 1 | cut -d ' ' -f 5 | cut -d ':' -f -2)

first=$(echo "$remain" | cut -d ':' -f 1)
second=$(echo "$remain" | cut -d ':' -f 2)

status=$(cat /sys/class/power_supply/CMB0/status);
# if [[ $status == "Full"]];
# then
#   echo Waiting...
d=$(date -d "+$first hour +$second minute" "+%l:%M %P")
# fi

if [[ $first == "discharging" ]]
# if acpi hasn't detected dis/charging yet
then
   echo Waiting...
elif [[ $status == "Discharging" ]];
then
    #echo "Dis";
    echo "till $d";
elif [[ $status == "Charging" ]];
then
    #echo "Charg";
    echo "till $d";
else
    echo "";
    #:;
fi


