#!/bin/bash

##  $0 [/path/to/logdirectory]
## output
##  Throughput[KB/s] Time[s]

##MIGRATED TCP STREAM TEST from :: (::) port 0 AF_INET6 to fdba:12:4174::1 () port 0 AF_INET6 : demo
##Interim result: 8605.39 10^3bits/s over 1.158 seconds ending at 1400269226.150
##Interim result: 8889.70 10^3bits/s over 1.076 seconds ending at 1400269227.226

PROTOS=${PROTOS:-"olsr1 bmx batadv babel olsr2"}
DATADIR=${DATADIR:-$1}
DATADIR=${DATADIR:-"."}

echo "$(pwd)" >&2

## header
LINE=""
for proto in ${PROTOS}; do
  LINE="${LINE} ${proto}TP ${proto}T"
done
echo ${LINE}


PROVES=$(ls ${DATADIR}/netperf-bmx-* | wc -w)
## TODO build PROVES by scaning over PROTOS and selecting the longest; then if PROVES==0 ERROR

for it in $(seq 1 ${PROVES}); do
  LINE=""
  for proto in ${PROTOS}; do
    TP=0
    TIME=0
    if [ -f "${DATADIR}/netperf-${proto}-${it}.log" ]; then
      while read INLINE; do
        WORD1=$(echo ${INLINE} | awk '{print $1}')
        if [ "${WORD1}" == "Interim" ]; then
          iTIME=$(echo "${INLINE}" | awk '{print $6}')
          iTP=$(echo "${iTIME}*$(echo ${INLINE} | awk '{print $3}')" | bc -l) ## [Kb/s]

          TIME=$(echo ${TIME}+${iTIME} | bc -l)
          TP=$(echo ${TP}+${iTP} | bc -l)
        fi
      done < "${DATADIR}/netperf-${proto}-${it}.log"
      TP=$(echo "scale=3; ${TP}/8" | bc -l) ## [KB/s]
    else
      TIME="NA"
      TP="NA"
    fi
    LINE="${LINE} ${TP} ${TIME}"
  done
               ## strip 0 values (empty logfiles; node not reachable)
  echo ${LINE} | sed -e 's/ 0 0 / NA NA /g'
done
