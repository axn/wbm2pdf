#!/bin/bash

# $0 [infile]

## capture:
##tcpdump -nve -i wbm1 -s 66 -w /save/wbm-axn/tcpdump.raw \
##port 6240 or port 698 or port 269 or port 6696 or \
##\(ether proto 0x4305 and \( ether[14]==0x00 or ether[14]==0x44 \) \)

PROTOS=${PROTOS:-"olsr1 bmx batadv babel olsr2"}
DATADIR=${DATADIR:-"."}
TPCDUMPFILE=${TPCDUMPFILE:-"tcpdump.raw"}
INFILE=${1:-"${DATADIR}/${TPCDUMPFILE}"}
INTERVAL=${INTERVAL:-"1"}


## header
LINE="time totalP totalB"
for proto in ${PROTOS}; do
  LINE="${LINE} ${proto}P ${proto}B"
done
echo ${LINE}


## To add new protos following line must be tunned
##
## WARNING: the current one is meant for TShark 1.10.6 (v1.10.6 from master-1.10)
## we have noticed differences in output formats from one tshark version to another
## 
## we have also noticed differences in the filter
## e.g. in previous verisions a "," after ${INTERVAL}, was needed for the total
tshark -r ${INFILE} -q -z "io,stat,${INTERVAL},\
  udp.port==698,udp.port==6240,vlan.etype==0x4305 && data.data[0]==00,udp.port==6696,udp.port==269" \
  | tail -n +17 | head -n -1 | cut -d'>' -f 2- | tr -d '|' | sed "s/ 0 / NA /g"
