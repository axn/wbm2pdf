#!/bin/bash

## input
##   $0 in_file ["pattern 1" ["..." ["pattern 6"]]]
## output
##  TP[KB/s] and T[s] per protocol
## first line is discarted

##top -b -d 2 -n 5  ## -b batch; -n itreations (for the cpu load => first discarted -wrong numbers!!!!); -d time period
##Mem: 34460K used, 92048K free, 0K shrd, 7700K buff, 8052K cached
##CPU: 0% usr 10% sys 0% nic 0% idle 80% io 0% irq 10% sirq
##Load average: 0.58 0.42 0.31 5/54 6105
##PID PPID USER STAT VSZ %VSZ %CPU COMMAND
##6085 6080 root D 2980 2% 10% {wibed-node} /usr/bin/lua /usr/sbin/wibed-node
##4695 2 root RW 0 0% 10% [kworker/u2:2]
##1126 1 root S 1564 1% 0% /sbin/netifd
##2066 1 root S 1424 1% 0% bmx6

PROTOS=${PROTOS:-"olsr1 bmx batadv babel olsr2"}  ## TODO automatise protos
DATADIR=${DATADIR:-"."}
TOPFILE=${TOPFILE:-"top.log"}
INFILE=${1:-"${DATADIR}/${TOPFILE}"}

shift
PAT1=${1:-"/usr/sbin/olsrd -f /var/etc/olsrd.conf"}
PAT2=${2:-"/usr/sbin/bmx6 -f /etc/config/bmx6 -d0"}
PAT3=${3:-"BATMANADVANCE"}
PAT4=${4:-"/usr/sbin/babeld -D -I /var/run/babeld.pid"}
PAT5=${5:-"/usr/sbin/olsrd2 --load /etc/olsrd2.conf"}

DISCARTED1=""

NA="NA"

function resetoutline {
  OUT1="${NA} ${NA} ${NA}"; OUT2="${NA} ${NA} ${NA}"; OUT3="${NA} ${NA} ${NA}"; OUT4="${NA} ${NA} ${NA}"; OUT5="${NA} ${NA} ${NA}"
}

function printline {
  echo ${OUTLINE} | tr -d '%' | tr -d 'K'
}

resetoutline

## Header
LINE="usedK freeK shrdK buffK cachedK usr100 sys100 nic100 idle100 io100 irq100 sirq100"
for proto in ${PROTOS}; do
  LINE="${LINE} ${proto}VSZ ${proto}100VSZ ${proto}100CPU"
done
echo ${LINE}

while read INLINE; do
  WORD1=$(echo ${INLINE} | awk '{print $1}')
  case "${WORD1}" in
    "Mem:")
      if [ -n "${OUTLINE}" ]; then  ##discard first line
        if [ -z "${DISCARTED1}" ]; then
          resetoutline
          DISCARTED1="bla"
        else
          printline
        fi
      fi
      resetoutline
      MEM=$(echo ${INLINE} | awk '{print $2 " " $4 " " $6 " " $8 " " $10}')
    ;;
    "CPU:")
      CPU=$(echo ${INLINE} | awk '{print $2 " " $4 " " $6 " " $8 " " $10 " " $12 " " $14}')
    ;;
    [0-9]*)
      WORD8=$(echo ${INLINE} | cut -d' ' -f 8-) # is not trivial to cut from nth line with awk
      case "${WORD8}" in
        "$PAT1"*)
          OUT1=$(echo ${INLINE} | awk '{print $5 " " $6 " " $7}')
        ;;
        "$PAT2"*)
          OUT2=$(echo ${INLINE} | awk '{print $5 " " $6 " " $7}')
        ;;
        "$PAT3"*)
          OUT3=$(echo ${INLINE} | awk '{print $5 " " $6 " " $7}')
        ;;
        "$PAT4"*)
          OUT4=$(echo ${INLINE} | awk '{print $5 " " $6 " " $7}')
        ;;
        "$PAT5"*)
          OUT5=$(echo ${INLINE} | awk '{print $5 " " $6 " " $7}')
        ;;
      esac
    ;;
    *)
      :
    ;;
  esac
  
  OUTLINE="${MEM} ${CPU} ${OUT1} ${OUT2} ${OUT3} ${OUT4} ${OUT5}"
  
done < <(grep "Mem:\|CPU:\|${PAT1}\|${PAT2}\|${PAT3}\|${PAT4}\|${PAT5}" ${INFILE})

printline
