#!/bin/bash

PING=${1:-YES}
NETPERF=${2:-YES}
TOP=${3:-YES}
TCPDUMP=${4:-YES}

HOMEDIR="$(pwd)"
SAVEDIR="wbm-axn"
URLBASE=http://wibed.confine-project.eu/results

URLDATA=""

#Stationary tests:
URLDATA="$URLDATA \
wbmv7-axn-16_2014-05-16_19-28-43/wbmv7-axn-16_2014-05-16_19-28-43_wibed-3e9db0.tar.gz \
wbmv7-axn-16_2014-05-16_19-28-43/wbmv7-axn-16_2014-05-16_19-28-43_wibed-8a417e.tar.gz \
wbmv7-axn-16_2014-05-16_19-28-43/wbmv7-axn-16_2014-05-16_19-28-43_wibed-e61936.tar.gz \
wbmv7-axn-16_2014-05-16_19-28-43/wbmv7-axn-16_2014-05-16_19-28-43_wibed-f41ab0.tar.gz \
"


#Broken Crossed streams tests (dead daemons):
#URLDATA="$URLDATA \
#wbmv7-axn-17_2014-05-16_20-13-20/wbmv7-axn-17_2014-05-16_20-13-20_wibed-8a417e.tar.gz \
#wbmv7-axn-17_2014-05-16_20-13-20/wbmv7-axn-17_2014-05-16_20-13-20_wibed-c24174.tar.gz \
#wbmv7-axn-17_2014-05-16_20-13-20/wbmv7-axn-17_2014-05-16_20-13-20_wibed-e61936.tar.gz \
#wbmv7-axn-17_2014-05-16_20-13-20/wbmv7-axn-17_2014-05-16_20-13-20_wibed-f41ab0.tar.gz \
#"

#Mobile tests:
URLDATA="$URLDATA \
wbmv7-axn-19_2014-05-16_21-35-33/wbmv7-axn-19_2014-05-16_21-35-33_wibed-3e9db0.tar.gz \
wbmv7-axn-19_2014-05-16_21-35-33/wbmv7-axn-19_2014-05-16_21-35-33_wibed-51aac8.tar.gz \
wbmv7-axn-19_2014-05-16_21-35-33/wbmv7-axn-19_2014-05-16_21-35-33_wibed-c2427a.tar.gz \
wbmv7-axn-19_2014-05-16_21-35-33/wbmv7-axn-19_2014-05-16_21-35-33_wibed-e61936.tar.gz \
wbmv7-axn-19_2014-05-16_21-35-33/wbmv7-axn-19_2014-05-16_21-35-33_wibed-f41ab0.tar.gz \
"


for D in $URLDATA; do
    cd $HOMEDIR
    EXPDIR="$( echo $D | awk -F'/' '{print $1}' )"
    NODETGZ="$( echo $D | awk -F'/' '{print $2}' )"
    NODEDIR="$( echo $NODETGZ | awk -F '.tar.gz' '{print $1}' )"
    if \
	mkdir -p test_data/$EXPDIR/$NODEDIR && \
	cd       test_data/$EXPDIR/$NODEDIR && \
	( [ -f $NODETGZ ] || wget -c $URLBASE/$D ) && \
	( [ -d ./$SAVEDIR ] || ( tar -xzvf $NODETGZ && mv $(find | grep "$SAVEDIR$") . ) ) && \
	cd ./$SAVEDIR \
	; then


	if [ "$PING" == "YES" ]; then
	    ../../../../ping.lua  && \
		../../../../ping.R --data=./ping.data --stat=./ping.stat --imgdir=./ \
		--name=s1rtt --type=ecdfVsRtt  --groups=1 \
		--name=s1rvh --type=rttVsHops  --groups=1 \
		--name=s1tim --type=dataVsTime --groups=1
	fi

	if [ "$NETPERF" == "YES" ]; then
	    ../../../../netperf.sh > netperf.data && \
		../../../../plotData.R  --vanilla --args --tests="netperf" --protos="olsr1 bmx batadv olsr2"
	fi

	if [ "$TOP" == "YES" ]; then
	    ../../../../top.sh > top.data && \
		../../../../plotData.R  --vanilla --args --tests="top" --protos="olsr1 bmx batadv olsr2"
	fi

	if [ "$TCPDUMP" == "YES" ]; then
	    ../../../../tcpdump.sh > tcpdump.data && \
		../../../../plotData.R  --vanilla --args --tests="tcpdump" --protos="olsr1 bmx batadv olsr2"
	fi


	cd $HOMEDIR
    else
	echo "Failed extracting $NODETGZ"
    fi
done



echo Created PDFs:
find test_data/ | grep -e "pdf$"
