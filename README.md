## Build Requirements ##

For a debian system the following packages should do it:

<pre>
sudo aptitude install --with-recommends \
build-essential \
lua5.1 \
texlive texlive-full texlive-latex-extra inkscape pdflatex \
r-recommended libjpeg62 r-cran-hmisc r-cran-plotrix r-cran-stringr
</pre>

## Building ##

<pre>
make
</pre>

This should:
* download some images and optional topology snapshots used to beatify the resulting pdf
* optinally render svg-based topology data to pdf image so that its usable by latex
* download the measurement data to test_data
* lua-process the measurement data (ping.lua,...) and create data tables readable by R (R-cran)
* create graphs from data tables (R math tool and script ping.R)
* create pdf from latex file wbmv7.tex


To clean all data:
<pre>
cd tex
make clean_data
</pre>

## Data ##

### Source ###
http://Server/results/ExperimentID/ExperimentID_wibed-NodeID.tar.gz
e.g. http://wibed.confine-project.eu/results/wbmv7-axn-16_2014-05-16_19-28-43/wbmv7-axn-16_2014-05-16_19-28-43_wibed-f41ab0.tar.gz

### Experiments and nodes selection ###
file: process.sh 
variable: URLDATA

## ping sample ###
<pre>
PING fdba:12:1ab0::1(fdba:12:1ab0::1) 400 data bytes
408 bytes from fdba:12:1ab0::1: icmp_seq=1 ttl=62 time=7.07 ms
408 bytes from fdba:12:1ab0::1: icmp_seq=2 ttl=62 time=92.1 ms
...
...
...
--- fdba:12:1ab0::1 ping statistics ---
199 packets transmitted, 198 received, 0% packet loss, time 199081ms
rtt min/avg/max/mdev = 4.757/147.583/1057.819/227.404 ms, pipe 2
<\pre>
