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
