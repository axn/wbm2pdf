## Build Requirements ##

For a debian system the following packages should do it:

<pre>
sudo aptitude install --with-recommends \
build-essential \
lua5.1 \
texlive texlive-full texlive-latex-extra incscape pdflatex \
r-recommended libjpeg62 r-cran-hmisc r-cran-plotrix r-cran-stringr  \
</pre>

## Building ##

<pre>
cd tex
make
</pre>

This should:
* download some images and topology snapshots used to beatify the resulting pdf into tex/figures
* render svg-based topology data to pdf image so that its usable by latex
* download the measurement data to test_data
* lua-process the measurement data (ping.lua netp.lua) and create data tables readable by R (R-cran)
* create graphs from data tables (R math tool and script eval.R)
* create pdf from latex file tex/wbmv6.tex


To clean all data:
<pre>
cd tex
make clean_data
</pre>
