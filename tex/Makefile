
docname = wbmv7
figures = figures/*.png figures/*.pdf figures/*.svg
autofig = ../img/*.pdf
rawUrl = http://wibed.confine-project.eu/resultsdir
saveDir = wbm-axn

#rawData = downloads.battlemesh.org/WBMv6/test_data/
#rawData = wbmv7-axn-09_2014-05-14_16-20-08/wbmv7-axn-09_2014-05-14_16-20-08_wibed-e61936.tar.gz
rawData = wbmv7-axn-10_2014-05-14_20-01-50/wbmv7-axn-10_2014-05-14_20-01-50_wibed-e61936.tar.gz
#rawData = wbmv7-axn-10_2014-05-14_20-01-50/wbmv7-axn-10_2014-05-14_20-01-50_wibed-51aac8.tar.gz


$(docname).pdf : $(docname).tex $(figures) $(autofig) outline.sty
#	pdflatex $(docname).tex && pdflatex $(docname).tex
	echo


$(figures) : 
#	cd figures && wget -O battlemeshv7.png      http://battlemesh.org/moin_static196/common/battlemeshv7.png



$(autofig) ../test_data ../test_data/$(rawData): 
	mkdir -p ../test_data/$$( echo $(rawData) | awk -F'/' '{print $$1}' )  && \
	cd       ../test_data/$$( echo $(rawData) | awk -F'/' '{print $$1}' )  && \
	wget $(rawUrl)/$(rawData) && \
	tar -xzvf $$( echo $(rawData) | awk -F'/' '{print $$2}' ) && \
	cd ./$(saveDir) && ../../../ping.lua  && \
	../../../eval.R --data=./ping.data --stat=./ping.stat --imgdir=../../../img --name=s1rtt --type=ecdfVsRtt --groups=1  --name=s1rvh --type=rttVsHops --groups=1  --name=s1tim --type=dataVsTime --groups=1  



clean_data : 
	rm -f test_data ../ping.stat ../ping.data

clean_autofig : 
	rm -f $(autofig)

clean_figures :
	rm -f $(figures)

clean_all :
	make clean_data
	make clean_autofig
	make clean_figures
	rm -f $(docname).pdf $(docname).log $(docname).out $(docname).aux $(docname).toc 
