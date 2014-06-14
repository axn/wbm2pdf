
docname = wbmv7
figures = figures/*.png # figures/*.pdf figures/*.svg
rawUrl = http://wibed.confine-project.eu/resultsdir
saveDir = wbm-axn


$(docname).pdf : $(docname).tex $(figures) autofig outline.sty
	pdflatex $(docname).tex && pdflatex $(docname).tex
	echo


$(figures) : 
	mkdir -p figures && cd figures && wget -cO battlemeshv7.png http://battlemesh.org/moin_static196/common/battlemeshv7.png

autofig : process.sh ping.R ping.lua
	./process.sh YES YES YES YES #ping, netperf, top, overhead

clean_data : 
	rm -f test_data

clean_figures :
	rm -f $(figures)

clean_tex:
	rm -f $(docname).pdf $(docname).log $(docname).out $(docname).aux $(docname).toc 


clean_all :
	make clean_data
	make clean_figures
	make clean_tex
