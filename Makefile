docname = wbmv7
rawUrl = http://wibed.confine-project.eu/resultsdir
saveDir = wbm-axn

$(docname).pdf : $(docname).tex figures/battlemeshv7.png autofig outline.sty
	pdflatex $(docname).tex && pdflatex $(docname).tex
	echo

figures/battlemeshv7.png:
	mkdir -p figures && \
		wget -cO figures/battlemeshv7.png -U 'anything' "http://battlemesh.org/EndorsementTemplate?action=AttachFile&do=get&target=wbmv7.png"

autofig : process.sh ping.R ping.lua
	./process.sh YES YES YES YES #ping, netperf, top, overhead

clean_data : 
	rm -rf test_data

clean_figures :
	rm -rf figures

clean_tex:
	rm -f $(docname).pdf $(docname).log $(docname).out $(docname).aux $(docname).toc 


clean_all :
	make clean_data
	make clean_figures
	make clean_tex
