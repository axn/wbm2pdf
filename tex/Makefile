



docname = wbmv6
figures = figures/*.png figures/*.pdf figures/*.svg
autofig = ../img/*.pdf
rawData = downloads.battlemesh.org/WBMv6/test_data/





$(docname).pdf : $(docname).tex $(figures) $(autofig) outline.sty
	pdflatex $(docname).tex && pdflatex $(docname).tex


$(figures) : 
	cd figures && wget -O battlemeshv6.png      http://battlemesh.org/moin_static196/common/battlemeshv6.png
	cd figures && wget -O geographical_map1.png http://downloads.battlemesh.org/WBMv6/geographical_map1.png
	cd figures && wget -O geographical_map2.png http://downloads.battlemesh.org/WBMv6/geographical_map2.png
	cd figures && wget -O topo0.svg "http://battlemesh.org/BattleMeshV6/Tests?action=AttachFile&do=get&target=topo0.svg" &&\
	inkscape -D -z --file=topo0.svg --export-pdf=topo0.pdf }


$(autofig) : ../eval.R ../ping.data ../ping.stat
	rm -f $(autofig)
	../eval.R --data=../ping.data --stat=../ping.stat --imgdir=../img  \
\
--name=mArtt --type=ecdfVsRtt  --groups="1,2,3" \
--name=mArvh --type=rttVsHops  --groups="1,2,3" \
\
--name=m0rtt --type=ecdfVsRtt  --groups=1 \
--name=m0tim --type=dataVsTime --groups=1 \
--name=m0hop --type=ecdfVsHops --groups=1 \
--name=m0rvh --type=rttVsHops  --groups=1 \
--name=m1rtt --type=ecdfVsRtt  --groups=2 \
--name=m1tim --type=dataVsTime --groups=2 \
--name=m1hop --type=ecdfVsHops --groups=2 \
--name=m1rvh --type=rttVsHops  --groups=2 \
--name=m2rtt --type=ecdfVsRtt  --groups=3 \
--name=m2tim --type=dataVsTime --groups=3 \
--name=m2hop --type=ecdfVsHops --groups=3 \
--name=m2rvh --type=rttVsHops  --groups=3 \
\
--name=sABrtt --type=ecdfVsRtt  --groups='5,6,7,8,9,10,13,15,17,18,19,23,24,25,26,27,28,30,32,34,35,36,37,38,39' \
--name=sABrvh --type=rttVsHops  --groups='5,6,7,8,9,10,13,15,17,18,19,23,24,25,26,27,28,30,32,34,35,36,37,38,39' \
--name=sArtt --type=ecdfVsRtt  --groups='5,6,7,8,9,10,13,15,17,18,19,23,24,25,26,27,28,30' \
--name=sArvh --type=rttVsHops  --groups='5,6,7,8,9,10,13,15,17,18,19,23,24,25,26,27,28,30' \
--name=sBrtt --type=ecdfVsRtt  --groups='32,34,35,36,37,38,39' \
--name=sBrvh --type=rttVsHops  --groups='32,34,35,36,37,38,39' \
\
$$( for i in $$(seq 4 50); do echo -n "--name=s$${i}rtt --type=ecdfVsRtt --groups=$$i  --name=s$${i}rvh --type=rttVsHops --groups=$$i  --name=s$${i}tim --type=dataVsTime --groups=$$i  "; done  )



../ping.stat ../ping.data : ../ping.lua ../test_data
	cd .. && ./ping.lua


../test_data : 
	cd .. && rm -rf $(rawData) && wget -rl1 http://$(rawData) && \
	ln -s $(rawData) test_data && \
	cd test_data && \
	for i in $$( ls *.tar.gz ); do tar -xzvf $$i; done && \
	cd  ./random_ping_test1 && for f in $$(ls | awk -F'_' '{print $$4}' | sort -u ); do cat *$$f >> screenlog.all; done && \
	cd ../random_ping_test2 && for f in $$(ls | awk -F'_' '{print $$4}' | sort -u ); do cat *$$f >> screenlog.all; done


clean_data : 
	rm -rf ../$$( echo $(rawData) | awk -F'/' '{print $$1}' ) 
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
