



docname = wbmv6
figures = figures/*.png figures/*.pdf figures/*.svg
autofig = ../img/*.pdf
rawData = downloads.battlemesh.org/WBMv6/test_data/



$(docname).pdf : Makefile $(docname).tex $(figures) $(autofig)
	pdflatex $(docname).tex



$(figures) : 
	cd figures && wget -O battlemeshv6.png      http://battlemesh.org/moin_static196/common/battlemeshv6.png
	cd figures && wget -O geographical_map1.png http://downloads.battlemesh.org/WBMv6/geographical_map1.png
	cd figures && wget -O geographical_map2.png http://downloads.battlemesh.org/WBMv6/geographical_map2.png
	cd figures && wget -O topo0.svg "http://battlemesh.org/BattleMeshV6/Tests?action=AttachFile&do=get&target=topo0.svg" &&\
	inkscape -D -z --file=topo0.svg --export-pdf=topo0.pdf }


$(autofig) : Makefile ../eval.R ../wbmv6.data ../wbmv6.stat
	rm -f $(autofig)
	../eval.R --data=../wbmv6.data --stat=../wbmv6.stat --imgdir=../img  \
--name=m0rtt --type=ecdfVsRtt  --groups=1 \
--name=m0tim --type=dataVsTime --groups=1 \
--name=m0hop --type=ecdfVsHops --groups=1 \

not_now :
	\
\
--name=m1rtt --type=ecdfVsRtt  --groups=2 \
--name=m1tim --type=dataVsTime --groups=2 \
\
--name=m2rtt --type=ecdfVsRtt  --groups=3 \
--name=m2tim --type=dataVsTime --groups=3 \
\
--name=s4rtt --type=ecdfVsRtt  --groups=4 \
--name=s5rtt --type=ecdfVsRtt  --groups=5 \
--name=s6rtt --type=ecdfVsRtt  --groups=6 \
--name=s7rtt --type=ecdfVsRtt  --groups=7 \
--name=s8rtt --type=ecdfVsRtt  --groups=8 \
--name=s9rtt --type=ecdfVsRtt  --groups=9 \
\
--name=sArtt --type=ecdfVsRtt  --groups='4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21' \
--name=sBrtt --type=ecdfVsRtt  --groups='22,23,24,25,26,27,28'                            \
\
--name=s10rtt --type=ecdfVsRtt  --groups=10 \
--name=s11rtt --type=ecdfVsRtt  --groups=11 \
--name=s12rtt --type=ecdfVsRtt  --groups=12 \
--name=s13rtt --type=ecdfVsRtt  --groups=13 \
--name=s14rtt --type=ecdfVsRtt  --groups=14 \
--name=s15rtt --type=ecdfVsRtt  --groups=15 \
--name=s16rtt --type=ecdfVsRtt  --groups=16 \
--name=s17rtt --type=ecdfVsRtt  --groups=17 \
--name=s18rtt --type=ecdfVsRtt  --groups=18 \
--name=s19rtt --type=ecdfVsRtt  --groups=19 \
--name=s20rtt --type=ecdfVsRtt  --groups=20 \
--name=s21rtt --type=ecdfVsRtt  --groups=21 \
--name=s22rtt --type=ecdfVsRtt  --groups=22 \
--name=s23rtt --type=ecdfVsRtt  --groups=23 \
--name=s24rtt --type=ecdfVsRtt  --groups=24 \
--name=s25rtt --type=ecdfVsRtt  --groups=25 \
--name=s26rtt --type=ecdfVsRtt  --groups=26 \
--name=s27rtt --type=ecdfVsRtt  --groups=27 \
--name=s28rtt --type=ecdfVsRtt  --groups=28 \





../wbmv6.stat ../wbmv6.data : ../eval.lua ../test_data
	cd .. && ./eval.lua


../test_data : 
	cd .. && rm -rf $(rawData) && wget -rl1 http://$(rawData) && \
	ln -s $(rawData) test_data && \
	cd test_data && \
	for i in $$( ls *.tar.gz ); do tar -xzvf $$i; done && \
	cd random_ping_test1 && for f in $$(ls ping_test_olsr_* | awk -F'_' '{print $$4}' ); do cat *$$f >> screenlog.1; done


clean_data : 
	cd .. && rm -rf $(rawData) test_data

clean_autofig : 
	rm -f $(autofig)

clean_figures :
	rm -f $(figures)

clean_all :
	make clean_autofig
	make clean_figures
	rm -f $(docname).pdf
