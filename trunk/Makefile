# 

SOURCES = b16.lyx b16-eng.lyx Makefile \
	b16.nw b16-eng.nw b16.v b16-eval.v b16-eval-test.v \
	boot.asm b16.asm usb.asm b16_eval.rbf la.c b16.fs \
	COPYING b16.pdf b16-eng.pdf hex2boot \
	add-mem b16-usb-test.v usb-test.v usb.v \
	b16-eval-usb.v b16-eval-usb-test.v usbphys.v ram1024.v stackram.v \
        sieve.asm b16-gcc-slides.lyx gcc/b16.h gcc/b16.md.in

all:	b16.v b16.pdf b16-eng.pdf la

gcc/b16.md:	gcc/b16.md.in
	m4 <$< >$@

%.nw:	%.lyx
	lyx --execute "buffer-export literate" $<

%.tex:	%.nw
	noweave -delay -latex $< | sed -e 's/1<<dep/1<{}<dep/g' >$@
	latex $@

%.v:	%.nw
	notangle -Rb16.v $< >$@

%.dvi:	%.tex
	latex $<

%.ps:	%.dvi
	dvips -Pams -Pcmz -Ppdf $< -o $@

%.ps.gz:	%.ps
	gzip <$< >$@

%.pdf:	%.ps
	ps2pdf $< $@

la:	la.c
	gcc -O2 la.c -o la

dist:	$(SOURCES)
	mkdir b16
	tar cf - $(SOURCES) | (cd b16; tar xf -)
	tar jcf b16.tar.bz2 b16
	rm -rf b16
