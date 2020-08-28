#
# $Id: Makefile 279 2008-01-27 16:19:38Z balaji $
#
# Copyright (C) 2006-2007, University of Chicago. All rights reserved.
#

HEADER = assignment1
PICS = graphics
BIB = bib
TEX = text
NEW_CITES = yis


TARGETS: $(HEADER).pdf
.PHONY: all clean templates

tex_files = $(shell find $(TEX) -name '*.tex' -print)
bib_files = $(shell find $(BIB) -name '*.bib' -print)
pic_files = $(shell find $(PICS) \
		\( -name '*.eps' -print \) -or \( -name '*.epsi' -print \) \
		-or \( -name '*.ps' -print \) -or \( -name '*.png' -print \) \
		-or \( -name '*.fig' -print \) -or \( -name '*.pdf' -print \) \
	)
# pic_files = $(shell if [ -e "$(PICS)/L0_DFD.eps" ]; then find $(PICS) \
# 		\( -name '*.eps' -print \) -or \( -name '*.epsi' -print \) \
# 		-or \( -name '*.ps' -print \) -or \( -name '*.png' -print \) \
# 		-or \( -name '*.fig' -print \) -or \( -name '*.pdf' -print \); else \
#     wget https://iu.box.com/shared/static/ulfzl6tsnzvxn25axajmkputb1eyzs9p.eps -O $(PICS)/L0_DFD.eps; \
#     wget https://iu.box.com/shared/static/11s6te56p9ykdvd5a1yjlhvops17sruz.eps -O $(PICS)/L1_DFD.eps; \
#     wget https://iu.box.com/shared/static/d2xjznlkshmuct6sk78x95k10vtaykrd.eps -O $(PICS)/L2_DFD.eps; \
#     wget https://iu.box.com/shared/static/nk3q7196ibvea0payl1q86gbus3xsmlk.eps -O $(PICS)/Lab2_state_shaffer.eps; \
# 	wget https://iu.box.com/shared/static/77cepprihpext37ot2kjprbobm76gxb1.eps -O $(PICS)/Lab02_part3_jodstein.eps; \
#     fi \
# 	)
$(HEADER).pdf: $(HEADER).tex $(tex_files) $(pic_files) $(bib_files)
	@if test "`which rubber`" != "" ; then \
		rubber -Wall -d $(HEADER) ; \
	else \
		pdflatex $(HEADER) | tee latex.out ; \
			bibtex $(HEADER); \
			bibtex $(NEW_CITES); \
			touch .rebuild; \
		while [ -f .rebuild -o \
			-n "`grep '^LaTeX Warning:.*Rerun' latex.out`" ]; do \
			rm -f .rebuild; \
			pdflatex $(HEADER) | tee latex.out; \
		done ; \
		rm -f latex.out ; \
	fi

clean:
	find . \( -name '*.blg' -print \) -or \( -name '*.aux' -print \) -or \
		\( -name '*.bbl' -print \) -or \( -name '*~' -print \) -or \
		\( -name '*.lof' -print \) -or \( -name '*.lot' -print \) -or \
		\( -name '*.idx' -print \) -or \( -name '*.soc' -print \) -or \
		\( -name '*.toc' -print \) | xargs rm -f; \
	rm -f $(HEADER).dvi $(HEADER).log $(HEADER).ps $(HEADER).pdf $(HEADER).out \
		_region_* TAGS 
