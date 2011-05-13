export TEXINPUTS := ./inputs//:${TEXINPUTS}
export BIBINPUTS := ./inputs//:${BIBINPUTS}

SWEAVEOPTS = -c=cache 
SWEAVEOPTS = ""

TEXFILES = \
	  Master-TeachingStatsWithR.tex \
	  Chap-GettingStarted.tex \
	  Chap-Manipulate.tex \
	  Chap-SimulationBased.tex \
	  Chap-Multivariate.tex \
	  Chap-ByHand.tex \
	  Chap-MoreExamples.tex \
	  Chap-Core.tex \
	  Chap-Internet.tex \
	  Chap-Calculus.tex \
	  Chap-Misc.tex \
	  Chap-Handouts.tex \
	  Chap-MoreR.tex \
	  Chap-RIntro.tex \


all: Master-TeachingStatsWithR.pdf

tex: $(TEXFILES)

clean:
	- \rm Chap-*.tex 
	- \rm Chap-*.aux 
	- \rm Master-*aux 
	- \rm Master-*tex 
	- \rm Master-*blg 
	- \rm Master-*ind 
	- \rm Master-TeachingStatsWithR*.pdf

%.tex: %.Rnw setup.R inputs/problems.sty inputs/authNote.sty \
	inputs/sfsect.sty inputs/USCOTS.bib
	bin/sweave.sh -nc $(SWEAVEOPTS) $< 

Master-TeachingStatsWithR.pdf: $(TEXFILES) 
	./bin/latexmk -f -pdf  Master-TeachingStatsWithR.tex
