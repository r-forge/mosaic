export TEXINPUTS := ./inputs//:${TEXINPUTS}
export BIBINPUTS := ./inputs//:${BIBINPUTS}

SWEAVEOPTS = -c=cache 
SWEAVEOPTS = ""

TEXFILES = \
	  Master-TeachingStatsWithR.tex \
	  Chap-FrontMatter.tex \
	  Chap-GettingStarted.tex \
	  Chap-Manipulate.tex \
	  Chap-SimulationBased.tex \
	  Chap-ByHand.tex \
	  Chap-MoreExamples.tex \
	  Chap-Core.tex \
	  Chap-Calculus.tex \
	  Chap-Misc.tex \
	  Chap-Handouts.tex \
	  Chap-RIntro.tex \
	  Chap-MoreR.tex \
	  Chap-Internet.tex \
	  Chap-Multivariate.tex \

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
