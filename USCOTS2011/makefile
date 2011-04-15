export TEXINPUTS := ./inputs//:${TEXINPUTS}
export BIBINPUTS := ./inputs//:${BIBINPUTS}


TEXFILES = \
	  Master-TeachingStatsWithR.tex \
	  Chap-GettingStarted.tex \
	  Chap-Manipulate.tex \
	  Chap-SimulationBased.tex \
	  Chap-Multivariate.tex \
	  Chap-Core.tex \
	  Chap-Internet.tex \
	  Chap-Calculus.tex \
	  Chap-Misc.tex \
	  Chap-MoreR.tex \
	  Chap-RIntro.tex \


all: Master-TeachingStatsWithR.pdf

clean:
	- \rm Chap-*.tex Chap-*.aux Master-*aux Master-*tex \
		Master-*blg Master-*ind Master-TeachingStatsWithR*.pdf

%.tex: %.Rnw
	bin/sweave.sh -nc -c=cache $<

Master-TeachingStatsWithR.pdf: $(TEXFILES)
	./bin/latexmk -f -pdf  Master-TeachingStatsWithR.tex
