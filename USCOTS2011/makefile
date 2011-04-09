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
	\rm Chap-*.tex
	\rm Chap-*.aux
	\rm Master-*aux
	\rm Master-*tex
	\rm Master-*blg
	\rm Master-*ind

%.tex: %.Rnw
	bin/sweave.sh -nc -c=cache $<

Master-TeachingStatsWithR.pdf: $(TEXFILES)
	./bin/latexmk -f -pdf  Master-TeachingStatsWithR.tex
