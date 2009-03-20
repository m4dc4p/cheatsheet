all: CheatSheet.dvi
all: CheatSheet.pdf

TEXFLAGS += -interaction=nonstopmode
TEXFLAGS += -file-line-error-style

CheatSheet.tex: CheatSheet.lhs
	lhs2TeX --verb $< > $@

CheatSheet.dvi: CheatSheet.tex
	latex $(TEXFLAGS) $<

CheatSheet.ps: CheatSheet.dvi
	dvips -o $@ $<

CheatSheet.pdf: CheatSheet.tex
	pdflatex $(TEXFLAGS) $<

clean:
	-rm -f CheatSheet.tex CheatSheet.aux CheatSheet.log
	-rm -f CheatSheet.dvi CheatSheet.pdf CheatSheet.ps
