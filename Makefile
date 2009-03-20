all: CheatSheet.dvi
all: CheatSheet.pdf

# To handle LaTeX rebuilds needed for refs, we use rubber.

# Rubber is able to call lhs2TeX itself (just give the .lhs file as the
# parameter), but as of this writing, it's hardcoded to call it with --poly.
# Let's run lhs2TeX by ourselves for now.

lhs2TeX := lhs2TeX --verb
rubber  := rubber -s -W all

CheatSheet.tex: CheatSheet.lhs
	$(lhs2TeX) "$<" >"$@"

CheatSheet.dvi: CheatSheet.tex
	$(rubber) "$<"

CheatSheet.ps: CheatSheet.tex
	$(rubber) --ps "$<"

CheatSheet.pdf: CheatSheet.tex
	$(rubber) --pdf "$<"

.PHONY: clean
clean:
	$(RM) CheatSheet.tex CheatSheet.aux CheatSheet.log \
	  CheatSheet.dvi CheatSheet.pdf CheatSheet.ps
