all: CheatSheet.dvi
all: CheatSheet.pdf

latexflags := -interaction=nonstopmode -file-line-error-style
latex      := latex $(latexflags)
pdflatex   := pdflatex $(latexflags)

lhs2TeX := lhs2TeX --verb
dvips   := dvips

define run-while-needed
	@while true; do \
	  printf '%s\n' '$(1) "$<"'; \
	  $(1) "$<" >/dev/null; \
	  if ! egrep -q 'LaTeX Warning:.*Rerun' "$(<:.tex=.log)"; then break; fi; \
	done; \
	egrep '\.tex:|Warning|Error' "$(<:.tex=.log)" >&2 || true; \
	! egrep -q Error "$(<:.tex=.log)"
endef

%.tex: %.lhs
	$(lhs2TeX) "$<" >"$@"

%.dvi: %.tex
	$(call run-while-needed,$(latex))

%.ps: %.dvi
	$(dvips) "$<"

%.pdf: %.tex
	$(call run-while-needed,$(pdflatex))

.PHONY: clean
clean:
	$(RM) CheatSheet.tex CheatSheet.aux CheatSheet.log \
	  CheatSheet.dvi CheatSheet.pdf CheatSheet.ps
