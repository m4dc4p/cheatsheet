all: CheatSheet.dvi
all: CheatSheet.pdf

# The output of lhs2TeX
cleansuffix += .tex
# Various temporary files from LaTeX
cleansuffix += .aux .log .out .ptb
# The final output
cleansuffix += .dvi .ps .pdf

latexflags := -interaction=nonstopmode -file-line-error-style
latex      := latex $(latexflags)
pdflatex   := pdflatex $(latexflags)

lhs2TeX := lhs2TeX --verb
dvips   := dvips

define run-while-needed
	@while true; do \
	  printf '%s\n' '$(1) "$<"'; \
	  $(1) "$<" >/dev/null; \
	  if ! egrep -q 'Warning:.*Rerun' "$(<:.tex=.log)"; then break; fi; \
	done; \
	egrep '\.tex:|Warning|Error|XXX|TODO|FIXME' "$(<:.tex=.log)" >&2 || true; \
	! egrep -q Error "$(<:.tex=.log)"
endef

.NOTPARALLEL:

%.tex: %.lhs
	$(lhs2TeX) "$<" >"$@"

%.dvi: %.tex
	$(call run-while-needed,$(latex))

%.ps: %.dvi
	$(dvips) "$<"

%.pdf: %.tex
	$(call run-while-needed,$(pdflatex))

.SECONDARY: CheatSheet.tex
.PHONY: clean
clean:
	$(RM) $(addprefix CheatSheet,$(cleansuffix))
