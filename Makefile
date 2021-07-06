.PHONY: all
all: doc/pdf/phc-thesis.pdf

doc/pdf/phc-thesis.pdf: from-dissertation-tobin-hochstadt/*.scrbl scribblings/*.scrbl scribblings/*.rkt
	sed -i -e 's|PLACEHOLDER|$$(which pdflatex)|' pdflatex-wrapper/pdflatex
	PATH="$$PWD/pdflatex-wrapper:$$PATH" raco setup --doc-pdf doc/pdf/ --pkgs phc-thesis > pdf.log 2>&1 || (cat pdf.log && echo "=== plain ===" && cat /tmp/ltx/*.tex && echo "=== base64 ===" && base64 /tmp/ltx/*.tex && echo "=== end ===" && exit 1)
	gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dFastWebView=true -sOutputFile=doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
	mv doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
