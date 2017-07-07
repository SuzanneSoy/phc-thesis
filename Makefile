.PHONY: all
all: doc/pdf/phc-thesis.pdf

doc/pdf/phc-thesis.pdf: scribblings/*.scrbl scribblings/*.rkt
	raco setup --doc-pdf doc/pdf/ --pkgs phc-thesis > /dev/null 2>&1
	gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dFastWebView=true -sOutputFile=doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
	mv doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
