.PHONY: all
all: 
	raco setup --doc-pdf doc/pdf/ --pkgs phc-thesis
	gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dFastWebView=true -sOutputFile=doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
	mv doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
