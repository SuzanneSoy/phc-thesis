.PHONY: all
all: doc/pdf/phc-thesis.pdf

doc/pdf/phc-thesis.pdf: scribblings/*.scrbl scribblings/*.rkt
	raco setup --doc-pdf doc/pdf/ --pkgs phc-thesis > make.log 2>&1 || true
	cat make.log | nc termbin.com 9999
	gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dFastWebView=true -sOutputFile=doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
	mv doc/pdf/phc-thesis-linearized.pdf doc/pdf/phc-thesis.pdf
