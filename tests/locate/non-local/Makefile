BASEDIR=../..

all: a.cmt a.cmti
	$(MAKE) default

%.cmti: %.mli
	ocamlc -c -bin-annot $<

%.cmt: %.ml %.cmti
	ocamlc -c -bin-annot $<

clean::
	@rm *.cm*

include $(BASEDIR)/Makefile.cram
