# Makefile for the AwEnt module of Ada Works
#
# This module is responsible for handling entities. In simple words, what it does
# is to map tagged types into tables and create and execute SQL queries in order
# to manage persistent data in your application.\



ifndef ($(PREFIX))
	PREFIX=/usr/local
endif
ifndef ($(INCLUDE_PREFIX))
	INCLUDE_PREFIX=$(PREFIX)/include/kowent
endif
ifndef ($(LIB_PREFIX))
	LIB_PREFIX=$(PREFIX)/lib
endif
ifndef ($(GPR_PREFIX)) 
	GPR_PREFIX=$(LIB_PREFIX)/gnat 
endif



PROJECT=kowent



all: libs
	@echo All done



libs:
	gnatmake -P ${PROJECT}


clean:
	gnatclean -P ${PROJECT}


gprfile:
	@echo "Preparing GPR file.."
	@echo version:=\"$(VERSION)\" > gpr/kowent.def
	@echo prefix:=\"$(PREFIX)\" >> gpr/kowent.def
	@echo lib_prefix:=\"$(LIB_PREFIX)\" >> gpr/kowent.def
	@echo include_prefix:=\"$(INCLUDE_PREFIX)\" >> gpr/kowent.def
	@gnatprep gpr/kowent.gpr.in gpr/kowent.gpr gpr/kowent.def

gprclean:
	@rm -f gpr/*gpr
	@rm -f gpr/*.def


install: gprfile
	@echo "Installing files"
	install -d $(INCLUDE_PREFIX)
	install -d $(LIB_PREFIX)
	install -d $(GPR_PREFIX)
	install src/* -t $(INCLUDE_PREFIX)
	install lib/* -t $(LIB_PREFIX)
	install gpr/*.gpr -t $(GPR_PREFIX)
	make gprclean
