# Makefile for the AwEnt module of Ada Works
#
# This module is responsible for handling entities. In simple words, what it does
# is to map tagged types into tables and create and execute SQL queries in order
# to manage persistent data in your application.\

PROJECT=awent


all: libs
	@echo All done



libs:
	gnatmake -P ${PROJECT}


clean:
	gnatclean -P ${PROJECT}
