# Makefile for the AwEnt module of Ada Works
#
# This module is responsible for handling entities. In simple words, what it does
# is to map tagged types into tables and create and execute SQL queries in order
# to manage persistent data in your application.\

PROJECT=awent-samples


all: exec
	@echo All done



exec:
	gnatmake -j5 -P ${PROJECT}



clean:
	gnatclean -P ${PROJECT}


run:
	BOOK_STORE_CONFIG_PATH=${PWD}/data ./bin/book_store
