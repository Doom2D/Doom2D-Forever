CC:=gcc
CFLAGS:=-std=c99 -Wall -Werror -pedantic -O2
SOURCES:=master.c
ifeq ($(OS),Windows_NT)
	BIN:=bin\\d2df_master.exe
	DEL:=del
	LDFLAGS:=-lenet -lws2_32 -lwinmm -static-libgcc
else
	BIN:=bin/d2df_master
	DEL:=rm -f
	LDFLAGS:=-lenet
	CFLAGS += -Llibs -I.
endif

all: master
	
master:
	$(CC) $(CFLAGS) -o $(BIN) $(SOURCES) $(LDFLAGS)
	
clean:
	$(DEL) *.o $(BIN)