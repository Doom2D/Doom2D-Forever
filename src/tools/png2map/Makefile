CC:=gcc
CFLAGS:=-std=c11 -O2
SOURCES:=png2map.c
ifeq ($(OS),Windows_NT)
	BIN:=png2map.exe
	DEL:=del
	LDFLAGS:=-static-libgcc
else
	BIN:=png2map
	DEL:=rm -f
	LDFLAGS:=
endif

all: master

master:
	$(CC) $(CFLAGS) -o $(BIN) $(SOURCES) $(LDFLAGS)

clean:
	$(DEL) *.o $(BIN)
