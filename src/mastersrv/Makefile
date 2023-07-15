CC := gcc
CFLAGS := -std=gnu11 -Wall -Werror -Wno-unused-function -pedantic -O2
SOURCES := master.c
ifeq ($(OS),Windows_NT)
	BIN := bin\\d2df_master.exe
	DEL := del
	LDFLAGS := -lenet -lws2_32 -lwinmm -static-libgcc -static
else
	BIN := bin/d2df_master
	DEL := rm -f
	LDFLAGS := -lenet
	CFLAGS += -Llibs -I. -DENABLE_PIPE
endif

all: master

master:
	$(CC) $(CFLAGS) -o $(BIN) $(SOURCES) $(LDFLAGS)

clean:
	$(DEL) *.o $(BIN)
