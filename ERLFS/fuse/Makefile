CC=gcc
CFLAGS=-c -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT
LDFLAGS=-L/usr/lib -lfuse -lerl_interface -lei
SOURCES=fuse_erlfs.c
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=erlfs


all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rf *.o erlfs
