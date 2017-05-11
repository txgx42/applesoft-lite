AFLAGS  = 
LFLAGS  = -C replica1.cfg
BINFILE = applesoft-lite.bin
OBJS    = applesoft-lite.o io.o cffa1.o wozmon.o

$(BINFILE): $(OBJS)
	ld65 $(LFLAGS) $(OBJS) -o $(BINFILE)

applesoft-lite.o: applesoft-lite.s
	ca65 $(AFLAGS) $<

wozmon.o: wozmon.s
	ca65 $(AFLAGS) $<

cffa1.o: cffa1.s
	ca65 $(AFLAGS) $<

io.o: io.s
	ca65 $(AFLAGS) $<

clean:
	rm $(OBJS) $(BINFILE)

