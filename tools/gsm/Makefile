.SUFFIXES: .o .c 
CC=gcc



# Erlang

ERL_INCLUDE = -I/usr/local/lib/erlang/usr/include
ERL_LIBS    = -L/usr/local/lib/erlang/usr/lib \
              -lerts 
EI_INCLUDE  = -I/usr/local/lib/erlang/lib/erl_interface-3.5.7/include
EI_LIBS     = -L/usr/local/lib/erlang/lib/erl_interface-3.5.7/lib \
              -lei \
              -lerl_interface




INCLUDE = -I/usr/local/include
LIBS = -L/usr/local/lib/

TARGET_LIB = gsmoperate.so

ALL: $(TARGET_LIB)

.c.o:
	$(CC) $(CFLAGS) $(ERL_INCLUDE) $(EI_INCLUDE) $(INCLUDE) $(LIBS)  -c $<   -std=c99


gsmoperate.so: GsmOperate.o LinuxGsmOperate.o
	$(CC) -o  $@ $^ $(ERL_LIBS) $(EI_LIBS) $(INCLUDE) $(LIBS)  -fpic -shared  -std=c99

clean:
	rm -f *.o *.so
