.SUFFIXES: .o .c 
CC=gcc


MAGICKWAND_SHARED_LIBADD = -Wl,-rpath,/usr/local/lib -L/usr/local/lib -Wl,-rpath,/usr/local/lib -L/usr/local/lib  
#MAGICKWAND_SHARED_LIBADD = -Wl,-rpath,/usr/local/lib -L/usr/local/lib -lMagickWand -Wl,-rpath,/usr/local/lib -L/usr/local/lib  -lMagickWand 
MAGICK_INCLUDES = 
#MAGICK_INCLUDES = -I/include/ImageMagick -I/usr/local/include/ImageMagick 
# Erlang

ERL_INCLUDE = -I/usr/lib/erlang/usr/include 
ERL_LIBS    = -L/usr/lib/erlang/usr/lib \
              -lerts
EI_INCLUDE  = -I/usr/lib/erlang/lib/erl_interface-3.6.4/include
EI_LIBS     = -L/usr/lib/erlang/lib/erl_interface-3.6.4/lib \
              -lei \
              -lerl_interface

TARGET_LIB = regex_drv.so

ALL: $(TARGET_LIB)

.c.o:
	$(CC) $(CFLAGS) $(ERL_INCLUDE) $(EI_INCLUDE) $(MAGICK_INCLUDES) -c $<


regex_drv.so: regex_drv.o regex.o \
    os_match.o os_match_compile.o os_match_execute.o os_match_free_pattern.o os_regex.o os_regex_compile.o os_regex_execute.o \
    os_regex_free_pattern.o os_regex_free_substrings.o os_regex_match.o os_regex_strbreak.o os_regex_str.o
	$(CC) -o $@ $^ $(ERL_LIBS) $(EI_LIBS) $(MAGICKWAND_SHARED_LIBADD) -fpic -shared
	cp regex_drv.so ../priv/lib 
	mv regex_drv.so ../../priv/lib 
	rm -f *.o 

clean:
	rm -f *.o *.so
