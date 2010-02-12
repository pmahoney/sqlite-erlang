PROG='io:format("~s~n", [code:lib_dir(erl_interface)])'
OTP_DIR=$(shell erl -eval $(PROG) -s init stop -noshell)
LIB=-lsqlite3 -lerl_interface -lei
FLAGS=-I$(OTP_DIR)/include -L$(OTP_DIR)/lib
ifdef NOTEST
FLAGS+=-DRELEASE
endif
GCC=/usr/bin/gcc

all: obj
	$(GCC) -Wall erl_comm.o sqlite_port.o $(FLAGS) $(LIB) -o sqlite_port

obj: erl_comm.c sqlite_port.c
	$(GCC) -Wall -c $(FLAGS) erl_comm.c sqlite_port.c 

clean:
	- rm -f *.o sqlite_port

