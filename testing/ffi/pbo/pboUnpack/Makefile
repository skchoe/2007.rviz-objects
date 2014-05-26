# Project: pboUnpack
# Makefile created by Dev-C++ 4.9.9.2

CPP  = g++
CC   = gcc
WINDRES = 
RES  = 
OBJ  = main.o Timer.o glInfo.o $(RES)
LINKOBJ  = main.o Timer.o glInfo.o $(RES)
LIBS =  -L"/usr/lib" -lglut -lGLU -lGL
INCS =  -I"/usr/include" 
CXXINCS =  
BIN  = ./pboUnpack
CXXFLAGS = $(CXXINCS) -Wall   -O2
CFLAGS = $(INCS)   -O2
RM = rm -f

.PHONY: all all-before all-after clean clean-custom

#all: all-before ./pboUnpack all-after 
all:
	$(CPP) -o pboUnpack main.cpp Timer.cpp glInfo.cpp $(CXXFLAGS) -L$(LIBS) -I$(INCS) 


clean: clean-custom
	${RM} $(OBJ) $(BIN)

$(BIN): $(OBJ)
	$(CPP) $(LINKOBJ) -o "./pboUnpack" $(LIBS)

main.o: main.cpp
	$(CPP) -c main.cpp -o main.o $(CXXFLAGS)

Timer.o: Timer.cpp
	$(CPP) -c Timer.cpp -o Timer.o $(CXXFLAGS)

glInfo.o: glInfo.cpp
	$(CPP) -c glInfo.cpp -o glInfo.o $(CXXFLAGS)
