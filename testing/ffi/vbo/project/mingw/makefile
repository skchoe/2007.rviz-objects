NAME = vbo-mingw
BINARY = ../../$(NAME).exe
SOURCE = ../../source
BUILD = ../../build/mingw
EXTERNAL_INCLUDE = -I../../external/include
EXTERNAL_LIBRARY = -L../../external/library/mingw -lmingw32 -mwindows -lSDLmain -lSDL -lopengl32 -lglu32 -lglew32 -lcg -lcgGL

OPTION = -O2 -Wall #-pedantic

OBJECT = \
	$(BUILD)/sample.o\
	$(BUILD)/vbo01.o\
	$(BUILD)/vbo02.o\
	$(BUILD)/vbo03.o\
	$(BUILD)/vbo04.o\
	$(BUILD)/vbo05.o\
	$(BUILD)/vbo06.o\
	$(BUILD)/vbo07.o

all: $(BINARY) clean

clean:
	rm $(BUILD)/*

$(BINARY): $(OBJECT)
	g++ -s -o $(BINARY) $^ $(EXTERNAL_LIBRARY)

$(BUILD)/%.o: $(SOURCE)/%.cpp
	g++ $(EXTERNAL_INCLUDE) -o $@ -c $< $(OPTION)

