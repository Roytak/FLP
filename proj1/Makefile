GHC = ghc
GHC_FLAGS = -Wall -o flp-fun

SOURCE = flp-fun.hs
TARGET = flp-fun

MERLIN_LIBNCURSESW_PATH = /lib/x86_64-linux-gnu/libncursesw.so.6.4
MERLIN_LIBS = ./merlin_libs

# default target
all: $(TARGET)

$(TARGET): $(SOURCE)
	$(GHC) $(GHC_FLAGS) $(SOURCE)

# merlin target
merlin:
	mkdir -p $(MERLIN_LIBS)
	ln -s $(MERLIN_LIBNCURSESW_PATH) $(MERLIN_LIBS)/libncursesw.so.5
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(MERLIN_LIBS) $(GHC) $(GHC_FLAGS) $(SOURCE)
	rm -rf $(MERLIN_LIBS)

clean:
	rm -f $(TARGET) *.hi *.o
	rm -rf $(MERLIN_LIBS)

.PHONY: all clean merlin
