# Compiler and flags
GHC = ghc
GHC_FLAGS = -Wall -o flp-fun

# Target executable
TARGET = flp-fun

# Source file
SOURCE = flp-fun.hs

# Default target
all: $(TARGET)

# Compile the Haskell source file
$(TARGET): $(SOURCE)
	$(GHC) $(GHC_FLAGS) $(SOURCE)

# Clean up compiled files
clean:
	rm -f $(TARGET) *.hi *.o

# Phony targets (not actual files)
.PHONY: all clean