CXX = clang++
# We add -ansi for sane regular expression parsing
CXXFLAGS = -ansi --std=c++14
LDFLAGS = -lfl

LLVM_CXXFLAGS = `llvm-config --cxxflags --cppflags --ldflags --system-libs --libs core`
# We add -fexceptions because llvm-config removes exception handling needed by our parser
POST_LLVM_CXXFLAGS = -fexceptions

decaf: parser.o scanner.o driver.o main.o ast.o types.o debug_visitor.o codegen_visitor.o
	$(CXX) $(CXXFLAGS) $^ $(LLVM_CXXFLAGS) $(POST_LLVM_CXXFLAGS) -o $@
	# rm *.o

%.o: %.cc
	$(CXX) -c $(CXXFLAGS) $< $(LLVM_CXXFLAGS) $(POST_LLVM_CXXFLAGS) -o $@

parser.cc: decaf.yy
	bison -o parser.cc --defines=parser.h decaf.yy

scanner.cc: decaf.ll
	flex -o scanner.cc decaf.ll

clean:
	rm -rf parser.cc parser.h scanner.cc location.hh  position.hh stack.hh *.o decaf
