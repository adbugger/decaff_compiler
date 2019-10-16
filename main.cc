// $Id$

#include <iostream>
#include <fstream>
#include "driver.h"
#include "ast.h"
#include "visitors.h"

using namespace std;

int main(int argc, char* argv[]) {
    ASTContext ast; ast.pRoot = nullptr;
    decaf::Driver driver(ast);

    bool result = false;
    if(argc == 2) {
        // user has specified an input file
        string infile(argv[1]);
        result = driver.parse_file(infile);
        // if(result) std::cout << "Parsing file " << infile << " done\n";
        // else std::cerr << "Some error occurred\n";
    }
    else if (argc == 1) {
        // Parse from std input. Each line must be a valid program.
        string line;
        while (getline(cin, line) && !line.empty()) {
      	  result = driver.parse_string(line, "input");
      	  // if (result) std::cout << "Parsing over from stdin\n";
          // else std::cerr << "Some error occurred\n";
        }
    }
    else {
        std::cerr << "Extra arguments specified.\n";
        std::cerr << "Usage:\n./decaf\n\tFor parsing stdin. Note that each line "
                  << "must be a valid program.\n./decaf <inputFileName>\n\tFor "
                  << "parsing a file.\n";
    }

    if(ast.pRoot == nullptr){
        std::cerr << "AST Root not found.\n";
        return 1;
    }
    //ASTVisitor* v = new DebugVisitor();
    //ast.pRoot->accept(*v);
    //std::cout << "\n\n";

    ASTVisitor* generator = new CodegenVisitor();
    ast.pRoot->accept(*generator);

    dynamic_cast<CodegenVisitor*>(generator)->generateCode();
    return 0;
}
