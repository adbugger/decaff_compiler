#include "types.h"

#include <iostream>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcovered-switch-default"

std::ostream& operator<<(std::ostream& os, Type t) {
    switch(t){
        case Type::VOID : os << "void"; break;
        case Type::INT  : os << "int";  break;
        case Type::BOOL : os << "bool"; break;
        default         : os.setstate(std::ios_base::failbit);
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, AssignOp op) {
    switch(op){
        case AssignOp::ASS_EQ   : os << "=";    break;
        case AssignOp::ASS_DEC  : os << "+=";   break;
        case AssignOp::ASS_INC  : os << "-=";   break;
        default                 : os.setstate(std::ios_base::failbit);
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, BinaryOp op) {
    switch(op){
        case BinaryOp::ADD  : os << "+";    break;
        case BinaryOp::SUB  : os << "-";    break;
        case BinaryOp::MUL  : os << "*";    break;
        case BinaryOp::DIV  : os << "/";    break;
        case BinaryOp::MOD  : os << "%";    break;

        case BinaryOp::LT   : os << "<";    break;
        case BinaryOp::GT   : os << ">";    break;
        case BinaryOp::LE   : os << "<=";   break;
        case BinaryOp::GE   : os << ">=";   break;

        case BinaryOp::ISEQ : os << "==";   break;
        case BinaryOp::NEQ  : os << "!=";   break;

        case BinaryOp::AND  : os << "&&";   break;
        case BinaryOp::OR   : os << "||";   break;

        default             : os.setstate(std::ios_base::failbit);
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, UnaryOp op) {
    switch(op){
        case UnaryOp::SUB   : os << "unary -";  break;
        case UnaryOp::EXCL  : os << "unary !";  break;
        default             : os.setstate(std::ios_base::failbit);
    }
    return os;
}

#pragma clang diagnostic pop
