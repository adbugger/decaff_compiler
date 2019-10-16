#ifndef __TYPES_H__
#define __TYPES_H__

#include <iostream>

enum class Type {
    VOID,
    INT,
    BOOL,
};
std::ostream& operator<<(std::ostream&, Type);

enum class AssignOp {
    ASS_EQ,
    ASS_DEC,
    ASS_INC,
};
std::ostream& operator<<(std::ostream&, AssignOp);

enum class BinaryOp {
    ADD, SUB, MUL, DIV, MOD,
    LT, GT, LE, GE,
    ISEQ, NEQ,
    AND, OR,
};
std::ostream& operator<<(std::ostream&, BinaryOp);

enum class UnaryOp {
    SUB,
    EXCL,
};
std::ostream& operator<<(std::ostream&, UnaryOp);

#endif /* __TYPES_H__ */
