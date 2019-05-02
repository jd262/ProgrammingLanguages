#ifndef clox_chunk_h
#define clox_chunk_h

// A dynamic byte array together with a dynamic array of constants

#include "common.h"
#include "value.h"

// opcodes understood by the VM

typedef enum {
    OP_ADD,
    OP_NEGATE,
    OP_CONSTANT,
    OP_RETURN,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_LONG_CONSTANT,
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    int* lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);   // writes a byte into the chunk - store source line #
int addConstant(Chunk* chunk, Value value);              // adds a constant to the constant table and returns index
//int subtractConstant(Chunk* chunk, Value value);

#endif