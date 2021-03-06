#include <stdio.h>

#include "debug.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);

    for (int offset=0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

// These functions return the index of the next bytecode since
// they can have different sizes

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset+1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset+1];   // index of the constant
    printf("%-16s %4d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset+2;
}

static int longConstantInstruction(const char* name, Chunk* chunk, int offset){
    //uint32_t constant = chunk->code[offset+1];
    uint32_t constant = 0x00;
    for(int i=0; i < 3; i++) 
    {
        constant = constant | (chunk->code[offset+i+1]<< (8 * i)); // | = BITWISE OR 
    }   
    printf("%-16s %4d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset+4;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset-1]) {
        printf("   | ");
    } else {
        printf("%4d ", chunk->lines[offset]);
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        case OP_SUB:
            return simpleInstruction("OP_SUB", offset);
        case OP_MUL:
            return simpleInstruction("OP_MUL", offset);
        case OP_DIV:
            return simpleInstruction("OP_DIV", offset);
        case OP_LONG_CONSTANT:
            return longConstantInstruction("OP_LONG_CONSTANT", chunk, offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset+1;
    }
}