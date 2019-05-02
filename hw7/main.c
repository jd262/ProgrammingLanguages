//Jacob Darabaris
//HW7
#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"
#include "stdio.h"

int main(int argc, const char* argv[]) {
    initVM();
    Chunk chunk;
    initChunk(&chunk);


    writeChunk(&chunk, OP_LONG_CONSTANT, 123);
    writeChunk(&chunk, addConstant(&chunk, 5.0), 123);

    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, addConstant(&chunk, 7.5), 123);
    
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, addConstant(&chunk, 3.1415), 123);

    writeChunk(&chunk, OP_NEGATE, 123);
    writeChunk(&chunk, OP_ADD, 123);
    writeChunk(&chunk, OP_ADD, 123);  
    
    writeChunk(&chunk, OP_RETURN, 123);


    disassembleChunk(&chunk, "test chunk");
    printf("== Running ==\n");
    interpret(&chunk); 

    freeVM();
    freeChunk(&chunk);

    return 0;
}