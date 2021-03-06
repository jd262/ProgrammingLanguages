#include <stdio.h>

#include "common.h"
#include "debug.h"
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
}

void freeVM() {

}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

    for(;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("         ");
        for(Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(vm.chunk, (int)(vm.ip-vm.chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_ADD: {
                Value b = pop();
                Value a = pop();
                push(a+b);
                break;
            }
            case OP_NEGATE:
                push(-pop());
                break;
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
            case OP_SUB: {
                Value b = pop();
                Value a = pop();
                push(a-b);
                break;
            }
            case OP_MUL: {
                Value b = pop();
                Value a = pop();
                push(a*b);
                break;
            }
            case OP_DIV: {
                Value b = pop();
                Value a = pop();
                push(a/b);
                break;
            }
            case OP_LONG_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                //Account for 2 extra bytes
                for (int i = 0; i < 2; i++)
                {
                    READ_BYTE();
                }
                break;
            }
            
        }
    }
#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    vm.ip = vm.chunk->code;
    return run();
}