#ifndef BYTECODE_H
#define BYTECODE_H

#include "buffer.h"
#include <stdbool.h>
#include <stddef.h>

int  parse_bytecode(const char *input, unsigned char *out);
int execute_bytecode_binary(const unsigned char *bytecode, int sc_len,
                                Buffer *outputBuffer, bool capture);
int  execute_bytecode(const char *input, Buffer *outputBuffer, bool capture);
void eval_bytecode(BufferManager *bm);
int  compile_c_to_bytecode(const char *source_code, Buffer *outputBuffer, unsigned char *bytecode_out, int max_bytecode_len);
void compile_and_execute_bytecode(BufferManager *bm);
int  extract_bytecode_from_buffer(Buffer *buffer, unsigned char *bytecode_out, int max_bytecode_len);
void init_bytecode_guile_bindings(void);
int compile_and_execute_region(BufferManager *bm);

#endif /* BYTECODE_H */
