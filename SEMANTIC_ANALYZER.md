# Semantic Analyzer for printf

This semantic analyzer provides compile-time validation for `printf` statements in C code.

## Features

### 1. Format String Parsing
The analyzer correctly parses printf format strings including:
- Basic format specifiers: `%d`, `%i`, `%u`, `%f`, `%s`, `%c`, `%p`, etc.
- Length modifiers: `h`, `hh`, `l`, `ll`, `L`, `z`, `j`, `t`
- Width and precision specifiers
- Flags: `+`, `-`, `#`, `0`, space
- Literal `%%` (escaped percent sign)

### 2. Argument Count Validation
The analyzer verifies that the number of arguments passed to `printf` matches the number of format specifiers in the format string.

**Example Error:**
```c
int x = 42;
printf("Value: %d, Double: %d\n", x);  // ERROR: expects 2 args, got 1
```

**Error Message:**
```
Semantic Error: printf argument count mismatch: format string "Value: %d, Double: %d" expects 2 arguments, but 1 were provided
```

### 3. Type Compatibility Checking
The analyzer performs basic type checking to ensure arguments match their format specifiers:

| Format Specifier | Compatible Types |
|-----------------|------------------|
| `%d`, `%i` | int, short, long, long long, char, signed variants |
| `%u`, `%o`, `%x`, `%X` | int, short, long, long long, char, unsigned variants |
| `%f`, `%F`, `%e`, `%E`, `%g`, `%G`, `%a`, `%A` | float, double |
| `%c` | char, int |
| `%s` | char* (pointer to char) |
| `%p` | any pointer type |

**Example:**
```c
int x = 42;
printf("Integer: %d\n", x);     // OK
printf("String: %s\n", "hi");   // OK
printf("Float: %f\n", 3.14);    // OK
```

## Implementation Details

### Architecture
The semantic analyzer is implemented in [src/logic/semantic.rs](src/logic/semantic.rs) and:

1. **Traverses the AST** recursively to find all `Printf` nodes
2. **Parses format strings** to extract format specifiers
3. **Infers expression types** from the AST nodes
4. **Validates compatibility** between format specifiers and argument types

### Error Types
All semantic errors are defined in [src/error.rs](src/error.rs):

- `PrintfArgumentMismatch` - Wrong number of arguments
- `PrintfTypeMismatch` - Argument type doesn't match format specifier
- `InvalidFormatSpecifier` - Unknown or invalid format specifier
- `IncompleteFormatSpecifier` - Format string ends prematurely

### Integration
The semantic analyzer runs after parsing, before code generation:

```
Source Code → Lexer → Parser → Semantic Analyzer → [Code Generation]
```

## Usage

The semantic analyzer is automatically invoked when running the compiler:

```bash
cargo run
```

If semantic errors are found, they will be reported and compilation will stop.

## Testing

### Valid printf Examples
```c
int main() {
    int x = 42;
    float y = 3.14;
    char* name = "Alice";
    
    printf("Integer: %d\n", x);
    printf("Float: %f\n", y);
    printf("String: %s, Number: %d\n", name, x);
    
    return 0;
}
```

### Invalid printf Examples
See [input/test_error.c](input/test_error.c) for examples of code that triggers semantic errors.

## Limitations

1. **Type inference is simplified** - Only works for literals, binary/unary operations, and simple expressions
2. **No variable tracking** - Cannot track variable types across assignments
3. **No cross-function analysis** - Cannot validate printf in called functions
4. **Conservative type checking** - When type cannot be inferred, validation is skipped to avoid false positives

## Future Enhancements

Potential improvements:
- Symbol table for variable type tracking
- Support for more complex expressions
- Validation of other standard library functions (scanf, fprintf, etc.)
- Warning for potentially problematic conversions
- Support for custom format specifiers
