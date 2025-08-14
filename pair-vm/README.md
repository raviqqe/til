# Pair VM

## Instructions

- `cons` creates a cons with an operand in `car` and a value from the `stack` register in `cdr`.
- `set` sets a value in an address to an another address.

### Examples

#### `constant`

1. `cons`

#### `get`

1. `cons`
1. `set a ad`

#### `set`

> TODO

#### `if`

> TODO

#### `call`

> TODO

## Memory model

- Everything is a pair.
- A pair consists of `car`, `cdr`, and a tag attached to the `cdr` pointer.
- The tag's size is 16 bits.
- The VM has the following registers
  - `code`: The program counter
  - `stack`: The stack pointer
