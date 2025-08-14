# Pair VM

## Instructions

- `cons` creates a cons with an operand in `car` and a value from the `stack` register in `cdr`.
- `set` sets an operand or a value in an address to an another address.

## Memory model

- Everything is a pair.
- A pair consists of `car`, `cdr`, and a tag attached to the `cdr` pointer.
- The tag's size is 16 bits.
- The VM has the following registers
  - `code`: The program counter
  - `stack`: The stack pointer

## Examples

### RVM

#### `constant`

1. `cons`

#### `get`

1. `cons`
1. `set ad... a`

#### `set`

1. `set a y...`

#### `if`

> TODO

#### `call`

> TODO
