# Pair VM

## Instructions

> TODO

- `set`

### Examples

#### `constant`

1. "stack"
1. `cons`
1. `set-car`

#### `get`

1. "stack"
1. `cons`
1. `set-car` an operand

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
- The VM has a single `root` register.
