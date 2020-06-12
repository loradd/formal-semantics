# gofer-labs
Formal Semantics Labs in Gofer

## Prerequisites
- Install [Gofer](https://github.com/rusimody/gofer)
- Update path: `$ export PATH=/path/to/gofer/executable:$PATH`

## Execution
- Start interpreter: `$ gofer`
- Enable Haskell-like syntax: `? :s -S`
- Load source file: `? :l while.gs`

### Task 1 - Repeat - Natural Semantics
- `task1` implements the factorial function using the `repeat`-construct
- `s_task1 "y"` retrieves the value for variable `y` in state `s_task1` 

### Task 2 - Repeat - Structural Operational Semantics
- `s_task2 "y"` retrieves the value of variable `y` in state `s_task2`

### Task 3 - Repeat - Denotational Semantics
- `s_task3 "y"` retrieves the value of variable `y` in state `s_task3`
