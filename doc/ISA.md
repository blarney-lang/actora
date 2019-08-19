```
           25---------------------15----------------------+
PushIPtr    | 1000000000           | data<16>             |
            +----------------------+----------------------+
PushInt     | 1000000001           | data<16>             |
            +----------------------+----------------------+
PushAtom    | 1000000010           | data<16>             |
            +----------------------+-------------5--------+
Slide       | 1000000100           | dist<10>    | n<6>   |
            +---------------------------------------------+
Return      | 1000000101           | dist<10>    | 000001 |
            +----------------------+----------------------+
Copy        | 1000000110           | n<16>                |
            +----------------------+-----------7----------+
Call        | 1000001000           | addr<16>             |
            +----------------------+----------------------+
ICall       | 1000001001           |                      |
            +----------------------+----------------------+
Jump        | 1000001010           | addr<16>             |
            +----------------------+----------------------+
IJump       | 1000001011           |                      |
            +----------------------+-------14-------------+
Load        | 1000001101           | pop<1> |             |
            +----------------------+-----13------7------1-+
Store       | 1000001110           | k<2> | a<6> | n<6> | |
            +----------------------+----------------------+
Halt        | 1000001111           | error<16>            |
            +----------------------+----------------------+
Add         | 1000100000           |                      |
            +----------------------+----------------------+
Sub         | 1000100010           |                      |
            +----------------------+----------------------+
SetUpper    | 1000100101           | data<16>             |
            +----------------------+----------------------+
Eq          | 1000110000           |                      |
            +----------------------+----------------------+
NotEq       | 1000110010           |                      |
            +----------------------+----------------------+
Less        | 1000110100           |                      |
            +----------------------+----------------------+
GreaterEq   | 1000110110           |                      |
            +---24-------23---------14-------9------------+
BranchPop   | 0 | neg<1> | cond<9>  | pop<5> | offset<10> |
            +---+--------+----------+--------+------------+
```

## PushIPtr

Push a zero-extended instruction pointer onto the stack.

```
25          15         
+------------+----------+
| 1000000000 | data<16> |
+------------+----------+
```

## PushInt

Push a sign-extended integer onto the stack.

```
25          15         
+------------+----------+
| 1000000001 | data<16> |
+------------+----------+
```

## PushAtom

Push a zero-extended atom onto the stack.

```
25          15         
+------------+----------+
| 1000000010 | data<16> |
+------------+----------+
```

## Slide

Slide the top `n` stack elements down the stack by `dist` positions.

```
25          15          5
+------------+----------+------+
| 1000000100 | dist<10> | n<6> |
+------------+----------+------+
```

## Return

Pop address from return stack and jump to it.  Also slide top stack
element any number of places down the stack.

```
25          15     
+------------+----------+--------+
| 1000000101 | dist<10> | 000001 |
+------------+----------+--------+
```

Not the similarity to the Slide instruction, both in encoding and
semantics.

## Copy

Push the nth element from the stop of the stack onto the stack.

```
25          15      
+------------+-------+
| 1000000110 | n<16> |
+------------+-------+
```

## Call

Push program counter onto return stack and jump to given address
(zeroExtended).

```
25          15     
+------------+----------+
| 1000001000 | addr<16> |
+------------+----------+
```

## ICall

Push program counter onto return stack and jump to address on top of
stack.

```
25          15     
+------------+------------+
| 1000001001 | unused<16> |
+------------+------------+
```

## Jump

Jump to given address (zero extended).

```
25          15     
+------------+----------+
| 1000001010 | addr<16> |
+------------+----------+
```

## IJump

Jump to address on top of the stack.

```
25          15     
+------------+------------+
| 1000001011 | unused<16> |
+------------+------------+
```

## BranchPop

This relative conditional branch instruction also pops from the stack
when the branch is taken.  

```
25  24       23        14       9            
+---+--------+---------+--------+------------+
| 0 | neg<1> | cond<9> | pop<5> | offset<10> |
+---+--------+---------+--------+------------+
```

The 9-bit condition `cond` on the top stack element, which may be
negated using the `neg` bit, has the following format.

```
8     5          
+-----+----------+
| 000 |          |  Is top a function pointer?
+-----+----------+
| 001 | data<6>  |  Is top an int with value == signExtend(data)?
+-----+----------+
| 010 | data<6>  |  Is top an atom with value == zeroExtend(data)?
+-----+----------+
| 100 | 000010   |  Is top a pointer to a cons constructor?
+-----+----------+
| 101 | len<6>   |  Is top a pointer to a tuple of given length?
+-----+----------+
| 110 | arity<6> |  Is top a pointer to a closure of given arity?
+-----+----------+
```

## Load

Load data from the heap at the address specified on top of the stack,
and push it onto the stack.  Optionally, the pointer can be popped
before pushing the heap data to the stack.

```
25          15      14
+------------+--------+------------+
| 1000001101 | pop<1> | unused<16> |
+------------+--------+------------+
```


## Store

Pop the top `n` stack elements and append them to the heap.

```
25          15        13          7      1
+------------+---------+----------+------+-----------+
| 1000001110 | kind<2> | arity<6> | n<6> | unused<2> |
+------------+---------+----------+------+-----------+
```

The kind field is one of:

  * `00` - list constructor
  * `01` - tuple constructor
  * `10` - closure constructor

The `arity` is only valid when storing a closure.

## Add

Replace the top two stack elements with their sum.

```
25          15     
+------------+------------+
| 1000100000 | unused<16> |
+------------+------------+
```

## Sub

Replace the top two stack elements with their difference.

```
25          15     
+------------+------------+
| 1000100010 | unused<16> |
+------------+------------+
```

## Eq

Equality comparison>

```
25          15
+------------+------------+
| 1000110000 | unused<16> |
+------------+------------+
```

## NotEq

Disequality comparison.

```
25          15
+------------+------------+
| 1000110010 | unused<16> |
+------------+------------+
```

## Less

Integer comparison.

```
25          15
+------------+------------+
| 1000110100 | unused<16> |
+------------+------------+
```

## GreaterEq

Integer comparison.

```
25          15
+------------+------------+
| 1000110110 | unused<16> |
+------------+------------+
```

## SetUpper

Overwrite the upper 16 bits of the top stack element with the given
data.

```
25          15         
+------------+----------+
| 1000100101 | data<16> |
+------------+----------+
```

## Halt

Terminate with the given error code.

```
25          15
+------------+------------+
| 1000001111 | error<16>  |
+------------+------------+
```

## Error codes

  Error           | Code   | Meaning
  --------------- | ------ | -------
  ENone           | 0      | No error
  EStackOverflow  | 1      | Stack overflow
  EHeapOverflow   | 2      | Live heap too large
  EArith          | 3      | Bad type in arithmetic operand(s)
  ELoadAddr       | 4      | Load address is not a data pointer
  EJumpAddr       | 5      | Jump/branch/call target is not an instr pointer
  EStackIndex     | 6      | Stack index out of bounds
  EUnknown        | 7      | Unknown error
  EInstrIndex     | 8      | Instruction index out of bounds
  EUnknownInstr   | 9      | Unrecognised instruction
  EStackUnderflow | 10     | Stack underflow
  EBindFail       | 16     | Pattern mismatch in binding
  ECaseFail       | 17     | No matching case alternatives
  EEqnFail        | 18     | No matching equation
  EApplyFail      | 19     | Application of non-closure
