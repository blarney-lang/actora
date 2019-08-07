#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

// Size (number of elements) of heap, stack, and return stack
#define HEAP_SIZE      64000
#define STACK_SIZE     4000
#define RET_STACK_SIZE 1000

// Instruction decoding
// ====================

#define I_PushIPtr  0b1000000000
#define I_PushInt   0b1000000001
#define I_PushAtom  0b1000000010
#define I_Slide     0b1000000100
#define I_Return    0b1000000101
#define I_Copy      0b1000000110
#define I_Call      0b1000001000
#define I_ICall     0b1000001001
#define I_Jump      0b1000001010
#define I_IJump     0b1000001011
#define I_Load      0b1000001101
#define I_Store     0b1000001110
#define I_Halt      0b1000001111
#define I_Add       0b1000100000
#define I_AddImm    0b1000100001
#define I_Sub       0b1000100010
#define I_SubImm    0b1000100011
#define I_SetUpper  0b1000100101
#define I_Eq        0b1000110000
#define I_NotEq     0b1000110010
#define I_Less      0b1000110100
#define I_LessEq    0b1000110110

// Get 10-bit opcode field from instruction
inline uint32_t getOpcode(uint32_t instr)
  { return (instr >> 16); }

// Get 16-bit data field from instruction
inline uint32_t getOperand(uint32_t instr)
  { return instr & 0xffff; }

// Get 16-bit int field from instruction
inline int32_t getIntOperand(uint32_t instr)
{
  int16_t val = (int16_t) (instr & 0xffff);
  return (int32_t) val;
}

// Get distance from Slide instruction
inline uint32_t getSlideDist(uint32_t instr)
  { return (instr >> 6) & 0x3ff; }

// Get length from Slide instruction
inline uint32_t getSlideLen(uint32_t instr)
  { return instr & 0x3f; }

// Get pop flag from Load instruction
inline bool getLoadPopFlag(uint32_t instr)
  { return ((instr >> 15) & 1) == 1; }

// Get pointer kind from Store instruction
inline uint32_t getStoreKind(uint32_t instr)
  { return (instr >> 14) & 0x3; }

// Get closure arity from Store instruction
inline uint32_t getStoreArity(uint32_t instr)
  { return (instr >> 8) & 0x3f; }

// Get object length from Store instruction
inline uint32_t getStoreLen(uint32_t instr)
  { return (instr >> 2) & 0x3f; }

// Is it a BranchPop instruction
inline bool isBranchPop(uint32_t instr)
  { return (instr >> 25) == 0; }

// Decode BranchPop instruction
inline void decodeBranchPop(uint32_t instr,
  uint32_t* neg, uint32_t* condKind, uint32_t* condArg,
    uint32_t* pop, uint32_t* offset)
{
  *offset = instr & 0x3ff; instr >>= 10;
  *pop = instr & 0x1f; instr >>= 5;
  *condArg = instr & 0x3f; instr >>= 6;
  *condKind = instr & 0x7; instr >>= 3;
  *neg = instr & 0x1;
}

// Error codes
// ===========

#define ENone             0
#define EStackOverflow    1
#define EHeapOverflow     2
#define EArith            3
#define ELoadAddr         4
#define EJumpAddr         5
#define EStackIndex       6
#define EUnknown          7
#define EInstrIndex       8
#define EUnknownInstr     9
#define EStackUnderflow   10
#define EBindFail         16
#define ECaseFail         17
#define EEqnFail          18
#define EApplyFail        19
#define MaxErrorCode      64

const char* getErrorString(uint32_t err)
{
  if (err == ENone)           return "ENone";
  if (err == EStackOverflow)  return "EStackOverflow";
  if (err == EHeapOverflow)   return "EHeapOverflow";
  if (err == EArith)          return "EArith";
  if (err == ELoadAddr)       return "ELoadAddr";
  if (err == EJumpAddr)       return "EJumpAddr";
  if (err == EStackIndex)     return "EStackIndex";
  if (err == EUnknown)        return "EUnknown";
  if (err == EInstrIndex)     return "EInstrIndex";
  if (err == EUnknownInstr)   return "EUnknownInstr";
  if (err == EStackUnderflow) return "EStackUnderflow";
  if (err == EBindFail)       return "EBindFail";
  if (err == ECaseFail)       return "ECaseFail";
  if (err == EEqnFail)        return "EEqnFail";
  if (err == EApplyFail)      return "EApplyFail";
  fprintf(stderr, "Unknown error code: %d\n", err);
  exit(EXIT_FAILURE);
}

// Bytecode loader
// ===============

struct Bytecode {
  uint32_t numAtoms;
  uint32_t numLabels;
  uint32_t numInstrs;
  char** atoms;
  char** labelNames;
  uint32_t* labelAddrs;
  uint32_t* instrs;
};

void loadBytecode(FILE* fp, Bytecode* code)
{
  // Read number of atoms, labels, and instructions
  if (fscanf(fp, "%i %i %i", &code->numAtoms,
        &code->numLabels, &code->numInstrs) != 3) {
    fprintf(stderr, "Error loading bytecode\n");
    exit(EXIT_FAILURE);
  }

  // Load atoms
  code->atoms = new char* [code->numAtoms];
  for (uint32_t i = 0; i < code->numAtoms; i++) {
    code->atoms[i] = new char [128];
    if (fscanf(fp, "%127s", code->atoms[i]) <= 0) {
      fprintf(stderr, "Error loading bytecode\n");
      exit(EXIT_FAILURE);
    }
  }

  // Load labels
  code->labelNames = new char* [code->numLabels];
  code->labelAddrs = new uint32_t [code->numLabels];
  for (uint32_t i = 0; i < code->numLabels; i++) {
    code->labelNames[i] = new char [128];
    if (fscanf(fp, "%127s %u",
          code->labelNames[i], &code->labelAddrs[i]) <= 0) {
      fprintf(stderr, "Error loading bytecode\n");
      exit(EXIT_FAILURE);
    }
  }

  // Load instructions
  code->instrs = new uint32_t [code->numInstrs];
  for (uint32_t i = 0; i < code->numInstrs; i++) {
    if (fscanf(fp, "%u", &code->instrs[i]) <= 0) {
      fprintf(stderr, "Error loading bytecode\n");
      exit(EXIT_FAILURE);
    }
  }
}

// Stack machine
// =============

// Cell kind
enum CellKind {
  FUN         = 0b000,
  INT         = 0b001,
  ATOM        = 0b010,
  PTR_CONS    = 0b100,
  PTR_TUPLE   = 0b101,
  PTR_CLOSURE = 0b110,
  GC          = 0b111
};

// Is cell kind a pointer?
bool isPointer(uint8_t kind)
  { return kind == PTR_CONS || kind == PTR_TUPLE || kind == PTR_CLOSURE; }

// Cell tag
struct CellTag {
  uint8_t kind;
  // If a pointer, the length of the object pointed-to
  uint16_t len;
  // If a pointer to a closure, the closure arity
  uint8_t arity;
};

// Heap/stack cell
struct Cell {
  CellTag tag;
  uint32_t val;
};

struct State {
  // Program counter
  uint32_t pc;
  // Heap pointer
  uint32_t hp;
  // Stack pointer
  uint32_t sp;
  // Return stack pointer
  uint32_t rp;
  // Heap
  Cell heap[HEAP_SIZE];
  // Stack
  Cell stack[STACK_SIZE];
  // Return stack
  uint32_t retStack[RET_STACK_SIZE];
  // Count number of cycles
  uint64_t cycles;
};

// Helpers
// =======

inline Cell makeInt(int32_t val)
  { Cell cell; cell.tag.kind = INT; cell.val = val; return cell; }

inline Cell makeAtom(uint32_t val)
  { Cell cell; cell.tag.kind = ATOM; cell.val = val; return cell; }

inline Cell makeFun(uint32_t val)
  { Cell cell; cell.tag.kind = FUN; cell.val = val; return cell; }

// Garbage collector
// =================

uint32_t gcCopy(Cell* cell, Cell* heap, Cell* toSpace, uint32_t front)
{
  if (isPointer(cell->tag.kind)) {
    assert(cell->val < HEAP_SIZE);
    Cell* p = &heap[cell->val];
    if (p->tag.kind == GC)
      cell->val = p->val;
    else {
      uint32_t len = cell->tag.len;
      for (uint32_t i = 0; i < len; i++) toSpace[front+i] = p[i];
      p->tag.kind = GC;
      p->val = front;
      cell->val = front;
      front += len;
    }
  }
  return front;
}

uint32_t gc(State *s)
{
  // Setup to-space
  Cell* toSpace = new Cell [HEAP_SIZE];
  uint32_t back = 0;
  uint32_t front = 0;

  // Loop over stack
  for (uint32_t i = 0; i < s->sp; i++)
    front = gcCopy(&s->stack[i], s->heap, toSpace, front);

  // Copy reachable heap to to-space
  while (back != front) {
    front = gcCopy(&toSpace[back], s->heap, toSpace, front);
    back++;
  }
  s->hp = front;

  // Copy to-space back to heap
  for (uint32_t i = 0; i < front; i++)
    s->heap[i] = toSpace[i];

  // Free to-space
  delete [] toSpace;

  // Raise error if free space is less than 10% of available
  uint32_t percent = 10;
  if ((percent - ((percent*front)/HEAP_SIZE)) < 1) return EHeapOverflow;
  return ENone;
}

// Emulator
// ========

// Run program
uint32_t run(Bytecode* code, State* s)
{
  while (1) {
    if (s->pc >= code->numInstrs) return EInstrIndex;
    uint32_t instr = code->instrs[s->pc];
    uint32_t op = getOpcode(instr);

    if (isBranchPop(instr)) {
      if (s->sp == 0) return EStackUnderflow;
      Cell top = s->stack[s->sp-1];
      uint32_t neg, condKind, condArg, pop, offset;
      decodeBranchPop(instr, &neg, &condKind, &condArg, &pop, &offset);
      bool branch = false;
      if (condKind == FUN) {
        branch = top.tag.kind == FUN;
      }
      if (condKind == ATOM) {
        // Atom
        branch = top.tag.kind == ATOM && top.val == condArg;
      }
      else if (condKind == INT) {
        // Integer
        uint32_t condVal = condArg;
        if (condVal & 0x20) condVal |= 0xfffffc00;
        branch = top.tag.kind == INT && top.val == condVal;
      }
      else if (condKind == PTR_CONS) {
        // Cons pointer
        branch = top.tag.kind == PTR_CONS;
      }
      else if (condKind == PTR_TUPLE) {
        // Tuple pointer
        branch = top.tag.kind == PTR_TUPLE && top.tag.len == condArg;
      }
      else if (condKind == PTR_CLOSURE) {
        // Closure pointer
        branch = top.tag.kind == PTR_CLOSURE && top.tag.arity == condArg;
      }
      if (neg) branch = !branch;
      if (branch) {
        if (pop > s->sp) return EStackUnderflow;
        s->sp -= pop;
        s->pc += offset;
      }
      else
        s->pc++;
      s->cycles++;
    }
    else if (op == I_PushInt) {
      if (s->sp >= STACK_SIZE) return EStackOverflow;
      s->stack[s->sp++] = makeInt(getIntOperand(instr));
      s->pc++;
      s->cycles++;
    }
    else if (op == I_PushAtom) {
      if (s->sp >= STACK_SIZE) return EStackOverflow;
      s->stack[s->sp++] = makeAtom(getOperand(instr));
      s->pc++;
      s->cycles++;
    }
    else if (op == I_PushIPtr) {
      if (s->sp >= STACK_SIZE) return EStackOverflow;
      s->stack[s->sp++] = makeFun(getOperand(instr));
      s->pc++;
      s->cycles++;
    }
    else if (op == I_SetUpper) {
      if (s->sp == 0) return EStackUnderflow;
      Cell* top = &s->stack[s->sp-1];
      top->val = (getOperand(instr) << 16) | (top->val & 0xffff);
      s->pc++;
      s->cycles++;
    }
    else if (op == I_Slide) {
      uint32_t len = getSlideLen(instr);
      uint32_t dist = getSlideDist(instr);
      if (len > s->sp) return EStackUnderflow;
      if (dist > (s->sp - len)) return EStackIndex;
      for (uint32_t i = len; i > 0; i--)
        s->stack[s->sp - dist - i] = s->stack[s->sp - i];
      s->sp -= dist;
      s->pc++;
      s->cycles += len;
    }
    else if (op == I_Return) {
      uint32_t dist = getSlideDist(instr);
      if (s->rp == 0) return EStackUnderflow;
      s->pc = s->retStack[s->rp-1];
      s->rp--;
      Cell top = s->stack[s->sp-1];
      s->sp -= dist;
      s->stack[s->sp++] = top;
      s->cycles++;
    }
    else if (op == I_Copy) {
      uint32_t offset = getOperand(instr);
      if (offset >= s->sp) return EStackIndex;
      if (s->sp >= STACK_SIZE) return EStackOverflow;
      s->stack[s->sp] = s->stack[s->sp - 1 - offset];
      s->sp++;
      s->pc++;
      s->cycles++;
    }
    else if (op == I_Call) {
      uint32_t addr = getOperand(instr);
      if (s->rp >= RET_STACK_SIZE) return EStackOverflow;
      s->retStack[s->rp++] = s->pc + 1;
      s->pc = addr;
      s->cycles++;
    }
    else if (op == I_ICall) {
      if (s->sp == 0) return EStackUnderflow;
      Cell top = s->stack[s->sp-1];
      if (top.tag.kind != FUN) return EJumpAddr;
      if (s->rp >= RET_STACK_SIZE) return EStackOverflow;
      s->retStack[s->rp++] = s->pc + 1;
      s->pc = top.val;
      s->sp--;
      s->cycles++;
    }
    else if (op == I_Jump) {
      s->pc = getOperand(instr);
      s->cycles++;
    }
    else if (op == I_IJump) {
      if (s->sp == 0) return EStackUnderflow;
      Cell top = s->stack[s->sp-1];
      if (top.tag.kind != FUN) return EJumpAddr;
      s->pc = top.val;
      s->sp--;
      s->cycles++;
    }
    else if (op == I_Load) {
      bool pop = getLoadPopFlag(instr);
      if (s->sp == 0) return EStackUnderflow;
      Cell top = s->stack[s->sp-1];
      if (! isPointer(top.tag.kind)) return ELoadAddr;
      uint32_t len = top.tag.len;
      if (pop) s->sp--;
      if (s->sp+len >= STACK_SIZE) return EStackOverflow;
      uint32_t addr = top.val;
      if (addr+len >= HEAP_SIZE) return ELoadAddr;
      for (int32_t i = len-1; i >= 0; i--)
        s->stack[s->sp++] = s->heap[addr+i];
      s->pc++;
      s->cycles += 1 + (len+1)/2;
    }
    else if (op == I_Store) {
      uint32_t kind = getStoreKind(instr);
      uint32_t arity = getStoreArity(instr);
      uint32_t len = getStoreLen(instr);
      if (len == 0 && s->sp >= STACK_SIZE) return EStackOverflow;
      if (len > s->sp) return EStackUnderflow;
      if (s->hp+len >= HEAP_SIZE) {
        uint32_t err = gc(s);
        if (err != ENone) return err;
      }
      Cell cell;
      if (kind == 0b00) cell.tag.kind = PTR_CONS;
      else if (kind == 0b01) cell.tag.kind = PTR_TUPLE;
      else if (kind == 0b10) cell.tag.kind = PTR_CLOSURE;
      else return EUnknown;
      cell.tag.len = len;
      cell.tag.arity = arity;
      cell.val = s->hp;
      for (int i = 0; i < len; i++) {
        s->heap[s->hp++] = s->stack[s->sp-1];
        s->sp--;
      }
      s->stack[s->sp++] = cell;
      s->pc++;
      s->cycles += (len+1)/2;
    }
    else if (op == I_AddImm || op == I_SubImm) {
      if (s->sp < 1) return EStackIndex;
      Cell* a = &s->stack[s->sp-1];
      if (a->tag.kind != INT) return EArith;
      if (op == I_AddImm)
        a->val = (uint32_t) ((int32_t) a->val + getIntOperand(instr));
      else
        a->val = (uint32_t) ((int32_t) a->val - getIntOperand(instr));
      s->pc++;
      s->cycles++;
    }
    else if (op == I_Add || op == I_Sub) {
      if (s->sp < 2) return EStackIndex;
      Cell* a = &s->stack[s->sp-1];
      Cell* b = &s->stack[s->sp-2];
      if (a->tag.kind != INT || b->tag.kind != INT) return EArith;
      if (op == I_Add)
        b->val = (uint32_t) ((int32_t) a->val + (int32_t) b->val);
      else
        b->val = (uint32_t) ((int32_t) a->val - (int32_t) b->val);
      s->sp--;
      s->pc++;
      s->cycles++;
    }
    else if (op == I_Eq || op == I_NotEq ||
             op == I_Less || op == I_LessEq) {
      if (s->sp < 2) return EStackIndex;
      Cell* a = &s->stack[s->sp-1];
      Cell* b = &s->stack[s->sp-2];
      if (a->tag.kind != INT || b->tag.kind != INT) return EArith;
      b->tag.kind = ATOM;
      if (op == I_Eq)
        b->val = ((int32_t) a->val == (int32_t) b->val) ? 1 : 0;
      else if (op == I_NotEq)
        b->val = ((int32_t) a->val != (int32_t) b->val) ? 1 : 0;
      else if (op == I_Less)
        b->val = ((int32_t) a->val < (int32_t) b->val) ? 1 : 0;
      else if (op == I_LessEq)
        b->val = ((int32_t) a->val <= (int32_t) b->val) ? 1 : 0;
      s->sp--;
      s->pc++;
      s->cycles++;
    }
    else if (op == I_Halt) {
      return getOperand(instr);
    }
    else {
      return EUnknownInstr;
    }
  }
}

// Render result
// =============

void render(Bytecode* code, State* s, Cell cell)
{
  if (cell.tag.kind == INT) printf("%d", cell.val);
  if (cell.tag.kind == ATOM) {
    assert(cell.val < code->numAtoms);
    printf("%s", code->atoms[cell.val]);
  }
  if (cell.tag.kind == PTR_CONS) {
    printf("[");
    render(code, s, s->heap[cell.val]);
    printf("|");
    render(code, s, s->heap[cell.val+1]);
    printf("]");
  }
  if (cell.tag.kind == PTR_TUPLE) {
    printf("{");
    for (uint32_t i = 0; i < cell.tag.len; i++) {
      render(code, s, s->heap[cell.val+i]);
      if (i < cell.tag.len-1) printf(", ");
    }
    printf("}");
  }
  if (cell.tag.kind == PTR_CLOSURE) {
    printf("CLOSURE");
  }
}

// Stack trace
// ===========

// Determine the function in which the given instruction resides
char* owner(Bytecode* code, uint32_t ip)
{
  for (int32_t i = code->numLabels - 1;  i >= 0; i--) {
    if (ip > code->labelAddrs[i])
      return code->labelNames[i];
  }
  return (char*) "";
}

// Display stack trace from current machine state
void stackTrace(Bytecode* code, State* s)
{
  printf("Stack trace:\n");
  printf("  %s\n", owner(code, s->pc));
  uint32_t rp = s->rp;
  while (rp != 0) {
    printf("  %s\n", owner(code, s->retStack[rp-1]));
    rp--;
  }
}

// Main function
// =============

int main(int argc, char** argv)
{
  if (argc != 2) {
    printf("Usage: redemu [FILE]\n");
    return 0;
  }

  FILE *fp = fopen(argv[1], "rt");
  if (fp == NULL) {
    fprintf(stderr, "Can't open file '%s'\n", argv[1]);
    return 0;
  }

  Bytecode code;
  loadBytecode(fp, &code);

  State* state = new State;
  state->pc = 0;
  state->hp = 0;
  state->sp = 0;
  state->rp = 0;
  state->cycles = 0;

  uint32_t err = run(&code, state);
  if (err != ENone) {
    fprintf(stderr, "Aborting with error %s\n", getErrorString(err));
    stackTrace(&code, state);
    exit(EXIT_FAILURE);
  }
  assert(state->sp > 0);
  render(&code, state, state->stack[0]);
  printf("\n");
  printf("Cycles: %lu\n", state->cycles);

  fclose(fp);
  return 0;
}
