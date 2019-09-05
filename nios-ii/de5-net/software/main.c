#include <stdint.h>
#include <stdbool.h>
#include <baremetal.h>
#define INLINE inline __attribute__((always_inline))

typedef uint32_t Word;
typedef uint32_t Hdr;

Word* heap;
Word* heapEnd;
Word* freePtr;
uint32_t freeLen;
Word* nextFreePtr;
uint32_t* stackBase;

extern uint32_t __e_heapBase;
#define ATOM__91_93 0
#define ATOM_b 1
#define ATOM_e 2
#define ATOM_false 3
#define ATOM_r 4
#define ATOM_true 5

#define TAG_PTR 0
#define TAG_INT 1
#define TAG_ATOM 2
#define TAG_HDR 3

#define HDR_CONS 0
#define HDR_TUPLE 1
#define HDR_APP 2
#define HDR_FREE 3

INLINE bool isPtr(Word x) { return (x&3) == TAG_PTR; }
INLINE uint32_t* getPtr(Word x) { return (uint32_t*) x; }
INLINE Word makePtr(Word* x) { return (uint32_t) x; }

INLINE bool isHdr(Hdr x) { return (x&3) == TAG_HDR; }
INLINE bool isCons(Hdr x) { return ((x>>3)&3) == HDR_CONS; }
INLINE bool isTuple(Hdr x) { return ((x>>3)&3) == HDR_TUPLE; }
INLINE bool isApp(Hdr x) { return ((x>>3)&3) == HDR_APP; }
INLINE bool isFree(Hdr x) { return ((x>>3)&3) == HDR_FREE; }
INLINE Hdr makeCons() { return (2 << 5) | (HDR_CONS<<3) | TAG_HDR; }
INLINE Hdr makeTuple(uint32_t len) { return (len<<5) | (HDR_TUPLE<<3) | TAG_HDR; }
INLINE Hdr makeApp(uint32_t arity, uint32_t len) { return (arity<<16)| (len<<5) | (HDR_APP<<3) | TAG_HDR; }
INLINE Hdr makeFree(uint32_t len) { return (len<<5) | (HDR_FREE<<3) | TAG_HDR; }
INLINE uint32_t getLen(Hdr x) { return x >> 5; }
INLINE uint32_t getAppLen(Hdr x) { return (x&0xffff) >> 5; }
INLINE uint32_t getAppArity(Hdr x) { return x >> 16; }
INLINE uint32_t isMarked(Hdr x) { return (x>>2)&1; }
INLINE Hdr markHdr(Hdr x) { return x|4; }
INLINE Hdr unmarkHdr(Hdr x) { return x & ~4; }

INLINE bool isInt(Word x) { return (x&3) == TAG_INT; }
INLINE int32_t getInt(Word x) { return (int32_t) (x >> 2); }
INLINE uint32_t getUInt(Word x) { return ((uint32_t) x) >> 2; }
INLINE Word makeInt(int32_t x) { return (((uint32_t) x) << 2) | TAG_INT; }

INLINE bool isAtom(Word x) { return (x&3) == TAG_ATOM; }
INLINE Word makeAtom(uint32_t x) { return (x << 2) | TAG_ATOM; }

void _error(char errorCode) {
  putchar(errorCode);
  putchar('\n');
  while (1);
}

uint32_t _gc(); 

// Slow path for heap allocation
Word* _allocSlow(uint32_t len);

// Fast path for heap allocation
INLINE Word* _alloc(uint32_t len) {
  if (len <= freeLen) {
    Word* p = freePtr;
    freeLen -= len;
    freePtr += len;
    return p;
  } else return _allocSlow(len);
}

Word* _allocSlow(uint32_t len) {
  if (nextFreePtr == 0) _gc();
  else {
    freePtr = nextFreePtr;
    freeLen = getLen(freePtr[0]);
    nextFreePtr = getPtr(freePtr[1]);
  }
  return _alloc(len);
}

INLINE Word add(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getInt(x) + getInt(y));
}

INLINE Word sub(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getInt(x) - getInt(y));
}

INLINE Word eq(Word x, Word y) {
if (!((isInt(x) && isInt(y)) || (isAtom(x) && isAtom(y)))) _error('P');
return makeAtom(getInt(x) == getInt(y) ? ATOM_true : ATOM_false);
}

INLINE Word neq(Word x, Word y) {
if (!((isInt(x) && isInt(y)) || (isAtom(x) && isAtom(y)))) _error('P');
return makeAtom(getInt(x) != getInt(y) ? ATOM_true : ATOM_false);
}

INLINE Word less(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeAtom(getInt(x) < getInt(y) ? ATOM_true : ATOM_false);
}

INLINE Word lessEq(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeAtom(getInt(x) <= getInt(y) ? ATOM_true : ATOM_false);
}

INLINE Word band(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getInt(x) & getInt(y));
}

INLINE Word bor(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getInt(x) | getInt(y));
}

INLINE Word bxor(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getInt(x) ^ getInt(y));
}

INLINE Word bsl(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getInt(x) << getInt(y));
}

INLINE Word bsr(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getInt(x) >> getInt(y));
}

INLINE Word bsra(Word x, Word y) {
if (!(isInt(x) && isInt(y))) _error('P');
return makeInt(getUInt(x) >> getUInt(y));
}

void _36ifFail() { _error('I'); }

const char* atoms[] = {
"[]", 
"b", 
"e", 
"false", 
"r", 
"true", 
};
// Mark all reachable heap nodes
void _mark(Word w) {
  if (isPtr(w)) {
    Word* p = getPtr(w);
    if (p >= heap && p < heapEnd && isHdr(p[0]) && !isFree(p[0])) {
      if (isMarked(*p)) return;
      *p = markHdr(*p);
      if (isApp(*p)) {
        for (uint32_t i = 0; i < getAppLen(*p); i++) _mark(p[2+i]);
      } else {
        for (uint32_t i = 0; i < getLen(*p); i++) _mark(p[1+i]);
      }
    }
  }
}

// Reclaim space and unmark all nodes
void _sweep() {
  Word first[2];
  // Pointer to header of current heap node
  Word* hdr = first;
  // Currently in free-space region?
  bool inFreeRegion = false;
  // Length of current free-space region
  uint32_t len = 0;
  // Iterate over heap
  Word* p = heap;
  while (p < heapEnd) {
    if (isHdr(p[0]) && isMarked(p[0])) {
      uint32_t n = isApp(p[0]) ? 2+getAppLen(p[0]) : 1+getLen(p[0]);
      p[0] = unmarkHdr(p[0]);
      p += n;
      inFreeRegion = false;
    }
    else {
      if (inFreeRegion == false) {
        // Check that there space to start a new free region
        if ((p+1 == heapEnd) || (isHdr(p[1]) && isMarked(p[1]))) {
          p[0] = makeInt(0);
          p++;
        }
        else {
          inFreeRegion = true;
          hdr[0] = makeFree(len);
          hdr[1] = makePtr(p);
          hdr = p;
          len = 2;
          p += 2;
        }
      }
      else {
        p[0] = makeInt(0);
        len++;
        p++;
      }
    }
  }
  // Terminate the free-space list
  hdr[0] = makeFree(len);
  hdr[1] = makePtr(0);
  // Intialise heap allocator state
  freePtr = heap;
  freeLen = getLen(first[0]);
  nextFreePtr = getPtr(first[1]);
}

uint32_t _gc()
{
  perf_start(1);
  regs_t regs;
  getRegs(regs);
  
  // Mark from registers
  for (uint32_t i = 0; i < NUM_REGS; i++) _mark((Word) regs[i]);

  // Mark from stack
  register uint32_t* sp asm ("sp");
  Word* s = sp;
  while (s <= stackBase) {
    _mark(*s);
    s++;
  }

  // Sweep phase
  _sweep();
perf_stop(1);
  return 0;
}

Word redblack_58balance(Word w1, Word w2, Word w3, Word w4);
Word redblack_58benchmark(Word w1, Word w2);
Word redblack_58build(Word w1);
Word redblack_58buildAndCheck(Word w1);
Word redblack_58check(Word w1, Word w2);
Word redblack_58ins(Word w1, Word w2);
Word redblack_58insert(Word w1, Word w2);
Word redblack_58makeBlack(Word w1);
Word redblack_58member(Word w1, Word w2);
Word redblack_58start();

Word redblack_58balance(Word x_0, Word x_1, Word x_2, Word x_3) {
{
if (x_0 != makeAtom(ATOM_b)) goto x_4;
if (!isPtr(x_1)) goto x_4;
Word x_5,x_6,x_7,x_8;
{
  Word* ptr = getPtr(x_1);
  if (!isTuple(*ptr)) goto x_4;
  if (getLen(*ptr) != 4)goto x_4;
  x_5 = ptr[1]; x_6 = ptr[2]; x_7 = ptr[3]; x_8 = ptr[4]; 
}
if (x_5 != makeAtom(ATOM_r)) goto x_4;
if (!isPtr(x_6)) goto x_4;
Word x_9,x_10,x_11,x_12;
{
  Word* ptr = getPtr(x_6);
  if (!isTuple(*ptr)) goto x_4;
  if (getLen(*ptr) != 4)goto x_4;
  x_9 = ptr[1]; x_10 = ptr[2]; x_11 = ptr[3]; x_12 = ptr[4]; 
}
if (x_9 != makeAtom(ATOM_r)) goto x_4;
Word A = x_10;
Word X = x_11;
Word B = x_12;
Word Y = x_7;
Word C = x_8;
Word Z = x_2;
Word D = x_3;
Word x_13;
{
  Word* ptr = _alloc(5);
  x_13 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = A;
  ptr[3] = X;
  ptr[4] = B;
}
Word x_14;
{
  Word* ptr = _alloc(5);
  x_14 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = C;
  ptr[3] = Z;
  ptr[4] = D;
}
Word x_15;
{
  Word* ptr = _alloc(5);
  x_15 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_r);
  ptr[2] = x_13;
  ptr[3] = Y;
  ptr[4] = x_14;
}
return x_15;
}
x_4: ;
{
if (x_0 != makeAtom(ATOM_b)) goto x_16;
if (!isPtr(x_1)) goto x_16;
Word x_17,x_18,x_19,x_20;
{
  Word* ptr = getPtr(x_1);
  if (!isTuple(*ptr)) goto x_16;
  if (getLen(*ptr) != 4)goto x_16;
  x_17 = ptr[1]; x_18 = ptr[2]; x_19 = ptr[3]; x_20 = ptr[4]; 
}
if (x_17 != makeAtom(ATOM_r)) goto x_16;
Word A = x_18;
Word X = x_19;
if (!isPtr(x_20)) goto x_16;
Word x_21,x_22,x_23,x_24;
{
  Word* ptr = getPtr(x_20);
  if (!isTuple(*ptr)) goto x_16;
  if (getLen(*ptr) != 4)goto x_16;
  x_21 = ptr[1]; x_22 = ptr[2]; x_23 = ptr[3]; x_24 = ptr[4]; 
}
if (x_21 != makeAtom(ATOM_r)) goto x_16;
Word B = x_22;
Word Y = x_23;
Word C = x_24;
Word Z = x_2;
Word D = x_3;
Word x_25;
{
  Word* ptr = _alloc(5);
  x_25 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = A;
  ptr[3] = X;
  ptr[4] = B;
}
Word x_26;
{
  Word* ptr = _alloc(5);
  x_26 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = C;
  ptr[3] = Z;
  ptr[4] = D;
}
Word x_27;
{
  Word* ptr = _alloc(5);
  x_27 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_r);
  ptr[2] = x_25;
  ptr[3] = Y;
  ptr[4] = x_26;
}
return x_27;
}
x_16: ;
{
if (x_0 != makeAtom(ATOM_b)) goto x_28;
Word A = x_1;
Word X = x_2;
if (!isPtr(x_3)) goto x_28;
Word x_29,x_30,x_31,x_32;
{
  Word* ptr = getPtr(x_3);
  if (!isTuple(*ptr)) goto x_28;
  if (getLen(*ptr) != 4)goto x_28;
  x_29 = ptr[1]; x_30 = ptr[2]; x_31 = ptr[3]; x_32 = ptr[4]; 
}
if (x_29 != makeAtom(ATOM_r)) goto x_28;
if (!isPtr(x_30)) goto x_28;
Word x_33,x_34,x_35,x_36;
{
  Word* ptr = getPtr(x_30);
  if (!isTuple(*ptr)) goto x_28;
  if (getLen(*ptr) != 4)goto x_28;
  x_33 = ptr[1]; x_34 = ptr[2]; x_35 = ptr[3]; x_36 = ptr[4]; 
}
if (x_33 != makeAtom(ATOM_r)) goto x_28;
Word B = x_34;
Word Y = x_35;
Word C = x_36;
Word Z = x_31;
Word D = x_32;
Word x_37;
{
  Word* ptr = _alloc(5);
  x_37 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = A;
  ptr[3] = X;
  ptr[4] = B;
}
Word x_38;
{
  Word* ptr = _alloc(5);
  x_38 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = C;
  ptr[3] = Z;
  ptr[4] = D;
}
Word x_39;
{
  Word* ptr = _alloc(5);
  x_39 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_r);
  ptr[2] = x_37;
  ptr[3] = Y;
  ptr[4] = x_38;
}
return x_39;
}
x_28: ;
{
if (x_0 != makeAtom(ATOM_b)) goto x_40;
Word A = x_1;
Word X = x_2;
if (!isPtr(x_3)) goto x_40;
Word x_41,x_42,x_43,x_44;
{
  Word* ptr = getPtr(x_3);
  if (!isTuple(*ptr)) goto x_40;
  if (getLen(*ptr) != 4)goto x_40;
  x_41 = ptr[1]; x_42 = ptr[2]; x_43 = ptr[3]; x_44 = ptr[4]; 
}
if (x_41 != makeAtom(ATOM_r)) goto x_40;
Word B = x_42;
Word Y = x_43;
if (!isPtr(x_44)) goto x_40;
Word x_45,x_46,x_47,x_48;
{
  Word* ptr = getPtr(x_44);
  if (!isTuple(*ptr)) goto x_40;
  if (getLen(*ptr) != 4)goto x_40;
  x_45 = ptr[1]; x_46 = ptr[2]; x_47 = ptr[3]; x_48 = ptr[4]; 
}
if (x_45 != makeAtom(ATOM_r)) goto x_40;
Word C = x_46;
Word Z = x_47;
Word D = x_48;
Word x_49;
{
  Word* ptr = _alloc(5);
  x_49 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = A;
  ptr[3] = X;
  ptr[4] = B;
}
Word x_50;
{
  Word* ptr = _alloc(5);
  x_50 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = C;
  ptr[3] = Z;
  ptr[4] = D;
}
Word x_51;
{
  Word* ptr = _alloc(5);
  x_51 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_r);
  ptr[2] = x_49;
  ptr[3] = Y;
  ptr[4] = x_50;
}
return x_51;
}
x_40: ;
{
Word Col = x_0;
Word A = x_1;
Word X = x_2;
Word B = x_3;
Word x_53;
{
  Word* ptr = _alloc(5);
  x_53 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = Col;
  ptr[2] = A;
  ptr[3] = X;
  ptr[4] = B;
}
return x_53;
}
x_52: ;
_error('E');
}
Word redblack_58benchmark(Word x_54, Word x_55) {
{
if (x_54 != makeInt(0)) goto x_56;
Word N = x_55;
return makeAtom(ATOM_true);
}
x_56: ;
{
Word M = x_54;
Word N = x_55;
Word x_58 = redblack_58buildAndCheck(N);
Word x_61;
if (x_58 == makeAtom(ATOM_true)) {
Word x_59 = sub(M, makeInt(1));
Word x_60 = redblack_58benchmark(x_59,N);
x_61 = x_60;
} else {
x_61 = makeAtom(ATOM_false);
}
return x_61;
}
x_57: ;
_error('E');
}
Word redblack_58build(Word x_62) {
{
if (x_62 != makeInt(0)) goto x_63;
return makeAtom(ATOM_e);
}
x_63: ;
{
Word N = x_62;
Word x_65 = sub(N, makeInt(1));
Word x_66 = redblack_58build(x_65);
Word x_67 = redblack_58insert(N,x_66);
return x_67;
}
x_64: ;
_error('E');
}
Word redblack_58buildAndCheck(Word x_68) {
{
Word N = x_68;
Word x_70 = redblack_58build(N);
Word x_71 = redblack_58check(N,x_70);
return x_71;
}
x_69: ;
_error('E');
}
Word redblack_58check(Word x_72, Word x_73) {
{
if (x_72 != makeInt(0)) goto x_74;
Word T = x_73;
return makeAtom(ATOM_true);
}
x_74: ;
{
Word N = x_72;
Word T = x_73;
Word x_76 = redblack_58member(N,T);
Word x_79;
if (x_76 == makeAtom(ATOM_true)) {
Word x_77 = sub(N, makeInt(1));
Word x_78 = redblack_58check(x_77,T);
x_79 = x_78;
} else {
x_79 = makeAtom(ATOM_false);
}
return x_79;
}
x_75: ;
_error('E');
}
Word redblack_58ins(Word x_80, Word x_81) {
{
Word X = x_80;
if (x_81 != makeAtom(ATOM_e)) goto x_82;
Word x_83;
{
  Word* ptr = _alloc(5);
  x_83 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_r);
  ptr[2] = makeAtom(ATOM_e);
  ptr[3] = X;
  ptr[4] = makeAtom(ATOM_e);
}
return x_83;
}
x_82: ;
{
Word X = x_80;
if (!isPtr(x_81)) goto x_84;
Word x_85,x_86,x_87,x_88;
{
  Word* ptr = getPtr(x_81);
  if (!isTuple(*ptr)) goto x_84;
  if (getLen(*ptr) != 4)goto x_84;
  x_85 = ptr[1]; x_86 = ptr[2]; x_87 = ptr[3]; x_88 = ptr[4]; 
}
Word Col = x_85;
Word A = x_86;
Word Y = x_87;
Word B = x_88;
Word x_89 = less(X, Y);
Word x_97;
if (x_89 == makeAtom(ATOM_true)) {
Word x_90 = redblack_58ins(X,A);
Word x_91 = redblack_58balance(Col,x_90,Y,B);
x_97 = x_91;
} else {
Word x_92 = eq(X, Y);
Word x_96;
if (x_92 == makeAtom(ATOM_true)) {
Word x_93;
{
  Word* ptr = _alloc(5);
  x_93 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = Col;
  ptr[2] = A;
  ptr[3] = Y;
  ptr[4] = B;
}
x_96 = x_93;
} else {
Word x_94 = redblack_58ins(X,B);
Word x_95 = redblack_58balance(Col,A,Y,x_94);
x_96 = x_95;
}
x_97 = x_96;
}
return x_97;
}
x_84: ;
_error('E');
}
Word redblack_58insert(Word x_98, Word x_99) {
{
Word X = x_98;
Word S = x_99;
Word x_101 = redblack_58ins(X,S);
Word x_102 = redblack_58makeBlack(x_101);
return x_102;
}
x_100: ;
_error('E');
}
Word redblack_58makeBlack(Word x_103) {
{
if (!isPtr(x_103)) goto x_104;
Word x_105,x_106,x_107,x_108;
{
  Word* ptr = getPtr(x_103);
  if (!isTuple(*ptr)) goto x_104;
  if (getLen(*ptr) != 4)goto x_104;
  x_105 = ptr[1]; x_106 = ptr[2]; x_107 = ptr[3]; x_108 = ptr[4]; 
}
Word Col = x_105;
Word A = x_106;
Word Y = x_107;
Word B = x_108;
Word x_109;
{
  Word* ptr = _alloc(5);
  x_109 = makePtr(ptr);
  ptr[0] = makeTuple(4);
  ptr[1] = makeAtom(ATOM_b);
  ptr[2] = A;
  ptr[3] = Y;
  ptr[4] = B;
}
return x_109;
}
x_104: ;
_error('E');
}
Word redblack_58member(Word x_110, Word x_111) {
{
Word X = x_110;
if (x_111 != makeAtom(ATOM_e)) goto x_112;
return makeAtom(ATOM_false);
}
x_112: ;
{
Word X = x_110;
if (!isPtr(x_111)) goto x_113;
Word x_114,x_115,x_116,x_117;
{
  Word* ptr = getPtr(x_111);
  if (!isTuple(*ptr)) goto x_113;
  if (getLen(*ptr) != 4)goto x_113;
  x_114 = ptr[1]; x_115 = ptr[2]; x_116 = ptr[3]; x_117 = ptr[4]; 
}
Word Col = x_114;
Word A = x_115;
Word Y = x_116;
Word B = x_117;
Word x_118 = less(X, Y);
Word x_123;
if (x_118 == makeAtom(ATOM_true)) {
Word x_119 = redblack_58member(X,A);
x_123 = x_119;
} else {
Word x_120 = eq(X, Y);
Word x_122;
if (x_120 == makeAtom(ATOM_true)) {
x_122 = makeAtom(ATOM_true);
} else {
Word x_121 = redblack_58member(X,B);
x_122 = x_121;
}
x_123 = x_122;
}
return x_123;
}
x_113: ;
_error('E');
}
Word redblack_58start() {
{
Word x_125 = redblack_58benchmark(makeInt(2000),makeInt(500));
return x_125;
}
x_124: ;
_error('E');
}

void _render(Word w) {
  if (isInt(w)) printf("0x%x", getInt(w));
  if (isAtom(w)) printf("%s", atoms[getInt(w)]);
  if (isPtr(w)) {
    Word* app = getPtr(w);
    if (isCons(app[0])) {
      printf("[");
      _render(app[1]);
      printf("|");
      _render(app[2]);
      printf("]");
    }
    if (isTuple(app[0])) {
      uint32_t n = getLen(app[0]);
      printf("{");
      for (uint32_t i = 0; i < n; i++) {
        _render(app[i+1]);
        if (i < n-1) printf(", ");
      }
      printf("}");
    }
    if (isApp(app[0])) printf("APP");
  }
}

int main() {
  register uint32_t* sp asm ("sp");
  stackBase = sp;
  heap = (Word*) &__e_heapBase;
  heapEnd = heap + (HEAP_SIZE/4);
  freePtr = heap;
  freeLen = (HEAP_SIZE/4);
  nextFreePtr = 0;
perf_reset(); perf_start(0);
  Word result = redblack_58start();
perf_stop(0);
printf("Cycles: 0x%x%x\n", perf_get_hi(0), perf_get_lo(0));
printf("GC cycles: 0x%x%x\n\n", perf_get_hi(1), perf_get_lo(1));
  _render(result);
  printf("\n");
  return 0;
}
