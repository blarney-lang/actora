module Backend.BaremetalC where

-- Standard imports
import Data.Char
import System.Directory

-- Local imports
import Syntax
import Bytecode
import Compiler
import Monad.Fresh

-- Baremetal C generator options
data BareCGenOpts =
  BareCGenOpts {
    sourceProg   :: [Decl]
  , targetDir    :: String
  }

genBareC :: BareCGenOpts -> IO ()
genBareC opts = do
    createDirectory (targetDir opts)
    writeFile (targetDir opts ++ "/main.c") ccode
    writeFile (targetDir opts ++ "/link.ld") link
  where
    bytecode :: [Instr]
    bytecode = compile (sourceProg opts)

    ccode :: String
    ccode = unlines $ concat
      [ includes
      , defines
      , typeDecls
      , helpers
      , atomNames
      , globals
      , render
      , main
      ]
-- TODO: C stack
    link :: String
    link = unlines
      [ "OUTPUT_ARCH( \"riscv\" )"
      , "C_STACK_SIZE = 1024;"
      , "STACK_SIZE = 4096;"
      , "RET_STACK_SIZE = 1024;"
      , "SECTIONS"
      , "{"
      , "  . = 0;"
      , "  .text   : { *.o(.text*) }"
      , "  . = 0x10000;"
      , "  .bss    : { *.o(.bss*) }"
      , "  .rodata : { *.o(.rodata*) }"
      , "  .sdata  : { *.o(.sdata*) }"
      , "  .data   : { *.o(.data*) }"
      , "  . += C_STACK_SIZE;"
      , "  __stackBase = ALIGN(.);"
      , "  . += 4;"
      , "  __e_retBase = ALIGN(.);"
      , "  . += RET_STACK_SIZE;"
      , "  __e_stackBase = ALIGN(.);"
      , "  . += STACK_SIZE;"
      , "  __e_heapBase = ALIGN(.);"
      , "  __e_heapSize = 0x10000 - __e_heapBase;"
      , "}"
      ]

    mangle :: String -> String
    mangle (x:xs) = first x ++ concatMap rest xs
      where
        encode x = "_" ++ show (fromEnum x)
        first x = if isAlpha x then [x] else encode x
        rest x = if isAlphaNum x then [x] else encode x

    includes :: [String]
    includes =
      [ "#include <stdint.h>"
      , "#include <stdbool.h>"
      ]

    defines :: [String]
    defines =
         [ "#define INLINE inline __attribute__((always_inline))"
         , ""
         , "#define PTR_APP   1"
         , "#define PTR_CONS  3"
         , "#define PTR_TUPLE 5"
         , "#define INT       0"
         , "#define ATOM      2"
         , "#define FUN       4"
         ]
      ++ [ "#define ATOM_" ++ mangle atom ++ " " ++ show n
         | (atom, n) <- zip (atoms bytecode) [0..] ]
      where
        labels = [label | LABEL label <- bytecode]

    typeDecls :: [String]
    typeDecls =
      [ "typedef uint16_t Unsigned;"
      , "typedef int16_t Signed;"
      , "typedef struct { Unsigned tag; Unsigned val; } TaggedWord;"
      , "typedef struct { TaggedWord* sp; void* ip; } RetItem;"
      ]

    helpers :: [String]
    helpers =
      [ "INLINE Unsigned type(Unsigned t) { return t & 7; }"
      , "INLINE Unsigned ptrLen(Unsigned t) { return t >> 3; }"
      , "INLINE Unsigned funArity(Unsigned t) { return t >> 3; }"
      , "INLINE bool isPtr(Unsigned t) { return t & 1; }"
      , "INLINE Unsigned makeTag(Unsigned kind, Unsigned n)"
      , "  { return (n << 3) | kind; }"
      , "INLINE TaggedWord* toPtr(Unsigned p)"
      , "  { return (TaggedWord*) ((uintptr_t) p); }"
      , "INLINE Unsigned fromPtr(TaggedWord* p)"
      , "  { return (Unsigned) ((uintptr_t) p); }"
      ]

    globals :: [String]
    globals =
      [ "TaggedWord* sp;"
      , "RetItem* rp;"
      , "TaggedWord* heap;"
      , "Unsigned hp;"
      , "extern uint32_t __e_stackBase;"
      , "extern uint32_t __e_heapBase;"
      , "extern uint32_t __e_retBase;"
      ]

    atomNames :: [String]
    atomNames =
         [ "const char* atoms[] = {" ]
      ++ [ "\"" ++ atom ++ "\", "
         | atom <- atoms bytecode ]
      ++ [ "};" ]

    render :: [String]
    render =
      [ "void render(TaggedWord w) {"
      , "  if (type(w.tag) == INT) printf(\"0x%x\", w.val);"
      , "  if (type(w.tag) == ATOM) printf(\"%s\", atoms[w.val]);"
      , "  if (type(w.tag) == FUN) printf(\"FUN\");"
      , "  if (type(w.tag) == PTR_APP) printf(\"APP\");"
      , "  if (type(w.tag) == PTR_CONS) {"
      , "    printf(\"[\");"
      , "    render(*((TaggedWord*) w.val));"
      , "    printf(\"|\");"
      , "    render(*((TaggedWord*) w.val+1));"
      , "    printf(\"]\");"
      , "  }"
      , "  if (type(w.tag) == PTR_TUPLE) {"
      , "    Unsigned n = ptrLen(w.tag);"
      , "    printf(\"{\");"
      , "    for (Unsigned i = 0; i < n; i++) {"
      , "      render(*((TaggedWord*) w.val+i));"
      , "      if (i < n-1) printf(\", \");"
      , "    }"
      , "    printf(\"}\");"
      , "  }"
      , "}"
      ]

    main :: [String]
    main =
         [ "int main() {"
         , "  sp = (TaggedWord*) &__e_stackBase;"
         , "  rp = (RetItem*) &__e_retBase;"
         , "  heap = (TaggedWord*) &__e_heapBase;"
         , "  hp = 0;"
         , "  uint8_t flagApplyPtr;"
         , "  uint8_t flagApplyDone;"
         , "  uint8_t flagApplyOk;"
         , "  uint8_t flagApplyUnder;"
         ]
      ++ map ("  " ++) (instrs bytecode)
      ++ [ "  _icall_fail:"
         , "  _load_fail:"
         , "  _prim_fail:"
         , "  return -1;"
         , "}"
         ]
   
    atomTag :: Atom -> String
    atomTag (INT i) = "INT"
    atomTag (ATOM a) = "ATOM"
    atomTag (FUN f n) = "makeTag(FUN, " ++ show n ++ ")"

    atomVal :: Atom -> String
    atomVal (INT i) = show i
    atomVal (ATOM a) = "ATOM_" ++ mangle a
    atomVal (FUN (InstrLabel f) n) = "&&" ++ mangle f

    ptrKind :: PtrKind -> String
    ptrKind PtrApp = "PTR_APP"
    ptrKind PtrCons = "PTR_CONS"
    ptrKind PtrTuple = "PTR_TUPLE"

    instrs :: [Instr] -> [String]
    instrs is = concat $ snd (runFresh m "RET_" 0)
      where m = mapM (\i -> ("":) `fmap` instr i) is

    -- TODO
    --   * Check for stack underflow/overflow
    --   * Check for heap overflow
    instr :: Instr -> Fresh [String]
    instr (LABEL str) = return [mangle str ++ ":"]
    instr (PUSH a) =
      return
        [ "sp->val = " ++ atomVal a ++ ";"
        , "sp->tag = " ++ atomTag a ++ ";"
        , "sp++;"
        ]
    instr (PUSH_RET (InstrLabel label)) =
      return
        [ "rp->ip = &&" ++ mangle label ++ ";"
        , "rp->sp = sp;"
        , "rp++;"
        ]
    instr (CALL (InstrLabel label) n) = do
      retLabel <- fresh
      return
        [ "rp->ip = &&" ++ mangle retLabel ++ ";"
        , "rp->sp = sp - " ++ show n ++ ";"
        , "rp++;"
        , "goto " ++ mangle label ++ ";"
        , mangle retLabel ++ ":"
        ]
    instr ICALL = do
      retLabel <- fresh
      return
        [ "if (type(sp[-1].tag) != FUN) goto _icall_fail;"
        , "rp->ip = &&" ++ mangle retLabel ++ ";"
        , "rp->sp = sp - funArity(sp[-1].tag) - 1;"
        , "rp++;"
        , "sp--;"
        , "goto *((void*) ((uintptr_t) sp[0].val));"
        , mangle retLabel ++ ":"
        ]
    instr (COPY n) =
      return
        [ "*sp = sp[-" ++ show (n+1) ++ "]; sp++;" ]
    instr (JUMP (InstrLabel label)) =
      return [ "goto " ++ mangle label ++ ";" ]
    instr (SLIDE_JUMP pop n (InstrLabel label)) =
      return $
           [ "sp[-" ++ show (i+pop) ++ "] = " ++
               "sp[-" ++ show i ++ "];" | i <- reverse [1..n] ]
        ++ [ "sp -= " ++ show pop ++ ";"]
        ++ [ "goto " ++ mangle label ++ ";" ]
    instr (RETURN pop) =
      return
        [ "sp[-" ++ show pop ++ "] = sp[-1];"
        , "sp -= " ++ show (pop-1) ++ ";"
        , "rp--;"
        , "goto *(rp->ip);"
        ]
    instr (LOAD Nothing) =
      return
        [ "{"
        , "  if (! isPtr(sp[-1].tag)) goto _load_fail;"
        , "  uint32_t n = ptrLen(sp[-1].tag);"
        , "  TaggedWord* addr = toPtr(sp[-1].val) + n;"
        , "  sp--;"
        , "  for (uint32_t i = 1; i <= n; i++) {"
        , "    sp[0] = addr[-i];"
        , "    sp++;"
        , "  }"
        , "}"
        ]
    instr (LOAD (Just n)) =
      return $
        [ "{"
        , "  if (! isPtr(sp[-1].tag)) goto _load_fail;"
        , "  TaggedWord* addr = toPtr(sp[-1].val);"
        ] ++ concat
        [ [ "  sp[0] = addr[" ++ show (n-i) ++ "];"
          , "  sp++;"
          ]
        | i <- [1..n]
        ] ++
        [ "}" ]
    instr (STORE Nothing k) =
      return
        [ "{"
        , "  uint32_t n = sp - rp[-1].sp;"
        , "  TaggedWord* addr = &heap[hp];"
        , "  for (uint32_t i = 0; i < n; i++) {"
        , "    heap[hp] = sp[-1];"
        , "    hp++;"
        , "    sp--;"
        , "  }"
        , "  sp[0].tag = makeTag(" ++ ptrKind k ++ ", n);"
        , "  sp[0].val = fromPtr(addr);"
        , "  sp++;"
        , "}"
        ]
    instr (STORE (Just n) k) =
      return $
           [ "{"
           , "  TaggedWord* addr = &heap[hp];"
           ]
        ++ concat [ [ "  heap[hp] = sp[-1];"
                    , "  hp++; sp--;"
                    ]
                  | i <- [1..n] ]
        ++ [ "  sp[0].tag = makeTag(" ++ ptrKind k ++ ", " ++ show n ++ ");"
           , "  sp[0].val = fromPtr(addr);"
           , "  sp++;"
           , "}"
           ]
    instr (BRANCH (polarity, op) pop (InstrLabel label)) =
        return
          [ "if (" ++ cond ++ ") {"
          , "  sp -= " ++ show pop ++ ";"
          , "  goto " ++ mangle label ++ ";"
          , "}"
          ]
      where
        cond = if polarity == Pos then posCond else "!(" ++ posCond ++ ")"
        posCond =
          case op of
            IsAtom str   -> "sp[-1].tag == ATOM && "
                         ++ "sp[-1].val == ATOM_" ++ mangle str
            IsInt i      -> "sp[-1].tag == INT && "
                         ++ "sp[-1].val == " ++ show i
            IsCons       -> "type(sp[-1].tag) == PTR_CONS"
            IsTuple n    -> "type(sp[-1].tag) == PTR_TUPLE &&"
                         ++ "ptrLen(sp[-1].tag) == " ++ show n
            IsApplyPtr   -> "flagApplyPtr"
            IsApplyOk    -> "flagApplyOk"
            IsApplyDone  -> "flagApplyDone"
            IsApplyUnder -> "flagApplyUnder"
    instr CAN_APPLY =
      return
        [ "{"
        , "  uint32_t len = sp - rp[-1].sp;"
        , "  flagApplyPtr = type(sp[-1].tag) == PTR_APP;"
        , "  flagApplyDone = type(sp[-1].tag) != PTR_APP &&"
        , "                   !(type(sp[-1].tag) == FUN &&"
        , "                       funArity(sp[-1].tag) == 0) &&"
        , "                         len == 1;"
        , "  flagApplyOk = type(sp[-1].tag) == FUN &&"
        , "                  len > funArity(sp[-1].tag);"
        , "  flagApplyUnder = type(sp[-1].tag) == FUN &&"
        , "                     len <= funArity(sp[-1].tag);"
        , "}"
        ]
    instr (PRIM prim) =
        return
          [ "if (!(" ++ assert ++ ")) goto _prim_fail;"
          , "sp[-" ++ show pop ++ "].tag = " ++ resultTag ++ ";"
          , "sp[-" ++ show pop ++ "].val = " ++ result ++ ";"
          , if pop > 1 then "sp -= " ++ show (pop-1) ++ ";" else ""
          ]
      where
        resultTag =
          case prim of
            PrimEq -> "ATOM"
            PrimNotEq -> "ATOM"
            PrimLess -> "ATOM"
            PrimLessEq -> "ATOM"
            other -> "INT"
        result =
          case prim of
            PrimAdd -> "sp[-1].val + sp[-2].val"
            PrimSub -> "sp[-1].val - sp[-2].val"
            PrimAddImm imm -> "sp[-1].val + " ++ show imm
            PrimSubImm imm -> "sp[-1].val - " ++ show imm
            PrimEq -> "sp[-1].tag == sp[-2].tag &&"
                   ++ "sp[-1].val == sp[-2].val ? ATOM_true : ATOM_false"
            PrimNotEq -> "sp[-1].tag == sp[-2].tag &&"
                      ++ "sp[-1].val == sp[-2].val ? ATOM_false : ATOM_true"
            PrimLess -> "sp[-1].tag == INT && sp[-2].tag == INT &&"
                     ++ "sp[-1].val < sp[-2].val ? ATOM_false : ATOM_true"
            PrimLessEq -> "sp[-1].tag == INT && sp[-2].tag == INT &&"
                       ++ "sp[-1].val <= sp[-2].val ? ATOM_false : ATOM_true"
        assert =
          case prim of
            PrimAdd -> "sp[-1].tag == INT && sp[-2].tag == INT"
            PrimSub -> "sp[-1].tag == INT && sp[-2].tag == INT"
            PrimAddImm imm -> "sp[-1].tag == INT"
            PrimSubImm imm -> "sp[-1].tag == INT"
            PrimEq -> "(sp[-1].tag == INT && sp[-2].tag == INT) ||"
                   ++ "(sp[-1].tag == ATOM && sp[-2].tag == ATOM)"
            PrimNotEq -> "(sp[-1].tag == INT && sp[-2].tag == INT) ||"
                      ++ "(sp[-1].tag == ATOM && sp[-2].tag == ATOM)"
            PrimLess -> "sp[-1].tag == INT && sp[-2].tag == INT"
            PrimLessEq -> "sp[-1].tag == INT && sp[-2].tag == INT"
        pop =
          case prim of
            PrimAddImm imm -> 1
            PrimSubImm imm -> 1
            other -> 2
    instr HALT =
      return
        [ "render(sp[-1]);"
        , "printf(\"\\n\");"
        , "return 0;" ]
