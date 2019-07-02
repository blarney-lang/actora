module CBackend where

-- Standard imports
import Data.Char
import System.Directory
import Control.Monad

-- Local imports
import Syntax
import Bytecode
import Compiler
import Monad.Fresh

-- Word width
data CGenMode = BareGen_16 | BareGen_32 | CGen_32

-- Baremetal C generator options
data CGenOpts =
  CGenOpts {
    topModName   :: String
  , genMode      :: CGenMode
  , sourceProg   :: [Decl]
  , targetDir    :: String
  }

genC :: CGenOpts -> IO ()
genC opts = do
    createDirectoryIfMissing True (targetDir opts)
    writeFile (targetDir opts ++ "/main.c") ccode
    if baremetal
      then return ()
      else do
        writeFile (targetDir opts ++ "/Makefile") stdMakefile
  where
    bytecode :: [Instr]
    bytecode = compile (topModName opts) (sourceProg opts)

    baremetal :: Bool
    baremetal =
      case genMode opts of
        BareGen_16 -> True
        BareGen_32 -> True
        CGen_32 -> False

    int_t :: String
    int_t =
      case genMode opts of
        BareGen_16 -> "int16_t"
        BareGen_32 -> "int32_t"
        CGen_32 -> "int32_t"

    defaultRetStackSize = 1024
    defaultStackSize    = 4096
    defaultHeapSize     = 28000

    ccode :: String
    ccode = unlines $ concat
      [ includes
      , defines
      , typeDecls
      , helpers
      , atomNames
      , globals
      , garbageCollector
      , render
      , main
      ]

    stdMakefile :: String
    stdMakefile = unlines
      [ "STACK_SIZE ?= " ++ show defaultStackSize
      , "RET_STACK_SIZE ?= " ++ show defaultRetStackSize
      , "HEAP_SIZE ?= " ++ show (defaultHeapSize)
      , "main: main.c"
      , "\t@gcc -D STACK_SIZE=$(STACK_SIZE)         \\"
      , "       -D RET_STACK_SIZE=$(RET_STACK_SIZE) \\"
      , "       -D HEAP_SIZE=$(HEAP_SIZE)           \\"
      , "       -m32 -O3 main.c -o main"
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
     ++ if baremetal
        then
          [ "#include <baremetal.h>"
          ]
        else
          [ "#include <stdio.h>"
          , "#include <stdlib.h>"
          ]
      
    defines :: [String]
    defines =
         [ -- "#define HEAP_SIZE " ++ show defaultHeapSize
           ""
         , "#define INLINE inline __attribute__((always_inline))"
         , ""
         , "#define PTR_APP   1"
         , "#define PTR_CONS  3"
         , "#define PTR_TUPLE 5"
         , "#define INT       0"
         , "#define ATOM      2"
         , "#define FUN       4"
         , "#define GC        6"
         ]
      ++ [ "#define ATOM_" ++ mangle atom ++ " " ++ show n
         | (atom, n) <- zip (atoms bytecode) [0..] ]
      where
        labels = [label | LABEL label <- bytecode]

    typeDecls :: [String]
    typeDecls =
      [ "typedef u" ++ int_t ++ " Unsigned;"
      , "typedef " ++ int_t ++ " Signed;"
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
      [ "TaggedWord* stackBase;"
      , "TaggedWord* sp;"
      , "RetItem* rp;"
      , "TaggedWord* heap;"
      , "TaggedWord* heap2;"
      , "Unsigned hp;"
      ] ++
      if baremetal
        then
          [ "extern uint32_t __e_stackBase;"
          , "extern uint32_t __e_heapBase;"
          , "extern uint32_t __e_heap2Base;"
          , "extern uint32_t __e_retBase;"
          ]
        else []

    atomNames :: [String]
    atomNames =
         [ "const char* atoms[] = {" ]
      ++ [ "\"" ++ atom ++ "\", "
         | atom <- atoms bytecode ]
      ++ [ "};" ]

    render :: [String]
    render =
      [ "void render(TaggedWord w) {"
      , if baremetal
          then "  if (type(w.tag) == INT) printf(\"0x%x\", w.val);"
          else "  if (type(w.tag) == INT) printf(\"%d\", w.val);"
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
         ]
      ++ ( if baremetal
           then
             [ "  sp = stackBase = (TaggedWord*) &__e_stackBase;"
             , "  rp = (RetItem*) &__e_retBase;"
             , "  heap = (TaggedWord*) &__e_heapBase;"
             , "  heap2 = (TaggedWord*) &__e_heap2Base;"
             ]
           else
             [ "  sp = stackBase = (TaggedWord*) malloc(STACK_SIZE);"
             , "  rp = (RetItem*) malloc(RET_STACK_SIZE);"
             , "  heap = (TaggedWord*) malloc(HEAP_SIZE);"
             , "  heap2 = (TaggedWord*) malloc(HEAP_SIZE);"
             ]
         )
      ++ [ "  hp = 0;"
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
    atomVal (FUN (InstrLabel f) n) = "(Unsigned) &&" ++ mangle f

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
        , "  if (hp+n >= (HEAP_SIZE/sizeof(TaggedWord))) gc();"
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
           , "  if (hp+" ++ show n ++
                      " >= (HEAP_SIZE/sizeof(TaggedWord))) gc();"
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
          , "sp[-" ++ show pop ++ "].val = " ++ result ++ ";"
          , "sp[-" ++ show pop ++ "].tag = " ++ resultTag ++ ";"
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
                     ++ "sp[-1].val < sp[-2].val ? ATOM_true : ATOM_false"
            PrimLessEq -> "sp[-1].tag == INT && sp[-2].tag == INT &&"
                       ++ "sp[-1].val <= sp[-2].val ? ATOM_true : ATOM_false"
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

    garbageCollector =
      [ "INLINE TaggedWord* gcCopy(TaggedWord* w, TaggedWord* front) {"
      , "  if (isPtr(w->tag)) {"
      , "    TaggedWord* p = toPtr(w->val);"
      , "    if (p->tag == GC)"
      , "      w->val = p->val;"
      , "    else {"
      , "      uint32_t len = ptrLen(w->tag);"
      , "      for (uint32_t i = 0; i < len; i++) front[i] = p[i];"
      , "      p->tag = GC;"
      , "      p->val = fromPtr(front);"
      , "      w->val = fromPtr(front);"
      , "      front += len;"
      , "    }"
      , "  }"
      , "  return front;"
      , "}"
      , ""
      , "void gc()"
      , "{ "
      , "  // Setup to-space"
      , "  TaggedWord* back = heap2;"
      , "  TaggedWord* front = heap2;"
      , ""
      , "  // Loop over stack"
      , "  TaggedWord* s = stackBase;"
      , "  while (s != sp) {"
      , "    front = gcCopy(s, front);"
      , "    s++;"
      , "  }"
      , ""
      , "  // Copy reachable heap to to-space"
      , "  while (back != front) {"
      , "    front = gcCopy(back, front);"
      , "    back++;"
      , "  }"
      , ""
      , "  hp = front - heap2;"
      , ""
      , "  // Swap from-space and to-space"
      , "  TaggedWord* tmp;"
      , "  tmp = heap;"
      , "  heap = heap2;"
      , "  heap2 = tmp;"
      , "}"
      ]
