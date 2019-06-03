module Backend.StandardC where

-- Standard imports
import Data.Char
import System.Directory

-- Local imports
import Syntax
import Bytecode
import Compiler
import Monad.Fresh

-- C generator options
data CGenOpts =
  CGenOpts {
    sourceProg   :: [Decl]
  , targetDir    :: String
  , genMode      :: CGenMode
  }

-- Word width
data CGenMode = CGen_16 | CGen_32 | CGen_64

genC :: CGenOpts -> IO ()
genC opts = do
    createDirectory (targetDir opts)
    writeFile (targetDir opts ++ "/main.c") ccode
    writeFile (targetDir opts ++ "/Makefile") makefile
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

    makefile :: String
    makefile = unlines
      [ "STACK_SIZE ?= 32000"
      , "HEAP_SIZE ?= 32000"
      , "main: main.c"
      , "\t@gcc -D STACK_SIZE=$(STACK_SIZE)         \\"
      , "       -D RET_STACK_SIZE=$(STACK_SIZE)     \\"
      , "       -D HEAP_SIZE=$(HEAP_SIZE)           \\"
      , "       -O3 main.c -o main"
      ]

    mangle :: String -> String
    mangle (x:xs) = first x ++ concatMap rest xs
      where
        encode x = "_" ++ show (fromEnum x)
        first x = if isAlpha x then [x] else encode x
        rest x = if isAlphaNum x then [x] else encode x

    includes :: [String]
    includes =
      [ "#include <stdio.h>"
      , "#include <stdlib.h>"
      , "#include <stdint.h>"
      , "#include <stdbool.h>"
      , "#include <assert.h>"
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
      ++ [ "#define LABEL_" ++ mangle label ++ " " ++ show n
         | (label, n) <- zip labels [0..] ]
      ++ [ "#define ATOM_" ++ mangle atom ++ " " ++ show n
         | (atom, n) <- zip (atoms bytecode) [0..] ]
      where
        labels = [label | LABEL label <- bytecode]

    helpers :: [String]
    helpers =
      [ "INLINE Unsigned type(Unsigned t) { return t & 7; }"
      , "INLINE Unsigned ptrLen(Unsigned t) { return t >> 3; }"
      , "INLINE Unsigned funArity(Unsigned t) { return t >> 3; }"
      , "INLINE bool isPtr(Unsigned t) { return t & 1; }"
      , "INLINE Unsigned makeTag(Unsigned kind, Unsigned n)"
      , "  { return (n << 3) | kind; }"
      ]

    typeDecls :: [String]
    typeDecls =
      [ "typedef uint" ++ mode (genMode opts) ++ "_t Unsigned;"
      , "typedef int" ++ mode (genMode opts) ++ "_t Signed;"
      , "typedef struct { Unsigned tag; Unsigned val; } TaggedWord;"
      , "typedef struct { TaggedWord* sp; void* ip; } RetItem;"
      ]

    mode :: CGenMode -> String
    mode (CGen_16) = "16"
    mode (CGen_32) = "32"
    mode (CGen_64) = "64"

    globals :: [String]
    globals =
      [ "TaggedWord* sp;"
      , "RetItem* rp;"
      , "TaggedWord* heap;"
      , "Unsigned hp;"
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
      , "  if (type(w.tag) == INT) printf(\"%d\", w.val);"
      , "  if (type(w.tag) == ATOM) printf(\"%s\", atoms[w.val]);"
      , "  if (type(w.tag) == FUN) printf(\"FUN\");"
      , "  if (type(w.tag) == PTR_APP) printf(\"APP\");"
      , "  if (type(w.tag) == PTR_CONS) {"
      , "    printf(\"[\");"
      , "    render(heap[w.val]);"
      , "    printf(\"|\");"
      , "    render(heap[w.val+1]);"
      , "    printf(\"]\");"
      , "  }"
      , "  if (type(w.tag) == PTR_TUPLE) {"
      , "    Unsigned n = ptrLen(w.tag);"
      , "    printf(\"{\");"
      , "    for (Unsigned i = 0; i < n; i++) {"
      , "      render(heap[w.val+i]);"
      , "      if (i < n-1) printf(\", \");"
      , "    }"
      , "    printf(\"}\");"
      , "  }"
      , "}"
      ]

    main :: [String]
    main =
         [ "int main() {"
         , "  sp = malloc(STACK_SIZE * sizeof(TaggedWord));"
         , "  rp = malloc(RET_STACK_SIZE * sizeof(RetItem));"
         , "  heap = malloc(HEAP_SIZE * sizeof(TaggedWord));"
         , "  hp = 0;"
         , "  uint8_t flagApplyPtr;"
         , "  uint8_t flagApplyDone;"
         , "  uint8_t flagApplyOk;"
         , "  uint8_t flagApplyUnder;"
         ]
      ++ [ "  void* labels[] = {" ]
      ++ [ "  &&" ++ mangle label ++ ", "
         | LABEL label <- bytecode ]
      ++ [ "  };" ]
      ++ map ("  " ++) (instrs bytecode)
      ++ [ "  return -1;"
         , "}"
         ]
   
    atomTag :: Atom -> String
    atomTag (INT i) = "INT"
    atomTag (ATOM a) = "ATOM"
    atomTag (FUN f n) = "makeTag(FUN, " ++ show n ++ ")"

    atomVal :: Atom -> String
    atomVal (INT i) = show i
    atomVal (ATOM a) = "ATOM_" ++ mangle a
    atomVal (FUN (InstrLabel f) n) = "LABEL_" ++ mangle f

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
        [ "assert(type(sp[-1].tag) == FUN);"
        , "rp->ip = &&" ++ mangle retLabel ++ ";"
        , "rp->sp = sp - funArity(sp[-1].tag) - 1;"
        , "rp++;"
        , "sp--;"
        , "goto *labels[sp[0].val];"
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
        , "  assert(isPtr(sp[-1].tag));"
        , "  Unsigned n = ptrLen(sp[-1].tag);"
        , "  Unsigned addr = sp[-1].val + n;"
        , "  sp--;"
        , "  for (Unsigned i = 1; i <= n; i++) {"
        , "    sp[0] = heap[addr-i];"
        , "    sp++;"
        , "  }"
        , "}"
        ]
    instr (LOAD (Just n)) =
      return $
        [ "{"
        , "  assert(isPtr(sp[-1].tag));"
        , "  Unsigned addr = sp[-1].val + " ++ show (n-1) ++ ";"
        ] ++ concat
        [ [ "  sp[0] = heap[addr];"
          , "  sp++; addr--;"
          ]
        | i <- [1..n]
        ] ++
        [ "}" ]
    instr (STORE Nothing k) =
      return
        [ "{"
        , "  Unsigned n = sp - rp[-1].sp;"
        , "  Unsigned addr = hp;"
        , "  for (Unsigned i = 0; i < n; i++) {"
        , "    heap[hp] = sp[-1];"
        , "    hp++;"
        , "    sp--;"
        , "  }"
        , "  sp[0].tag = makeTag(" ++ ptrKind k ++ ", n);"
        , "  sp[0].val = addr;"
        , "  sp++;"
        , "}"
        ]
    instr (STORE (Just n) k) =
      return $
           [ "{"
           , "  Unsigned addr = hp;"
           ]
        ++ concat [ [ "  heap[hp] = sp[-1];"
                    , "  hp++; sp--;"
                    ]
                  | i <- [1..n] ]
        ++ [ "  sp[0].tag = makeTag(" ++ ptrKind k ++ ", " ++ show n ++ ");"
           , "  sp[0].val = addr;"
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
        , "  Unsigned len = sp - rp[-1].sp;"
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
          [ "assert(" ++ assert ++ ");"
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
