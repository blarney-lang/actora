module CBackend where

-- Standard imports
import Data.Char
import Data.List
import Control.Monad
import System.Directory
import qualified Data.Set as S
import qualified Data.Map as M

-- Local imports
import Syntax
import Descend
import Compiler
import Monad.Fresh

-- Target: NIOS-II or standard C (both in 32-bit mode)
data CGenMode = Gen_NIOSII_32 | Gen_C_32
  deriving Eq

-- C generator options
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
    if genMode opts == Gen_NIOSII_32
      then return ()
      else do
        writeFile (targetDir opts ++ "/Makefile") stdMakefile
  where
    -- Compile an expression to a simple expression
    -- and an instruction sequence
    exp :: Exp -> Fresh (Exp, [String])
    -- Atoms, integers, variables
    exp (Atom a) = return (Atom a, [])
    exp (Int i) = return (Int i, [])
    exp (Var v) = return (Var v, [])
    -- Functions
    exp (Fun f n) = error ("Unapplied function " ++ f)
    -- Lists and tuples
    exp (Cons e0 e1) = do
      (x0, code0) <- exp e0
      (x1, code1) <- exp e1
      v <- fresh
      return (Var v, code0 ++ code1 ++
        [ "Word " ++ v ++ " = makePtr(hp);"
        , "hp[0] = makeCons();"
        , "hp[1] = " ++ simple x0 ++ ";"
        , "hp[2] = " ++ simple x1 ++ ";"
        , "hp += 3;"
        ])
    exp (Tuple es) = do
      let n = length es
      (xs, codes) <- unzip <$> mapM exp es
      v <- fresh
      return (Var v, concat codes ++
        [ "Word " ++ v ++ " = makePtr(hp);"
        , "hp[0] = makeTuple(" ++ show n ++ ");"
        ] ++
        [ "hp[" ++ show i ++ "] = " ++ simple x ++ ";"
        | (x, i) <- zip xs [1..]
        ] ++
        [ "hp += " ++ show (n+1) ++ ";"])
    -- Application of primitive function
    exp (Apply (Fun f n) [e0, e1])
      | isPrim f = do
          (x0, code0) <- exp e0
          (x1, code1) <- exp e1
          v <- fresh
          return (Var v, code0 ++ code1 ++
            [ "Word " ++ v ++ " = " ++
                prim f (simple x0) (simple x1) ++ ";"
            ])
    -- Closure creation
    exp (Apply (Fun f n) es)
      | head f == '\\' = do
          let m = length es
          (xs, codes) <- unzip <$> mapM exp es
          v <- fresh
          return (Var v, concat codes ++
            [ "Word " ++ v ++ " = makePtr(hp);"
            , "hp[0] = makeApp(" ++ show (n-m) ++ ");"
            , "hp[1] = (Word)" ++ mangle f ++ ";"
            ] ++
            [ "hp[" ++ show i ++ "] = " ++ simple x ++ ";"
            | (x, i) <- zip xs [2..] ] ++
            [ "hp += " ++ show (n+2) ++ ";"])
    -- Application of known function
    exp (Apply (Fun f n) es)
      | n == length es = do
          (xs, codes) <- unzip <$> mapM exp es
          v <- fresh
          return (Var v, concat codes ++
            [ "Word " ++ v ++ " = " ++ mangle f ++ "(" ++
                concat (intersperse "," [simple x | x <- xs]) ++ ");"
            ])
      | otherwise =
          error ("Applying function " ++ f ++ " to wrong number of args")
    -- Application of unknown function
    exp (Apply f es) = do
      let n = length es
      (fes, codes) <- unzip <$> mapM exp (f:es)
      let x:ys = fes
      let funArgs = concat (intersperse "," (replicate n "Word"))
      v <- fresh
      return $ (Var v, concat codes ++
        [ "Word " ++ v ++ ";"
        , check ("isPtr(" ++ simple x ++ ")") "F"
        , "{"
        , "  Word* app = getPtr(" ++ simple x ++ ");"
        , "  " ++ check ("(isApp(app[0]) && getLen(app[0]) == "
                           ++ show n ++ ")") "F"
        , "  Word (*f)(Word*," ++ funArgs ++ ") = " ++
               "(Word (*)(Word*," ++ funArgs ++ ")) app[1];"
        , "  " ++ v ++ " = f(app," ++
                    concat (intersperse "," [simple y | y <- ys]) ++ ");"
        , "}"
        ])
    -- Conditional expression
    exp (Cond c e0 e1) = do
      (c', condCode) <- exp c
      (e0', e0Code) <- seq e0
      (e1', e1Code) <- seq e1
      v <- fresh
      return (Var v, condCode ++
        [ "Word " ++ v ++ ";"
        , "if (" ++ simple c' ++ " == makeAtom(ATOM_true)) {"
        ] ++
        e0Code ++
        [ v ++ " = " ++ simple e0' ++ ";"
        , "} else {"
        ] ++
        e1Code ++
        [ v ++ " = " ++ simple e1' ++ ";"
        , "}"
        ])
    -- Case expression
    exp (Case e alts) = do
        v <- fresh
        endLabel <- fresh
        (subj, subjCode) <- exp e
        altCode <- mapM (caseAlt v (simple subj) endLabel) alts
        return (Var v,
          [ "Word " ++ v ++ ";" ] ++
          subjCode ++ concat altCode ++
          [ "_error('C');"
          , endLabel ++ ": ;"
          ])
      where
        -- Compile case alternative
        caseAlt v subj endLabel (p, g, body) = do
          failLabel <- fresh
          let fail = "goto " ++ failLabel ++ ";"
          matchCode <- match subj p fail
          guardCode <- guard g fail
          (body', bodyCode) <- seq body
          return (
            matchCode ++
            guardCode ++
            bodyCode ++
              [ v ++ " = " ++ simple body' ++ ";"
              , "goto " ++ endLabel ++ ";"
              , failLabel ++ ": ;"
              ])

    -- Compile a simple expression
    simple :: Exp -> String
    simple (Atom a) = "makeAtom(ATOM_" ++ mangle a ++ ")"
    simple (Int i) = "makeInt(" ++ show i ++ ")"
    simple (Var v) = mangle v

    -- Compile pattern guard
    guard :: Guard -> Id -> Fresh [String]
    guard g fail =
      case g of
        Nothing -> return []
        Just cond -> branchNot cond fail

    -- Branch if expression evaluates to false
    branchNot :: Exp -> Id -> Fresh [String]
    branchNot e fail = do
      (e', is) <- exp e
      return (is ++
        [ "if (" ++ simple e' ++ " != makeAtom(ATOM_true)) " ++ fail ])

    -- Primitives
    prim :: Id -> String -> String -> String
    prim "+" x0 x1 = "add(" ++ x0 ++ ", " ++ x1 ++ ")"
    prim "-" x0 x1 = "sub(" ++ x0 ++ ", " ++ x1 ++ ")"
    prim "==" x0 x1 = "eq(" ++ x0 ++ ", " ++ x1 ++ ")"
    prim "/=" x0 x1 = "neq(" ++ x0 ++ ", " ++ x1 ++ ")"
    prim "<" x0 x1 = "less(" ++ x0 ++ ", " ++ x1 ++ ")"
    prim "<=" x0 x1 = "lessEq(" ++ x0 ++ ", " ++ x1 ++ ")"
    prim ">" x0 x1 = "less(" ++ x1 ++ ", " ++ x0 ++ ")"
    prim ">=" x0 x1 = "lessEq(" ++ x1 ++ ", " ++ x0 ++ ")"

    -- Pattern matching
    match :: String -> Exp -> Id -> Fresh [String]
    match subj (Var w) fail =
      return
        [ "Word " ++ mangle w ++ " = " ++ subj ++ ";" ]
    match subj (Atom a) fail =
      return
        [ "if (" ++ subj ++ " != makeAtom(ATOM_" ++ mangle a ++ ")) " ++ fail
        ]
    match subj (Int i) fail = 
      return
        [ "if (" ++ subj ++ " != makeInt(" ++ show i ++ ")) " ++ fail
        ]
    match subj (Fun f n) fail =
      error ("Pattern contains function identifier " ++ f)
    match subj (Cons p0 p1) fail = do
      h <- fresh
      t <- fresh
      code1 <- match h p0 fail
      code2 <- match t p1 fail
      return $
        [ "if (!isPtr(" ++ subj ++ ")) " ++ fail
        , "Word " ++ h ++ ", " ++ t ++ ";"
        , "{"
        , "  Word* ptr = getPtr(" ++ subj ++ ");"
        , "  if (!isCons(*ptr)) " ++ fail
        , "  " ++ h ++ " = ptr[1]; " ++ t ++ " = ptr[2];"
        , "}"
        ] ++ code1 ++ code2
    match subj (Tuple ps) fail = do
      let n = length ps
      vs <- replicateM n fresh
      codes <- zipWithM (\v p -> match v p fail) vs ps
      return $
        [ "if (!isPtr(" ++ subj ++ ")) " ++ fail
        , "Word " ++ concat (intersperse "," vs) ++ ";"
        , "{"
        , "  Word* ptr = getPtr(" ++ subj ++ ");"
        , "  if (!isTuple(*ptr)) " ++ fail
        , "  " ++ concat [ v ++ " = ptr[" ++ show i ++ "]; "
                         | (v, i) <- zip vs [1..] ]
        , "}"
        ] ++ concat codes

    seq :: [Exp] -> Fresh (Exp, [String])
    seq [] = error "C backend: seq applied to empty list"
    seq (Bind p e : rest) = do
      (e', eCode) <- exp e
      let fail = "_error(\"B\");"
      matchCode <- match (simple e') p fail
      if null rest
        then return (e', eCode ++ matchCode)
        else do
          (res, restCode) <- seq rest
          return (res, eCode ++ matchCode ++ restCode)

    seq [e] = exp e
    seq (e:rest) = do
      (e', eCode) <- exp e
      (rest', restCode) <- seq rest
      return (rest', eCode ++ restCode)

    -- Core program
    coreProg :: [Decl]
    coreProg = core (topModName opts) (sourceProg opts)

    -- Mapping from function name to list of equations
    funTable :: M.Map Id [([Exp], Guard, [Exp])]
    funTable = M.fromListWith (flip (++))
      [ (f, [(args, g, rhs)])
      | FunDecl f args g rhs <- coreProg ]

    -- Mapping from closure name to list of equations
    closureTable :: M.Map Id [([Id], [Exp], Guard, [Exp])]
    closureTable = M.fromListWith (flip (++))
      [ (f, [(vs, ps, g, rhs)])
      | ClosureDecl f vs ps g rhs <- coreProg ]

    -- Function compiler
    fun :: (Id, [([Exp], Guard, [Exp])]) -> Fresh [String]
    fun (f, eqns) = do
      args <- replicateM arity fresh
      code <- concat <$> mapM (eqn args) eqns
      return (
        [ "Word " ++ mangle f ++ "(" ++
            concat (intersperse ", " ["Word " ++ arg | arg <- args]) ++ ") {"
        ] ++ code ++
        [ "_error('E');"
        , "}"
        ])
      where
        arity = let (args, _, _) = head eqns in length args

    -- Compile one equation
    eqn :: [Id] -> ([Exp], Guard, [Exp]) -> Fresh [String]
    eqn args (ps, g, rhs) = do
        failLabel <- fresh
        let fail = "goto " ++ failLabel ++ ";"
        matchCode <- matcher (zip args ps) g fail
        (rhs', rhsCode) <- seq rhs
        return (["{"]
                  ++ matchCode
                  ++ rhsCode
                  ++ ["return " ++ simple rhs' ++ ";"]
                  ++ ["}"]
                  ++ [failLabel ++ ": ;"])
      where
        matcher [] g fail = guard g fail
        matcher ((v, p):rest) g fail = do
          codeFirst <- match v p fail
          codeRest <- matcher rest g fail
          return (codeFirst ++ codeRest)

    -- Function closure compiler
    closure :: (Id, [([Id], [Exp], Guard, [Exp])]) -> Fresh [String]
    closure (f, eqns) = do
      args <- replicateM arity fresh
      code <- concat <$> mapM (eqn args)
                [(ps, g, rhs) | (vs, ps, g, rhs) <- eqns]
      return (
        [ "Word " ++ mangle f ++ "(Word* env, " ++
            concat (intersperse ", " ["Word " ++ arg | arg <- args]) ++ ") {"
        ] ++
        [ "Word " ++ v ++ " = env[" ++ show i ++ "];"
        | (v, i) <- zip env [2..] ] ++
        code ++
        [ "_error('E');"
        , "}"
        ])
      where
        arity = let (_, args, _, _) = head eqns in length args
        env = let (vs, _, _, _) = head eqns in vs

    -- Compiled functions
    funs :: [String]
    funs = snd (runFresh m "x_" 0)
      where
        m = do code0 <- concat <$> mapM fun (M.toList funTable)
               code1 <- concat <$> mapM closure (M.toList closureTable)
               return ([""] ++ code0 ++ [""] ++ code1)

    -- Function prototypes
    protos :: [String]
    protos = map fun (M.toList funTable)
          ++ map closure (M.toList closureTable)
      where
        fun (f, eqns) =
            "Word " ++ mangle f ++ "(" ++ 
              concat (intersperse ", "
                ["Word w" ++ show i | i <- [1..n]]) ++ ");"
          where n = let (ps, _, _) = head eqns in length ps

        closure (f, eqns) =
            "Word " ++ mangle f ++ "(Word* env, " ++ 
              concat (intersperse ", "
                ["Word w" ++ show i | i <- [1..n]]) ++ ");"
          where n = let (_, ps, _, _) = head eqns in length ps

    -- Name mangler
    mangle :: String -> String
    mangle (x:xs) = first x ++ concatMap rest xs
      where
        encode x = "_" ++ show (fromEnum x)
        first x = if isAlpha x || x == '_' then [x] else encode x
        rest x = if isAlphaNum x || x == '_' then [x] else encode x

    -- Assert condition
    check cond errCode =
     "if (!" ++ cond ++ ") _error('" ++ errCode ++ "');" 

    ccode :: String
    ccode = unlines $ concat
      [ includes
      , helpers
      , atomNames
      -- , garbageCollector
      , protos
      , funs
      , render
      , main
      ]

    -- Default heap size
    defaultHeapSize :: Int
    defaultHeapSize = 28000
    
    -- Makefile for standard C generator
    stdMakefile :: String
    stdMakefile = unlines
      [ "HEAP_SIZE ?= " ++ show defaultHeapSize
      , "main: main.c"
      , "\t@gcc -D HEAP_SIZE=$(HEAP_SIZE) \\"
      , "       -falign-functions=4 \\"
      , "       -m32 -O3 main.c -o main"
      ]

    includes :: [String]
    includes =
        [ "#include <stdint.h>"
        , "#include <stdbool.h>"
        ]
     ++ if genMode opts == Gen_NIOSII_32
        then
          [ "#include <baremetal.h>"
          ]
        else
          [ "#include <stdio.h>"
          , "#include <stdlib.h>"
          ]

    helpers :: [String]
    helpers =
      [ "#define INLINE inline __attribute__((always_inline))"
      , ""
      , "typedef uint32_t Word;"
      , "typedef uint32_t Hdr;"
      , ""
      , "Word* heap;"
      , "Word* heap2;"
      , "Word* hp;"
      ] ++
      ( if genMode opts == Gen_NIOSII_32
        then [ "extern uint32_t __e_heapBase;"
             , "extern uint32_t __e_heap2Base;"
             ]
        else []
      ) ++ [""] ++
      [ "#define ATOM_" ++ mangle atom ++ " " ++ show n
      | (atom, n) <- zip atoms [0..] ] ++
      [ ""
      , "#define TAG_HDR 0"
      , "#define TAG_INT 1"
      , "#define TAG_ATOM 2"
      , "#define TAG_PTR 3"
      , ""
      , "#define HDR_CONS 0"
      , "#define HDR_TUPLE 1"
      , "#define HDR_APP 2"
      , ""
      , "INLINE bool isPtr(Word x) { return (x&3) == TAG_PTR; }"
      , "INLINE uint32_t* getPtr(Word x) " ++
          "{ return (uint32_t*) (x & 0xfffffffc); }"
      , "INLINE Word makePtr(Word* x) " ++
          "{ return ((uint32_t) x) | TAG_PTR; }"
      , ""
      , "INLINE bool isHdr (Word x) { return (x&3) == TAG_HDR; }"
      , "INLINE bool isCons(Hdr x) { return ((x>>2)&3) == HDR_CONS; }"
      , "INLINE bool isTuple(Hdr x) { return ((x>>2)&3) == HDR_TUPLE; }"
      , "INLINE bool isApp(Hdr x) { return ((x>>2)&3) == HDR_APP; }"
      , "INLINE uint32_t getLen(Hdr x) { return x >> 4; }"
      , "INLINE Hdr makeCons() { return (2 << 4) | (HDR_CONS << 2); }"
      , "INLINE Hdr makeTuple(uint32_t len) "
          ++ "{ return (len<<4) | (HDR_TUPLE<<2); }"
      , "INLINE Hdr makeApp(uint32_t len) "
          ++ "{ return (len<<4) | (HDR_APP<<2); }"
      , ""
      , "INLINE bool isInt(Word x) { return (x&3) == TAG_INT; }"
      , "INLINE int32_t getInt(Word x) { return (int32_t) (x >> 2); }"
      , "INLINE Word makeInt(int32_t x) " ++
          "{ return (((uint32_t) x) << 2) | TAG_INT; }"
      , ""
      , "INLINE bool isAtom(Word x) { return (x&3) == TAG_ATOM; }"
      , "INLINE Word makeAtom(uint32_t x) { return (x << 2) | TAG_ATOM; }"
      , ""
      , "void _error(char errorCode) {"
      , "  putchar(errorCode);"
      , "  putchar('\\n');"
      , "  while (1);"
      , "}"
      , ""
      , "INLINE Word add(Word x, Word y) {"
      , check "isInt(x) && isInt(y)" "P"
      , "return makeInt(getInt(x) + getInt(y));"
      , "}"
      , ""
      , "INLINE Word sub(Word x, Word y) {"
      , check "isInt(x) && isInt(y)" "P"
      , "return makeInt(getInt(x) - getInt(y));"
      , "}"
      , ""
      , "INLINE Word eq(Word x, Word y) {"
      , check "(isInt(x) && isInt(y)) || (isAtom(x) && isAtom(y))" "P"
      , "return makeAtom(getInt(x) == getInt(y) ? ATOM_true : ATOM_false);"
      , "}"
      , ""
      , "INLINE Word neq(Word x, Word y) {"
      , check "(isInt(x) && isInt(y)) || (isAtom(x) && isAtom(y))" "P"
      , "return makeAtom(getInt(x) != getInt(y) ? ATOM_true : ATOM_false);"
      , "}"
      , ""
      , "INLINE Word less(Word x, Word y) {"
      , check "isInt(x) && isInt(y)" "P"
      , "return makeAtom(getInt(x) < getInt(y) ? ATOM_true : ATOM_false);"
      , "}"
      , ""
      , "INLINE Word lessEq(Word x, Word y) {"
      , check "isInt(x) && isInt(y)" "P"
      , "return makeAtom(getInt(x) <= getInt(y) ? ATOM_true : ATOM_false);"
      , "}"
      , ""
      , "void " ++ mangle "$ifFail" ++ "() { _error('I'); }"
      , ""
      ]

    atomNames :: [String]
    atomNames =
         [ "const char* atoms[] = {" ]
      ++ [ "\"" ++ atom ++ "\", "
         | atom <- atoms ]
      ++ [ "};" ]

    atoms :: [String]
    atoms = S.toList (S.fromList (
              ["true", "false", "[]"] ++ concatMap get coreProg))
      where
        get (FunDecl v ps g es) =
             [x | p <- ps, Atom x <- universe p]
          ++ [x | e <- es, Atom x <- universe e]
          ++ [x | Just e <- [g], Atom x <- universe e]
        get (ClosureDecl v vs ps g es) =
             [x | p <- ps, Atom x <- universe p]
          ++ [x | e <- es, Atom x <- universe e]
          ++ [x | Just e <- [g], Atom x <- universe e]
        get other = []

    render :: [String]
    render =
      [ "void render(Word w) {"
      , if genMode opts == Gen_NIOSII_32
          then "  if (isInt(w)) printf(\"0x%x\", getInt(w));"
          else "  if (isInt(w)) printf(\"%d\", getInt(w));"
      , "  if (isAtom(w)) printf(\"%s\", atoms[getInt(w)]);"
      , "  if (isPtr(w)) {"
      , "    Word* app = getPtr(w);"
      , "    if (isCons(app[0])) {"
      , "      printf(\"[\");"
      , "      render(app[1]);"
      , "      printf(\"|\");"
      , "      render(app[2]);"
      , "      printf(\"]\");"
      , "    }"
      , "    if (isTuple(app[0])) {"
      , "      uint32_t n = getLen(app[0]);"
      , "      printf(\"{\");"
      , "      for (uint32_t i = 0; i < n; i++) {"
      , "        render(app[i+1]);"
      , "        if (i < n-1) printf(\", \");"
      , "      }"
      , "      printf(\"}\");"
      , "    }"
      , "    if (isApp(app[0])) printf(\"APP\");"
      , "  }"
      , "}"
      , ""
      ]

    main :: [String]
    main =
         [ "int main() {"
         ]
      ++ ( if genMode opts == Gen_NIOSII_32
           then
             [ "  heap = (Word*) &__e_heapBase;"
             , "  heap2 = (Word*) &__e_heap2Base;"
             ]
           else
             [ "  heap = (Word*) malloc(HEAP_SIZE);"
             , "  heap2 = (Word*) malloc(HEAP_SIZE);"
             ]
         )
      ++ [ "  hp = heap;"
         , "  Word result = " ++
                mangle (topModName opts ++ ":" ++ "start") ++ "();"
         , "  render(result);"
         , "  printf(\"\\n\");"
         , "  return 0;"
         , "}"
         ]
