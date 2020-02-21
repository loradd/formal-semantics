type Var = [Char]

data Aexp = N Int | V Var | Add Aexp Aexp |
            Mult Aexp Aexp | Sub Aexp Aexp
data Bexp = TRUE | FALSE | Eq Aexp Aexp | Le Aexp Aexp |
            Neg Bexp | And Bexp Bexp
data Stm =  Ass Var Aexp | Skip | Comp Stm Stm |
            If Bexp Stm Stm | While Bexp Stm | Repeat Stm Bexp |
            Block Stm Var Stm | Raise Var

type Z = Int
type T = Bool

type State = Var -> Z

update s x v y = v, if x == y
               = s y, otherwise

a_val :: Aexp -> State -> Z 
a_val (N n) s           = n
a_val (V x) s           = s x
a_val (Add a1 a2) s     = (a_val a1 s) + (a_val a2 s)
a_val (Mult a1 a2) s    = (a_val a1 s) * (a_val a2 s)
a_val (Sub a1 a2) s     = (a_val a1 s) - (a_val a2 s)

b_val :: Bexp -> State -> T
b_val TRUE s        = True
b_val FALSE s       = False
b_val (Eq a1 a2) s  = True,     if a_val a1 s == a_val a2 s
                    = False,    if a_val a1 s /= a_val a2 s
b_val (Le a1 a2) s  = True,     if a_val a1 s <= a_val a2 s
                    = False,    if a_val a1 s > a_val a2 s
b_val (Neg b) s     = True,     if b_val b s == False
                    = False,    if b_val b s == True
b_val (And b1 b2) s = True,     if b_val b1 s == True && 
                                   b_val b2 s == True
                    = False,    if b_val b1 s == False ||
                                   b_val b2 s == False

data Config = Inter Stm State | Final State

ns_stm  :: Config -> Config

ns_stm  (Inter (Ass x a) s)
        = Final (update s x (a_val a s))

ns_stm  (Inter (Skip) s)
        = Final s

ns_stm  (Inter (Comp ss1 ss2) s)
        = Final s''
          where Final s'    = ns_stm (Inter ss1 s)
                Final s''   = ns_stm (Inter ss2 s')

ns_stm  (Inter (If b ss1 ss2) s)
        = Final s', if b_val b s
          where Final s'    = ns_stm (Inter ss1 s)
ns_stm  (Inter (If b ss1 ss2) s)
        = Final s', if not(b_val b s)
          where Final s'    = ns_stm (Inter ss2 s)

ns_stm  (Inter (While b ss) s)
        = Final s'', if b_val b s
          where Final s'    = ns_stm (Inter ss s)
                Final s''   = ns_stm (Inter (While b ss) s') 
ns_stm  (Inter (While b ss) s)
        = Final s, if not(b_val b s)

-- Task 1 -----------------------------------------------------------------

ns_stm  (Inter (Repeat ss b) s)
        = Final s'', if b_val b s
          where Final s''   = ns_stm (Inter ss s)

ns_stm  (Inter (Repeat ss b) s)
        = Final s'', if not(b_val b s)
          where Final s'    = ns_stm (Inter ss s)
                Final s''   = ns_stm (Inter (If b Skip (Repeat ss b)) s')

---------------------------------------------------------------------------

s_ns ss s = s'
            where Final s' = ns_stm (Inter ss s)

s_init "x" = 3
s_init y = 0

factorial = Comp (Ass "y" (N 1))
                 (While (Neg (Eq (V "x") (N 1)))
                    (Comp (Ass "y" (Mult (V "y") (V "x")))
                          (Ass "x" (Sub (V "x") (N 1)))))

s_fac = s_ns factorial s_init

-- Task 1 -----------------------------------------------------------------

task1 = Comp (Ass "y" (N 1)) 
             (Repeat 
                (Comp (Ass "y" (Mult (V "y") (V "x")))
                      (Ass "x" (Sub (V "x") (N 1))))
                (Le (V "x") (N 0)))
s_task1 = s_ns task1 s_init

-- Task 1 -----------------------------------------------------------------