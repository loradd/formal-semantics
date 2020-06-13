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

-- Natural Semantics ------------------------------------------------------

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
        = Final s'', if b_val b s''
          where Final s''   = ns_stm (Inter ss s)

ns_stm (Inter (Repeat ss b) s)
        = Final s'', if not(b_val b s')
          where Final s'    = ns_stm (Inter ss s)
                Final s''   = ns_stm (Inter (Repeat ss b) s') 
-- Task 1 -----------------------------------------------------------------

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

-- Structural Operational Semantics ---------------------------------------

is_Final (Inter ss s) = False
is_Final (Final s) = True

sos_stm :: Config -> Config

sos_stm (Inter (Ass x a) s)
        = Final (update s x (a_val a s))

sos_stm (Inter (Skip) s)
        = Final s

sos_stm (Inter (Comp ss1 ss2) s)
        = Inter (Comp ss1' ss2) s', if not(is_Final (sos_stm (Inter ss1 s)))
          where Inter ss1' s' = sos_stm (Inter ss1 s)

sos_stm (Inter (Comp ss1 ss2) s)
        = Inter ss2 s', if is_Final (sos_stm (Inter ss1 s))
          where Final s' = sos_stm (Inter ss1 s)

sos_stm (Inter (If b ss1 ss2) s)
        = Inter ss1 s, if b_val b s

sos_stm (Inter (If b ss1 ss2) s)
        = Inter ss2 s, if not(b_val b s)

sos_stm (Inter (While b ss) s)
        = Inter (If b (Comp ss (While b ss)) Skip) s

-- Task 2 -----------------------------------------------------------------
-- <repeat S until b, s> 
-- => <S;if b then skip else (repeat S until b), s>
sos_stm (Inter (Repeat ss b) s)
        = Inter (Comp ss (If b Skip (Repeat ss b))) s
-- Task 2 -----------------------------------------------------------------

deriv_seq (Inter ss s)
        = (Inter ss s) : (deriv_seq (sos_stm (Inter ss s)))
deriv_seq (Final s)
        = [Final s]

s_sos ss s = s'
             where Final s' = last (deriv_seq (Inter ss s))

fac_seq = deriv_seq (Inter factorial s_init)

s_fac' = s_sos factorial s_init

-- Task 2 -----------------------------------------------------------------
s_task2 = s_sos task1 s_init
-- Task 2 -----------------------------------------------------------------

comp :: (a -> b) -> (c -> a) -> (c -> b)
comp f g x = f (g x)

fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (Ass x a)      = \s -> update s x (a_val a s)
s_ds Skip           = \s -> s 
s_ds (Comp ss1 ss2) = comp (s_ds ss2) (s_ds ss1)
s_ds (If b ss1 ss2) = \s -> if b_val b s then s_ds ss1 s else s_ds ss2 s
s_ds (While b ss)   = fix (\g s -> if b_val b s then g (s_ds ss s) else s)  

-- Task 3 -----------------------------------------------------------------
-- S_ds[|repeat S until b|] = FIX F
--   where F g = cond(B[|b|], id, g . S_ds[|S|])
s_ds (Repeat ss b)  = fix (\g s -> if b_val b s then s else g (s_ds ss s)) 
-- Task 3 -----------------------------------------------------------------

s_final = s_ds factorial s_init

-- Task 3 -----------------------------------------------------------------
s_task3 = s_ds task1 s_init
-- Task 3 -----------------------------------------------------------------


type Cont = State -> State
type Env_E = Var -> Cont

-- Task 4 -----------------------------------------------------------------
s_cs :: Stm -> Env_E -> (Cont -> Cont)
s_cs (Ass x a) env_e = \c s -> c (update s x (a_val a s))
s_cs Skip env_e = \c s -> c s
s_cs (Comp ss1 ss2) env_e = comp (s_cs ss1 env_e) (s_cs ss2 env_e)  
s_cs (If b ss1 ss2) env_e = \c s -> if b_val b s then s_cs ss1 env_e c s else s_cs ss2 env_e c s
s_cs (While b ss) env_e = fix (\g c s -> if b_val b s then (s_cs ss env_e (g c) s) else c s)
s_cs (Block ss e ss_e) env_e = \c s -> s_cs ss (update env_e e (s_cs ss_e env_e c)) c s
s_cs (Raise e) env_e = \c s -> (env_e e) s

test = Block (If (Le (V "x") (N 0)) Skip (Raise "exit"))
             "exit" (Ass "x" (N 0))

e_init x = id
c_init = id
s_final' = s_cs test e_init c_init s_init
-- Task 4 -----------------------------------------------------------------
