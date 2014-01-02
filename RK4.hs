module RK4 where

data State = S { x :: Float, v :: Float } deriving (Show, Eq)
data Deriv = D { dx :: Float, dv :: Float } deriving (Show, Eq)

type Accel = State -> Float
type Integrator = Accel -> Float -> State -> State

euler :: Integrator
euler af dt s0 = s' where
    s' = S x' v'
    x' = x s0 + dt * v s0
    v' = v s0 + dt * af s0

rk4 :: Integrator
rk4 af dt s0 = s' where
    evaluateHere = evaluate af s0
    a = evaluateHere   (D 0 0)   0
    b = evaluateHere   a         (dt*0.5)
    c = evaluateHere   b         (dt*0.5)
    d = evaluateHere   c         dt
    dxdt = 1.0/6.0 * (dx a + 2.0*(dx b + dx c) + dx d)
    dvdt = 1.0/6.0 * (dv a + 2.0*(dv b + dv c) + dv d)
    d' = D dxdt dvdt
    x' = x s0 + dt * dx d'
    v' = v s0 + dt * dv d'
    s' = S x' v'

evaluate af s d dt = D (v se) (af se) where
    se = S (x s + dt * dx d)
           (v s + dt * dv d)

runIntegrator ig af s dt steps = foldr (.) id (replicate steps (ig af dt)) $ s
scanIntegrator ig af s dt steps = scanl (\s igC -> igC s) s (replicate steps (ig af dt))

carAf s = 1.0
springAf k b s = -k*(x s) - b*(v s)
carExample ig = runIntegrator ig carAf (S 0 0) 0.5 100
springExample ig = runIntegrator ig (springAf 10 1) (S 1 1) 0.5 100
