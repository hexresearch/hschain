-- |NES.hs
--
-- Approximate Natural Evolution Strategies.
--
-- Natural Evolution Strategy at Wikipedia: https://en.wikipedia.org/wiki/Natural_evolution_strategy
--
-- Copyright (C) 2020 Serguey Zefirov.

{-# OPTIONS -Wno-tabs #-}

{-# LANGUAGE DerivingStrategies, FlexibleContexts, GeneralizedNewtypeDeriving
           , MultiParamTypeClasses, RecordWildCards
           , StandaloneDeriving #-}

module ML.NES where

import Control.Monad
import Control.Monad.State

import qualified Data.List as List

import System.Random

import Debug.Trace

checkAsserts :: Bool
checkAsserts = True

type FLOAT = Double

newtype Vec = Vec { vec :: [FLOAT] } deriving (Eq, Ord, Show)

instance Num Vec where
	fromInteger x = error $ "vec does not support fromInteger, use @constVec "++show x++" someOtherVec@ instead"
	Vec a + Vec b = Vec $ zipWith (+) a b
	_ * _ = error "multiplication is not supported for vecs"
	Vec a - Vec b = Vec $ zipWith (-) a b
	abs (Vec xs) = Vec $ map abs xs
	signum _ = error "no signum for vec"

vSum :: Vec -> FLOAT
vSum (Vec a) = sum a

vProduct :: Vec -> FLOAT
vProduct (Vec a) = product a

vMap :: (FLOAT -> FLOAT) -> Vec -> Vec
vMap f (Vec a) = Vec $ map f a

vZip :: (FLOAT -> FLOAT -> FLOAT) -> Vec -> Vec -> Vec
vZip op (Vec a) (Vec b) = Vec $ zipWith op a b

vInner :: Vec -> Vec -> FLOAT
vInner (Vec a) (Vec b) = sum $ zipWith (*) a b

vScale :: FLOAT -> Vec -> Vec
vScale a (Vec b) = Vec $ map (* a) b

vL2, vL1 :: Vec -> FLOAT
vL2 a = vInner a a
vL1 (Vec a) = sum $ map abs a

vFromList :: [FLOAT] -> Vec
vFromList = Vec

vToList :: Vec -> [FLOAT]
vToList (Vec a) = a

vLength :: Vec -> Int
vLength (Vec a) = length a

-- |A=diag(D) + UV^T.
--
-- A is equal to diagonal matrix D plus outer product
-- of V and U.
data A = A { aD, aU, aV :: Vec } deriving (Show)

invertA :: A -> A
invertA (A d u v) = A d' u' v'
	where
		d' = vMap (1/) d
		g = vSum $ vZip (*) d' $ vZip (*) u v
		alpha = (-1) / (1 + g)
		v' = vScale alpha $ vZip (*) d' v
		u' = vZip (*) d' u

determinantA :: A -> FLOAT
determinantA (A d u v) = (1 + tr) * vProduct d
	where
		d' = vMap (1/) d
		tr = vSum $ vZip (*) d' $ vZip (*) u v

transposeA :: A -> A
transposeA (A d u v) = A d v u

matrMultA :: A -> Vec -> Vec
matrMultA (A d u v) w = vZip (+) dw $ vScale vw u
	where
		dw = vZip (*) d w
		vw = vInner v w

-- |Normal distribution parametrization.
--
-- S = AA^T, S^-1 = (A^-T)(A^-1).
-- PDF: P(x) = (2 * pi) ^ (- length mu / 2)
--           * (determinant(S)) ^ (-1/2)
--           * exp( (-1/2) * (x -mu ) (S^-1) (x - mu))
-- Juxtaposition above means matrix multiplication.
-- det(S) = det(A)^2
-- det(A) = (1 + trace(D^(-1)uv^T)) * det(D)
data N = N { nMu :: Vec, nA :: A } deriving (Show)

data DN = DN { dnDMu :: Vec, dnDA :: A } deriving (Show)

numDerivative :: (Vec -> FLOAT) -> Vec -> Vec
numDerivative f center = vScale (0.5/d) $ dps + dms
	where
		l = vLength center
		fc = f center
		d = 1e-4
		ds = map (vFromList . take l) $ take l $ iterate (0:) (d:repeat 0)
		dps = vFromList $ map ((\x -> x - fc) . f) $ map (center +) ds
		dms = vFromList $ map ((\x -> fc - x) . f) $ map (center -) ds

-- |Getting local gradient of a multivariate normal distribution
-- at point x.
--
-- Useful identies:
-- d/dS(det(S)) = (1/det(S)) (S^-1)^T
-- d/dA(S(A)^-1) = -(S(A)^-1)(d/dA(S(A)))(S(A)^-1)
-- d/dq(A(q)^TA(q)) = (dA/dq)A + A^TdA/dq
-- d/dD(D+uv^T) = I
-- d/du(D+uv^T) = v^T
-- d/dv(D+uv^T) = d/dv(u^Tv)^T = (d/dv(u^Tv))^T=u
dnAtX :: Vec -> N -> Vec
dnAtX x (N mu a) = numDerivative evalAtX v0
	where
		k = vLength x
		v0 = vFromList $ vToList mu ++ aToList a
		evalAtX v = m / sqrtDetS * exponent
			where
				m = (2 * pi) ** (negate $ fromIntegral k / 2)
				DN mu1 a1 = dnFromSplitted v
				a1' = invertA a1
				a1'T = transposeA a1'
				xmu = x - mu1
				a1'Txmu = matrMultA a1' xmu
				a1'Ta1'xmu = matrMultA a1'T a1'Txmu
				toExp = (/2) $ negate $ vInner xmu a1'Ta1'xmu
				exponent = exp toExp
				deta1 = determinantA a1
				sqrtDetS = abs deta1

dnFromSplitted :: Vec -> DN
dnFromSplitted v 
	| mod vl 4 /= 0 = error "invalid length at dnFromSplitted"
	| otherwise = DN mu a
	where
		vl = vLength v
		k = div vl 4
		mu = vFromList $ take k $ vToList v
		a = aFromList mu $ drop k $ vToList v

aToList :: A -> [FLOAT]
aToList (A d u v) = concatMap vToList [d,u,v]

aFromList :: Vec -> [FLOAT] -> A
aFromList v xs = A (vFromList dx) (vFromList ux) vx
	where
		l = vLength v
		(dx,xs') = splitAt l xs
		(ux,xs'') = splitAt l xs'
		vx = vFromList $ take l xs''

matrMultS :: A -> Vec -> Vec
matrMultS a = matrMultA (transposeA a) . matrMultA a

updateN :: FLOAT -> Vec -> N -> N
updateN nu dnVec n = N mu' a'
	where
		dn = dnFromSplitted dnVec
		mu' = nMu n + vScale nu (dnDMu dn)
		A d u v = nA n
		A dd du dv = dnDA dn
		a' = A
			(d + vScale nu dd)
			(u + vScale nu du)
			(v + vScale nu dv)

startN :: (Monad m, Draw m) => Vec -> m N
startN mu = do
	u <- drawStdNormalVec mu
	v <- drawStdNormalVec mu
	d <- drawStdNormalVec mu
	let	a = A d v u
	return $ N mu a

drawSample :: (MonadIO m, Draw m) => N -> m Vec
drawSample n = do
	x <- drawStdNormalVec $ nMu n
--	liftIO $ putStrLn $ "drawn stdnormal "++show x
	let	y = nMu n + matrMultA (nA n) x
--	liftIO $ putStrLn $ "transformed into "++show y
	return y

data Fisher = Fisher
	{ fisherN :: Double
	, gradients :: [Vec]
	}

conjugateGradient :: (Vec -> Vec) -> Vec -> Vec
conjugateGradient matMul b = cg r0 r0 x0
	where
		x0 = b
		r0 = b - matMul x0
		cg p r x
			| vL1 r < 1e-5 = x
			| vL1 r' < 1e-5 = x'
			| otherwise = cg p' r' x'
			where
				r2 = vInner r r
				ap = matMul p
				pAp = vInner p ap
				alpha = r2 / pAp
				x' = x + vScale alpha p
				r' = r - vScale alpha ap
				beta = vInner r' r' / r2
				p' = r' + vScale beta p

mulByFisherInverse :: Fisher -> Vec -> Vec
mulByFisherInverse m b = conjugateGradient multF b
	where
		fvecs = gradients m
		scale = 1 / fisherN m
		multF v = vScale (scale ^ 2) v + List.foldl1 (+)
			[vScale (scale * d) g | g <- fvecs, let d = vInner g v]

class Draw m where
	-- |Draw normally distributed (N(0, 1)) vector given another vector as a template.
	drawStdNormalVec :: Vec -> m Vec

        -- |"Round" the sample - might be handy for some interesting domains.
        -- For example, a part of the sample can be 1-from-N, so you may
        -- choose exactly one and zero out others.
        -- By default, provided here, does nothing.
        roundDrawnSample :: Applicative m => Vec -> m Vec
        roundDrawnSample = pure

-- |Approximate Natural Evolution Strategy.
--
-- Lambda is a population size. StartMu is an initial
-- averages. S is a state to evaluate against.
-- And eval is a State action to evaluate a sample. 
aNES :: (MonadIO m, Draw m) => Int -> FLOAT -> Vec -> (Vec -> m FLOAT) -> m Bool -> m Vec
aNES lambda nu startMu eval stop = do
	n <- startN startMu
	loop n
	where
		ilambda = 1.0 / fromIntegral lambda
		loop n = do
			--liftIO $ putStrLn $ "current n "++show n
			done <- stop
			if done
				then return (nMu n)
				else contLoop n
		drawAssess n = do
			sample <- drawSample n
			score <- eval sample
			let	grad = dnAtX sample n
			--liftIO $ putStrLn $ "sample "++show sample++", score "++show score++", grad "++show grad
			return (score, grad)
		contLoop n = do
			scoredGrads <- fmap (zip (map ((^20) . (*ilambda)) [1..]) . map snd .
				List.sort) $ mapM (const $ drawAssess n) [1..lambda]
			let	matr = map snd scoredGrads
				grad = foldr1 (+) $ map (uncurry vScale) scoredGrads
				ngrad = mulByFisherInverse (Fisher (fromIntegral lambda) matr) grad
				n' = updateN nu ngrad n
			--liftIO $ putStrLn $ "matr "++show matr
			--liftIO $ putStrLn $ "grd "++show grad
			--liftIO $ putStrLn $ "ngrad "++show ngrad
			loop n'

genNormals :: StdGen -> [FLOAT]
genNormals sg = transform rs
	where
		rs = map (\x -> x * 2 - 1) $ randoms sg :: [FLOAT]
		transform (u:v:uvs)
			| s == 0 || s >= 1 = transform uvs
			| otherwise = (ms * u) : (ms * v) : transform uvs
			where
				s = u^2 + v^2
				ms = sqrt $ negate $ 2 * log s / s


