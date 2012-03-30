import Control.Monad.Par
import System.Mem.StableName

data AbstCost = AC Float
data TimeCost = TC Float
data Resource = Seq | CPU | GPU | Dist
data KappaRange = KLo TimeCost | KUp TimeCost | KRange TimeCost TimeCost


data Name a b = IO (StableName (a -> Par b))

data Estimator = Est {
    init :: (),
    reportTime  :: AbstCost -> TimeCost -> (),
    predictTime :: AbstCost -> TimeCost
}

data ResourceImpl a b = RI {
    name :: Name a b,
    resourceType :: Resource,
    run :: a -> Par b,
    cost :: a -> AbstCost,
    kappa :: KappaRange
}

data ResourceImplSet a b = RISet {
    impls :: [ResourceImpl a b],
    estimator :: Estimator
}

-- I don't know what type this should actually be (probably not String...). 
-- I want a table that has each stable name associated with a ResourceImplSet
data LookupTable = String
data StateInfo = SI {
    funTable :: LookupTable
}

-- Should just take a name and arguments, ideally
-- For now, I guess I'll assume there's a ResourceImplSet passed in until I
-- understand where to put and how to get this from a stored state
oracleFork :: ResourceImplSet a b -> Name a b -> a -> Par b
oracleFork = undefined 
