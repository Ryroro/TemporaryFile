import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x xs = snd $ head $ filter (\(a, b) -> a == x) xs

checkSat :: BDD -> Env -> Bool
checkSat (node, bnodes) env = checkSat' node bnodes 
  where checkSat' index bnodes
          | index == 1 = True
          | index == 0 = False
          | otherwise =
              case lookUp i env of
                True -> checkSat' r bnodes
                False -> checkSat' l bnodes
          where (i,l,r) = lookUp index bnodes

--sat :: BDD -> [[(Index, Bool)]]
sat bdd@(node, bnodes) = filter (checkSat bdd) allPoss 
  where allPoss = [zipWith (\i b -> (i, b)) allNodes bs| bs <- allBs]
        allBs = genAllPoss (length allNodes)
        allNodes = allNodes' bnodes []

allNodes' [] stack = sort stack
allNodes' ((root, (i, l, r)):xs) stack
  | i `elem` stack = allNodes' xs stack
  | otherwise = allNodes' xs (i : stack)

genAllPoss n = init (map (map (\x -> if x == 1 then True else False)) [toBase i n | i <- [0..2^n]])
toBase x n = toBase' x n []
          where toBase' _ 0 stack = stack
                toBase' x n stack = toBase' (x `div` 2) (n-1) ((x `mod` 2) : stack)
        
------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b)) = case b of
                            True -> Prim False
                            False -> Prim True
simplify (Or (Prim a) (Prim b)) = case a == False && b == False of
                                    True -> Prim False
                                    False -> Prim True
simplify (And (Prim a) (Prim b)) = case a == True && b == True of
                                    True -> Prim True
                                    False -> Prim False
simplify a = a  

restrict :: BExp -> Index -> Bool -> BExp
restrict bExp index b = restrict' bExp index b
  where restrict' bExp index b =
          case bExp of
            Or exp1 exp2 -> simplify $ Or (restrict' exp1 index b) (restrict' exp2 index b)
            And exp1 exp2 -> simplify $ And (restrict' exp1 index b) (restrict' exp2 index b)
            Not exp -> simplify $ Not (restrict' exp index b)
            IdRef i -> case i == index of
                        True -> Prim b
                        False -> IdRef i
            Prim b -> Prim b

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
--buildBDD :: BExp -> [Index] -> BDD
buildBDD indexes = buildBDD' 0 0
  where buildBDD' curIndex nlayer = curList ++ buildBDD' (curIndex+1) (nlayer+1) 
          where curList = [((2^nlayer)+times, (indexes !! curIndex, 2^nlayer+times, 2^nlayer+times+1)) | times <- [0..(2^nlayer-1)]]
        {-                           
        n = length indexes
        finalStack = [(i,(-1, -1, -1)) | i <- indexes]
        numIndex = length indexes
        allBPoss = genAllPoss numIndex
        allSitu = [zipWith (\i b -> (i, b)) indexes bs| bs <- allBPoss]
        
        allRes = [genResEnvPair bExp aSitu | aSitu <- allSitu]
        genResEnvPair bExp aSitu = case eval bExp aSitu of
                                     Prim True -> [True, aSitu]
                                     Prim False -> [False, aSitu]
        eval bExp [] = bExp
        eval bExp ((i, b):ibs) = eval (restrict bExp i b) ibs 
-}
-- type BDDNode =  (NodeId, (Index, NodeId, NodeId))

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' 
  = undefined

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


