{-# LANGUAGE DeriveFunctor #-}
module Dta where
    import qualified Data.IntMap as IntMap
    import System.IO as SysIO
    import Data.List.Split as Split
    import Data.String.Unicode as Unicode
    import Control.Monad.State as MonadState
    import qualified Data.Hex as Hex
    import qualified Text.XML.HXT.DOM.Util as Text
    import Data.Char as Char
    import Data.Function as Function
    import Data.Foldable as Foldable
    import Data.IntSet as IntSet
    import Data.Maybe as Maybe
    import Data.List as List
    --import Data.IntMap.Internal as ı

    data UniClass = Digit | Lower | Upper | MatSymbol | Symbol | Undefined  deriving (Show, Eq)

    data DecTree uc = Leaf uc
                    | Cmp{ pivot:: Char
                          , lt :: DecTree uc
                          , eql :: uc--character or nodeid in our cases
                          , gt :: DecTree uc
                      } deriving (Show,Eq,Functor)

    data Token = Ident
                | AlphaNum
                | Integ
                | Alphabet
                | Operation
                | Other deriving (Show, Eq)


    type NodeId = Int
    data Node t = Node { children:: DecTree [NodeId] --here it supposed to be IntSet instead I thought IntSet.toList can be used?
                        ,final :: [t] --to control if it is final state or not
                      }deriving (Show,Eq,Functor)

    data AltList a b = Singleton a
                      | Cons a b (AltList a b) deriving (Show,Functor)

    type Automaton t = IntMap.IntMap( Node t)

    type Freq = Int

    type Statistics = IntMap.IntMap(IntMap.IntMap Freq)

    type Stats = [NodeId]

    run:: Automaton Token -> [NodeId] -> [Char] -> State Stats ([Token],[Char])
    run aut is cs = loop [] is cs
      where
        loop :: [Token] -> [NodeId] -> [Char] -> State Stats ([Token],[Char])
        loop ts is cs = do
          -- modify (i:)    // TO DO you have to deal with state monad
          --List.map (\i -> modify(i:)) is            --ask what to do for statistics !!?
          let nodes = Maybe.mapMaybe (\i -> IntMap.lookup i aut) is
          let ts' = List.nub $ concat $ Prelude.map final nodes --tokens we can return -dublicates are removed
          let dts = Prelude.map children nodes

          case (nodes, cs) of-- so we are searching in our auto int map if not found (*1)
           ( _, []) -> return (ts', [])
           ([], cs)  -> return (ts, cs)
           _ ->  loop ts' (concat $ (Prelude.map (decide (head cs)) (Prelude.map children nodes)) ) (tail cs)  --here it goes next state which is determined by decide function

       --this method collects statistical information about how often which branches are taken.
    collectStats :: Stats -> Statistics
    collectStats ls = collect (IntMap.fromList []) ls  --this may give error try intmap(intmap then
      where
        collect:: Statistics -> Stats -> Statistics
        collect statMap [] = statMap --if no element left to add statMap
        collect statMap [z] = statMap
        collect statMap (x:xs )= do  --then list has at least two element
          case (IntMap.lookup (head xs) statMap) of
            (Just sndMap  ) -> collect (IntMap.insert (head xs) (IntMap.insertWith (+) x 1 sndMap) statMap) xs
            _ -> collect (IntMap.insert (head xs) (IntMap.fromList [(x,1)]) statMap) xs
{-
    --this method combine run and collectStats functions
    -- so it collect statistics while running automata
    runAndCollectStats:: Automaton Token -> NodeId -> [Char] -> Statistics
    runAndCollectStats aut nodeid str = collectStats (snd ( runState (run aut nodeid str) [] ))
-}
    calcFreq:: Statistics -> NodeId -> NodeId -> Freq
    calcFreq st fr to =  f to
      where
        f':: NodeId -> IntMap.IntMap Freq -> Freq
        f' i mapf = do
          case (IntMap.lookup i mapf) of
            (Just freq) -> freq
            _ -> 0
        f :: NodeId -> Freq
        f j = do
          case (IntMap.lookup fr st) of
            (Just  sndMap) -> f' j sndMap
            _ -> 0

    -- TO DO :: remove redundancy below by using newly added the calcFreq function above
    cost :: Statistics -> NodeId -> DecTree NodeId -> Int
    cost st i dt = loop dt 0
      where
        f':: NodeId -> IntMap.IntMap Freq -> Freq
        f' i mapf = do
          case (IntMap.lookup i mapf) of
            (Just freq) -> freq
            _ -> 0
        f :: NodeId -> Freq
        f j = do
          case (IntMap.lookup i st) of
            (Just  sndMap) -> f' j sndMap
            _ -> 0
        loop:: DecTree NodeId -> Int -> Int
        loop (Leaf j) k = (f j ) * k
        loop (Cmp p lt eql gt) k = ((f eql) * (k+1)) + (loop lt (k + 1)) + loop gt (k + 1) -- it is assumed that eql is at the same level with the lt gt




    --stats1 :: Statistics
    --stats1 = runAndCollectStats aut1 0 "123472583"


    decide ::  Char -> DecTree uc -> uc
    decide x (Leaf uc) = uc
    decide x ( Cmp p lt eql gt) =
      case (compare x p) of
           EQ -> eql
           GT -> decide x gt
           LT -> decide x lt


    --so after that point you should splitOn ";" and
    splitLine ::  [String] ->  [[[Char]]]
    splitLine arr = Prelude.map( splitOn ";" ) arr

    deleteNth :: Int -> [a] -> [a]
    deleteNth _ []     = []
    deleteNth i (a:as)
      | i == 0    = as
      | otherwise = a : deleteNth (i-1) as

    readTxtUnicode ::  Monad m => String ->m [[[Char]]]
    readTxtUnicode x = do
      let theLines = (lines x)
      let temp = splitLine theLines
      let fromTuple = Prelude.map (fst.(splitAt 3)) temp --[(["0000","<control>","Cc"],(_)], (a,b)] -> [["0000","<control>","Cc"]], a ]
      let classes = Prelude.map (deleteNth 1) fromTuple
      return classes

    --here string is converted to its Int
    convert :: [[Char]] -> (Int,UniClass)
    convert [x,y] = case y of "Lu" -> (Text.hexStringToInt x,Upper)
                              "Ll" -> (Text.hexStringToInt x,Lower)
                              "Nd" -> (Text.hexStringToInt x,Digit)
                              "Sm" -> (Text.hexStringToInt x,MatSymbol)
                              "Sc" -> (Text.hexStringToInt x,Symbol)
                              _ -> (Text.hexStringToInt x,Undefined)

    toUnicodeClass ::  [[[Char]]] -> [(Int,UniClass)]
    toUnicodeClass arr = Prelude.map (convert) arr

    --this method extract branches (boundaries)  --YOU CAN MERGE WITH HELPER AS IN RUN FUNC
    extractBranches :: [(Int,UniClass)] -> [(Int,UniClass)]
    extractBranches arr = helperExtract 0 ((fromIntegral (length arr)) - 1) Undefined arr

    --helperExtract first length(last) Token(lastOne)
    helperExtract :: Int -> Int -> UniClass -> [(Int,UniClass)] -> [(Int,UniClass)]
    helperExtract _ _ _ [] = []
    helperExtract _ _ _ [x] = [x]    --returns last element?
    helperExtract 0 y _ (x:xs) = x : ( helperExtract 1 y (snd x)  xs)
    helperExtract z y uni (x:t:xs) = if uni /= (snd x)
                                     then x : (helperExtract (z+1) y (snd x) xs)
                                     else if (snd x) /= (snd t)
                                       then x : (helperExtract (z+1) y (snd x) xs)
                                     else (helperExtract (z+1) y uni xs)

    --It takes boundaries (branches) as a paramter and return a decision tree
    --for unicode character classess
    toDecTree :: [(Char,UniClass)]  -> DecTree UniClass
    toDecTree arr = toDecTreeHelper arr 0 ((fromIntegral$ length arr) - 1)
          where
            toDecTreeHelper :: [(Char,UniClass)] -> Int -> Int -> DecTree UniClass
            toDecTreeHelper arr bottom top = if ( bottom >= top)
                                            then (Leaf (snd (arr !! top)))
                                            else Cmp (fst$ (arr !! mid )) (toDecTreeHelper arr bottom (mid - 1)) (snd (arr !! mid)) (toDecTreeHelper arr (mid + 1) top)
                                             where mid = ceiling (fromIntegral $ (bottom + top) `div` 2)



    join':: AltList a b -> b -> AltList a b -> AltList a b
    join' (Singleton a) b l = Cons a b l
    join' (Cons a1 b1 l1) b l = Cons a1 b1 (join' l1 b l)

    --here boundary p is converted to Int
    --for now AltList will be created without freqs but just NodeId
    -- !!basically i need freqs of each node so another attribute is added in the function below
    toAltList :: DecTree NodeId -> AltList (DecTree NodeId) (Int, NodeId)
    toAltList (Leaf i) = Singleton (Leaf i )
    toAltList (Cmp p lt eql gt)  = (join' l1 ((Char.ord p),eql) l2)
      where
       l1 = toAltList lt
       l2 = toAltList gt


    type AltListFreq = AltList (DecTree NodeId, Freq) (Int, NodeId, Freq)
    --AltList with freqs of each node from current node
    freqAltList:: Statistics -> NodeId -> AltList (DecTree NodeId) (Int, NodeId) -> AltListFreq
    freqAltList st i (Singleton (Leaf nodeid) ) = Singleton (Leaf nodeid, (calcFreq st i nodeid))
    freqAltList st i (Cons (Leaf id1) (p, id2) altLs ) = (Cons (Leaf id1, (calcFreq st i id1) ) (p, id2, (calcFreq st i id2)) (freqAltList st i altLs))


    type AltListid = AltList (DecTree NodeId) (Int, NodeId)
    -- Produce a list of worklists where one pair of the original
    -- worklist forms a new tree.
    cands ::  Statistics -> NodeId -> AltListid -> [(Int, AltListid)]
    cands _ _ (Singleton x) = [] --another statament between
    cands st i (Cons dta' (p',nodei') (Cons dtb' (p2', nodei2') aLs) ) = (cost st i dt', w2) : Prelude.map  (\ (c,ts) -> (c, Cons dta' (p',nodei') ts )  ) (cands st i  (Cons dtb' (p2', nodei2') aLs) )
      where
        dt'= Cmp (Char.chr p') dta' nodei' dtb'
        w2 = Cons dt' (p2',nodei2') aLs
    cands st i (Cons dta (p,nodei) (Singleton dtb)) = [(cost st i dt, w1)]
      where
      dt = Cmp (Char.chr p) dta nodei dtb
      w1 = Singleton dt

    step :: Statistics -> NodeId -> AltListid -> Maybe (AltListid)
    step st i aLs@(Cons _ _ _) = Just $ snd $ minimumBy (compare `on` fst) $ cands st i aLs
    step _ _ _ = Nothing


    -- Repeat a function until it returns Nothing.
    trampoline :: (a -> b -> c -> Maybe c) -> a-> b -> c -> c
    trampoline f a b c = case f a b c of
      Nothing -> c
      Just a' -> trampoline f a b a'

    --test = trampoline step stats1 0 (toAltList dt0)
    data AutomatonWithInit t =  AutWithInit NodeId (Automaton t) deriving Show

    alternation:: AutomatonWithInit t -> AutomatonWithInit t -> AutomatonWithInit t
    alternation (AutWithInit i1 a1) (AutWithInit i2 a2) = AutWithInit i0 a0
      where
        n = maximum (IntMap.keys a1)
        i2' = i2 + n + 2
        a2' = IntMap.mapKeysMonotonic (+ (n+2)) a2
        f key node = modifyStateIds node (n+2)
        a2'' = IntMap.mapWithKey f a2'
        i0 = n + 1 --new initial State
        t1 = IntMap.lookup i1 a1
        t2 = IntMap.lookup i2' a2''
        a0 = IntMap.unionsWith commonDecTree [maybe IntMap.empty (IntMap.singleton i0) t1, --common decitiontrees
                                maybe IntMap.empty (IntMap.singleton i0) t2,
                                a1,
                                a2'']

    --TO DO :: check this method
    --method for common decisiontrees
    --it takes two nodes and merge their decision trees
    commonDecTree :: Node t -> Node t -> Node t
    commonDecTree (Node dt1 lsF) (Node dt2 lsF2) = Node (mergeTrees dt1 dt2) (concat [lsF, lsF2])
      where
        mergeTrees :: DecTree [NodeId] -> DecTree [NodeId] -> DecTree [NodeId]
        mergeTrees (Cmp p ltT eqlT gtT ) dt2@(Cmp p2 lt2 eq2 gt2) = case (compare (Char.ord p) (Char.ord $ pivot dt2)) of
          EQ -> Cmp p (mergeTrees ltT lt2) ( (++) eqlT eq2)  (mergeTrees gtT gt2)
          GT -> Cmp p ltT eqlT (mergeTrees gtT dt2)
          LT -> Cmp p (mergeTrees ltT dt2) eqlT gtT
        mergeTrees (Leaf k) (dt2) = fmap (++ k) dt2
        mergeTrees dt1@(Cmp p lt eql gt) ( Leaf k2) = fmap (++ k2) dt1 --this case is not possible since dt2 should have a pivots?

    --this function is used to modify (increase) stated ids of a decision tree belonged to a node
    modifyStateIds :: Node t -> Int -> Node t
    modifyStateIds (Node dt1 ls) x = Node (fmap (Prelude.map (+ x)) dt1) ls


    sequence' :: AutomatonWithInit t -> AutomatonWithInit t -> AutomatonWithInit t
    sequence' (AutWithInit i1 a1) (AutWithInit i2 a2) = AutWithInit i1 a0
      where
        n = maximum (IntMap.keys a1)
        i2' = i2 + n --initial state of aut2 will be discarded
        a2' = IntMap.mapKeysMonotonic (+ (n)) a2
        f key node = modifyStateIds node (n) --updating state(inside dts) of second aut2
        a2'' = IntMap.mapWithKey f a2'
        t2 = IntMap.lookup i2' a2''
        f2 key node = insertTree node t2
        a1' = IntMap.mapWithKey f2 a1
        a0 = IntMap.unions [a1',
                            a2'
                            ]


    --I am not sure about eql part of the dt
    insertTree :: Node t -> Maybe (Node t) -> Node t
    insertTree node1 Nothing = node1
    insertTree node1@(Node dt1 []) (Just(Node dt2 lsF2)) = node1-- if node is not final [empty list] then do not add any tree
    insertTree (Node dt1 ls) (Just(Node dt2 lsF2)) =  Node (insert dt1 dt2) lsF2 --or concat of ls-lfF2?
      where
        insert:: DecTree [NodeId] -> DecTree [NodeId] -> DecTree [NodeId]
        insert (Leaf x) dt2 = dt2
        insert (Cmp p' lt' eql' gt') dt2 = Cmp p' (insert lt' dt2) eql' (insert gt' dt2)




    --First lets create a decision trees for nodes which is added to automata
    dt0:: DecTree [NodeId]
    dt0 = Cmp '8' (Cmp '4' (Leaf [1]) [1] (Leaf [2])) [2,3] (Leaf [5])

    dt1::DecTree [NodeId]--assuming at most 9 is entered
    dt1 = Cmp '4' (Leaf [1]) [1] (Cmp '8' (Leaf [3]) [3] (Leaf [4]))

    dt2:: DecTree [NodeId]
    dt2 = Cmp '5' (Leaf [3]) [2] (Cmp '8' (Leaf [2]) [2] (Leaf [4]))

    dt3:: DecTree [NodeId]
    dt3 = Cmp '8' (Leaf [3]) [3] (Leaf [4])

    dt4::DecTree [NodeId]
    dt4 = Cmp '4'(Leaf [4]) [4] (Leaf [4]) --here when it is other state it will loop

    dt5:: DecTree [NodeId]
    dt5 = Cmp '9' (Leaf [4]) [5] (Leaf [4])

    test23 = pivot dt0

    node0 = Node (dt0) []
    node1 = Node (dt1) [Integ]
    node2 = Node (dt2) [Alphabet]
    node3 = Node (dt3) [AlphaNum]
    node4 = Node (dt4) [Other]
    node5 = Node (dt5) [Operation]



    aut1::Automaton Token
    aut1 = IntMap.fromList[(0,node0)
                          ,(1,node1)
                          ,(2,node2)
                          ,(3,node3)
                          ,(4,node4)
                          ,(5,node5)]

    testx =  runState (run aut1 [0] "8") []

    --test toDecTree with simple dt
    aBoundary :: [(Char,UniClass)]
    aBoundary = [('0',Digit),('1',Lower),('2',Upper),('3',Symbol)]

    type DT = DecTree UniClass

    tree :: DT
    tree = Cmp '4' (Leaf Digit) Digit (Leaf Lower)

    --dec1
    dec1 :: DT
    dec1 = Leaf Digit
    --dec5te
    dec5 :: DT
    dec5 = Leaf Lower

    --dec4
    cmp4 = Cmp '4' dec1 Digit dec5
    dec4 :: DT
    dec4 = cmp4

    --dec9
    dec9 :: DT
    dec9 = Leaf Symbol

    --dec8
    cmp8 = Cmp '8' dec4 Lower dec9
    dec8 :: DT
    dec8 = cmp8
