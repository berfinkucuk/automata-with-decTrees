module Dta where
    import qualified Data.IntMap as IntMap
    import System.IO as SysIO
    import Data.List.Split as Split
    import Data.String.Unicode as Unicode
    --import qualified Data.Hex as Hex

    data UniClass = Digit | Lower | Upper | MathSymbol | Symbol | Undefined  deriving (Show, Eq)
    data DecTree uc = Leaf uc
                    | Cmp{ pivot:: Char
                          , lt :: DecTree uc
                          , eql :: uc--character
                          , gt :: DecTree uc
                      } deriving (Show,Eq)
    data Token = Ident
                | AlphaNum
                | Integ
                | Alphabet
                | Operation
                | Other deriving (Show)

    data DecTreeStr uc = LeafS uc              --this data type will be deleted when
                    | CmpS{ pivotS:: String     --I figure out how to convert hexadecimal number
                          , ltS :: DecTreeStr uc  --to unicode char
                          , eqlS :: uc--character
                          , gtS :: DecTreeStr uc
                          } deriving (Show,Eq)



    type NodeId = Int
    data Node t = Node { children:: DecTree NodeId
                        ,final :: [t] --to control if it is final state or not
                      }deriving (Show,Eq)

    type Automaton t = IntMap.IntMap( Node t)

    --It takes boundaries (branches) as a paramter and return a decision tree
    --toDecTree :: [([Char],UniClass)] -> DecTreeStr uc


    --first I will try to create a simple tree which takes
    toDecTree :: [(Char,UniClass)]  -> DecTree UniClass
    toDecTree arr = toDecTreeHelper arr 0 ((fromIntegral$ length arr) - 1)
      where
        toDecTreeHelper :: [(Char,UniClass)] -> Int -> Int -> DecTree UniClass
        toDecTreeHelper arr bottom top = if ( bottom >= top)
                                  then (Leaf (snd (arr !! top)))
                                  else Cmp (fst$ (arr !! mid )) (toDecTreeHelper arr bottom (mid - 1)) (snd (arr !! mid)) (toDecTreeHelper arr (mid + 1) top)
                          where mid = ceiling (fromIntegral $ (bottom + top) `div` 2)


    --so at the end this function returns longest match
    --actually to give the correct result loop function must be returned
    --once more : to do that I put whitespace at the end of the char
    --HOWEVER! please check it for other condition which may not apply
    run:: Automaton Token -> NodeId -> [Char] -> ([Token],[Char])
    run aut i cs = loop [] i cs
      where
       loop :: [Token] -> NodeId -> [Char] -> ([Token],[Char])
       loop ts i cs = case (IntMap.lookup i aut, cs) of-- so we are searching in our auto int map if not found (*1)
           (Just (Node dt ts'), c:cs) | null ts || not (null ts') -> --if ts is null OR ts' is not null
             loop ts' (decide c dt) cs  --here it goes next state which is determined by decide function
           _ -> (ts, cs) --(*1) then it just returns parameters taken



    decide ::  Char -> DecTree uc -> uc --  String also will be changed to Char
    decide x (Leaf uc) = uc
    decide x ( Cmp p lt eql gt) =
      case (compare x p) of
           EQ -> eql
           GT -> decide x gt
           LT -> decide x lt


    --so after that point you should splitOn ";" and
    splitLine ::  [String] ->  [[[Char]]]
    splitLine arr = map( splitOn ";" ) arr

    deleteNth :: Int -> [a] -> [a]
    deleteNth _ []     = []
    deleteNth i (a:as)
      | i == 0    = as
      | otherwise = a : deleteNth (i-1) as

    readTxtUnicode ::  Monad m => String ->m [[[Char]]]
    readTxtUnicode x = do
      let theLines = (lines x)
      let temp = splitLine theLines
      let fromTuple = map (fst.(splitAt 3)) temp --[(["0000","<control>","Cc"],(_)], (a,b)] -> [["0000","<control>","Cc"]], a ]
      let classes = map (deleteNth 1) fromTuple
      return classes

    convert :: [[Char]] -> ([Char],UniClass)
    convert [x,y] = case y of "Lu" -> (x,Upper)
                              "Ll" -> (x,Lower)
                              "Nd" -> (x,Digit)
                              "Sm" -> (x,MathSymbol)
                              "Sc" -> (x,Symbol)
                              _ -> (x,Undefined)

    toUnicodeClass ::  [[[Char]]] -> [([Char],UniClass)]
    toUnicodeClass arr = map (convert) arr

    --this method extract branches (boundaries)  --YOU CAN MERGE WITH HELPER AS IN RUN FUNC
    extractBranches :: [([Char],UniClass)] -> [([Char],UniClass)]
    extractBranches arr = helperExtract 0 ((fromIntegral (length arr)) - 1) Undefined arr

    --helperExtract first length(last) Token(lastOne)
    helperExtract :: Int -> Int -> UniClass -> [([Char],UniClass)] -> [([Char],UniClass)]
    helperExtract _ _ _ [] = []
    helperExtract _ _ _ [x] = [x]    --returns last element?
    helperExtract 0 y _ (x:xs) = x : ( helperExtract 1 y (snd x)  xs)
    helperExtract z y uni (x:t:xs) = if uni /= (snd x)
                                     then x : (helperExtract (z+1) y (snd x) xs)
                                     else if (snd x) /= (snd t)
                                       then x : (helperExtract (z+1) y (snd x) xs)
                                     else (helperExtract (z+1) y uni xs)

    --First lets create a decision trees for nodes which is added to automat
    dt0:: DecTree NodeId
    dt0 = Cmp '8' (Cmp '4' (Leaf 1) 1 (Leaf 2)) 2 (Leaf 5)

    dt1::DecTree NodeId--assuming at most 9 is entered
    dt1 = Cmp '4' (Leaf 1) 1 (Cmp '8' (Leaf 3) 3 (Leaf 4))

    dt2:: DecTree NodeId
    dt2 = Cmp '5' (Leaf 3) 2 (Cmp '8' (Leaf 2) 2 (Leaf 4))

    dt3:: DecTree NodeId
    dt3 = Cmp '8' (Leaf 3) 3 (Leaf 4)

    dt4::DecTree NodeId
    dt4 = Cmp '4'(Leaf 4) 4 (Leaf 4) --here when it is other state it will loop

    dt5:: DecTree NodeId
    dt5 = Cmp '9' (Leaf 4) 5 (Leaf 4)

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
