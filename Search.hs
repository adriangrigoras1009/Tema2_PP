{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    stare :: s,
    actiune :: Maybe a,
    parinte :: Maybe (Node s a),
    adancime :: Int,
    cost :: Float,
    copii :: [Node s a]
    }

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    (Node stare actiune parinte adancime cost copii) == (Node stare1 actiune1 parinte1 adancime1 cost1 copii1) = stare == stare1

instance Ord s => Ord (Node s a) where
    (Node stare actiune parinte adancime cost copii) <= (Node stare1 actiune1 parinte1 adancime1 cost1 copii1) = stare <= stare1

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node stare actiune parinte adancime cost copii) = stare

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node stare actiune parinte adancime cost copii) = parinte

nodeDepth :: Node s a -> Int
nodeDepth (Node stare actiune parinte adancime cost copii) = adancime

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node stare actiune parinte adancime cost copii) = copii

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Node stare actiune parinte adancime cost copii) = cost

nodeAction :: Node s a -> Maybe a
nodeAction (Node stare actiune parinte adancime cost copii) = actiune

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = recursiv initialState Nothing Nothing [] 0

recursiv :: (ProblemState s a, Eq s) => s -> Maybe (Node s a) -> Maybe a -> [Node s a] -> Int -> Node s a
recursiv stare parinte actiune lista adancime
    | isGoal stare == True = (Node stare actiune parinte adancime (h stare) lista)
    | otherwise = Node stare actiune parinte adancime (h stare) (lista ++ (map (\(x,y) -> recursiv y (Just (Node stare actiune parinte adancime (h stare) lista)) (Just x) [] (adancime+1)) (successors stare)))

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = map (\(x,y) -> (Node y (Just x) (Just node) (adancime node + 1) (h y) [])) (filter (\(x,y) -> (not (y `elem` visited))) (successors (stare node)))

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith f node (adunare (h (stare node)) (adancime node)) frontier -- newFrontier


f :: Float -> Float -> Float
f float1 float2
    | float1 > float2 = float2
    | otherwise = float1

adunare :: Float -> Int -> Float
adunare float1 int1 = float1 + (fromIntegral int1)
{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl (\acc x -> insertSucc acc x) frontier (suitableSuccs node visited)--newFrontier

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier
    | isGoal (stare (fst (deleteFindMin frontier))) == True = fst (deleteFindMin frontier)
    | otherwise = astar' (S.insert (stare (fst (deleteFindMin frontier))) visited) (insertSuccs (fst (deleteFindMin frontier)) (snd (deleteFindMin frontier)) (S.insert (stare (fst (deleteFindMin frontier))) visited)) 
{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' (S.singleton (stare initialNode)) (PQ.singleton (initialNode) 0)  -- goalNode

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: (Eq s) => Node s a -> [(a, s)]
extractPath goalNode = reverse $ map (\node -> (fromJust $ actiune node, stare node)) nodes where 
    nodes = takeWhile functie $ iterate (\node -> fromJust $ parinte node) goalNode

functie :: (Eq s) => Node s a -> Bool
functie lista
    | parinte lista == Nothing = False
    | otherwise = True