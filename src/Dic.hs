module Dict where

import Cp
import Exp
import List

type Dict = Exp String String

{-
• dic rd — procurar traduc¸oes para uma determinada palavra ˜
• dic in — inserir palavras novas (palavra e traduc¸ao) ˜
• dic imp — importar dicionarios fdo formato “lista de pares palavra-traduc¸ ´ ao” ˜
• dic exp — exportar dicionarios para o formato “lista de pares palavra-traduc¸ ´ ao”. 
-}

-- Codigo fornecido


d = [("ABA", ["BRIM"]),("ABALO", ["SHOCK"]),("AMIGO", ["FRIEND"]),("AMOR", ["LOVE"]),("MEDO", ["FEAR"]),("MUDO", ["DUMB", "MUTE"])]

e = [("PE",["FOOT"]),("PEDRA", ["STONE"]),("POBRE", ["POOR"]),("PODRE", ["ROTTEN"])]

f = d ++ e
-- Funcao de representacao de um dicionario:
dic_imp :: [(String, [String ])] -> Dict
dic_imp = Term "" . map (bmap id singl) . untar . discollect


--discollect
{-
discollect :: (Ord b, Ord a) => [(b, [a ])] -> [(b, a)]
discollect [] = []
discollect((a,[]):l) = discollect l
discollect ((a,b:c):l) = (a,b) : discollect((a,c):l) 
-}

discollect :: (Ord b, Ord a) => [(b, [a ])] -> [(b, a)]
discollect = cataList g where
	g = either (const []) (uncurry (++) . (f >< id))
		where f (a,l) = map (split (const a)  id) l

 
dic_search :: String -> Dict -> [Dict]
dic_search [] d  =  [d]
dic_search s (Var v)  = []
dic_search s (Term o l)  = if (maybe False id (fmap ((==) x . p1) (uncons l)))  then  concat . map (dic_search s) l else []

	



dic_exp :: Dict -> [(String, [String ])]
dic_exp = collect . tar
tar = cataExp g where 
g = either (singl . (split (const "") id)) (f)
	where f (a,b) = map ((++) a >< id) (concat b) 

--Traduçao -> Palavra
{-
dic_in :: String -> String -> Dict -> Dict
dic_in t p (Var v) = dic_imp[("",[v]),(t,[p])]
dic_in t p (Term o l) | (p == []) = Term o ((Var t):l)
		      | (o == []) = (Term o (dic_in t (tail p) (Term (singl (head p)) []):l) 
		      | (heap p == head o) (Term o (dic_in t (tail p) (Term (singl (head p)) []):l) -}



dic_rd :: String -> Dict -> [String]
dic_rd s d = while p loopBody exit (s,Just d)
	where p(a,b) = (a /= [] && isJust b)
	      exit(_,Nothing) = []
	      exit ([],Just (Term o l)) =  ((map (either id p1) . filter (isLeft) . map (outExp)) l)	

loopBody (s,Just (Var v)) = (s,Nothing)
loopBody(s,Just (Term o l)) | (o == []) = (s, termLsearch s l)
		            | (head s == head o) = (tail s,Just (Term o l))	
	              	    | otherwise = (s, termLsearch s l) 

		
	
termLsearch s ((Term o l):xs) = if (head s == head o) then Just (Term o l) else termLsearch s xs
termLsearch s (_:xs) = termLsearch s xs
termLsearch _ [] = Nothing 

isJust (Just _) = True
isJust _ = False

isLeft (Left _) = True
isLeft _ = False


{-

isJust (Just _) = True
isJust _ = False

isLeft (Left _) = True
isLeft _ = False
{-dic_rd  =  (either (singl . Nothing) (if (uncurry (==) . (head >< head)) then concat.p2 else (singl . Nothing) ) ) . uncurry -}
-}



{-
g Left("A") = ("","A")
g Right("A",[Var "crlh"]]) = g ("A",[("","crlh")]) = [("A",crlh)] // f (a,b) = map (cons b.p1 const a) b
				
g Right("A",[(Term "B" [Var "AB"]), (Var"oos")]) =  g Right("A",[("B","AB") , ("","oos")]) =  
	
//baseExp f g h = f -|- (g >< map h)
//expLeaves :: Exp a b -> [a]
//expLeaves = cataExp (either singl (concat . p2))
baseExp f g h = f -|- (g >< map h)

recExp x = baseExp id id x

cataExp (either singl (concat . p2)) = (either singl (concat . p2)) . recExp (cataExp (either singl (concat . p2))) . outExp (Var "eu")

= (either singl (concat . p2)) . recExp (cataExp (either singl (concat . p2))) Left ("eu") 
= (either singl (concat . p2)) . recExp (cataExp (either singl (concat . p2))) Left ("eu") 
f -|- (g >< map h)
= (either singl (concat . p2)) . id -|- (id >< map (cataExp (either singl (concat . p2)))) Left ("eu") 
= ["eu"]

g Left("l") = ["l"]
g Right ("A", [Var "l"]

Term "A" [Var "Eu"]
(either singl (concat . p2)) . id -|- (id >< map (cataExp (either singl (concat . p2)))) Right ("A", [Var "Eu"])

(either singl (concat . p2)) . Right ("A",     map (cataExp (either singl (concat . p2))) [Var "Eu"])

(either singl (concat . p2)) . Right ("A",  (cataExp (either singl (concat . p2))) [Var "Eu"])

(either singl (concat . p2)) . Right ("A",  ["Eu"]) -}		





