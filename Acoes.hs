
module Acoes where

type Entidade = (Int, String)
type Entidades = [Entidade]

type Split = (Int, Int, Float)
type Splits = [Split]

type Acao = (Int, Float, Int)
type Acoes = [Acao]


--bdAcoes::Acoes
--bdAcoes = 
--	[(100, 1.50, 0), (100, 2.79, 0),
--	 (80, 1.37, 0), (100, 2.00, 0)]

--
--Ler as ofertas das acoes
leOferta:: IO(Int, Float, Int)
leOferta = do
	putStr "Digite um valor inteiro: "
	n <- getLine
	putStr "Digite um valor flot: "
	v <- getLine
	putStr "Digite um valor inteiro: "
	i <- getLine
	return (read n, read v, read i)


leInt :: IO(Int)
leInt = do
	putStr "Digite um valor inteiro: "
	n <- getLine
	return (read n)

leFloat :: IO(Float)
leFloat = do
	putStr "Digite um valor float: "
	f <- getLine
	return (read f)

leString :: IO(String)
leString = do
	putStr "Digite uma string: "
	s <- getLine
	return (read s)

--
--Funcao para retornar o numero de acoes de uma tupla
fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

-- v valor das acoes    k numero de acoes
efetivacaoPossivel :: Acoes -> Int -> Float -> Bool
efetivacaoPossivel [] k v = False
efetivacaoPossivel (x:xs) k v =
        if ((k <= (fst' x)) && (v == (snd' x)))
         then 
         	True
         else efetivacaoPossivel xs k v

--Recebe um lance/oferta (0/1) 
--para popular uma lista de split [split]
--operacao :: Int -> Float -> Int -> Int -> Bool

while test action = do
	val <- test
	if val then do {action; while test action}
		else return()


--popularAcoes :: Int -> Float -> Int -> Bool
--popularAcoes nAcoes valor idUser = 
--	if(bdAcoes <-)

main :: IO ()
main = do 
	n1 <- leOferta
	print (n1)

--lance ::

--oferta ::

--executaMercado :: 