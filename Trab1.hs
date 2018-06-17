type Navio = (Int, Int, Int, Int) -- (ID, Chegada, Partida, Carga)
type Berco = (Int, Int, Int) -- (ID, Abertura, Fechamento)
type InfoPorto = [[Int]] -- [[tn1b1,tn2b1,...][tn1b2,tn2b2,...]...]    
                         -- tn1b1 -> tempo de atendimento navio1 berco1
--type NaviosAlocadosBerco = (Berco, [Navio])

atendido :: Navio -> Berco -> InfoPorto -> Bool
atendido n b ip = vercheg n b  
                         && verpart n b ip
                         && ta n b ip /= 0

vercheg n b = cheg n >= aber b --Verifica se o inicio da JT do navio eh maior que o inicio da JT do berco
verpart n b ip = fech b >= said n b ip --verifica se o final da JT do berco eh maior que o inicio da JT do navio mais o tempo de atendimento
                     -- && part n > fech b 

--filaNavios ln = if null (tail ln)
--                   then ln
--                else if menor (head ln) ln 
--                        then head ln : filaNavios(tail ln)
--                     else filaNavios((tail ln ++ [head ln]))

--menor a ln = a == minimum ln

filaNavios' :: [Navio] -> [Navio]
filaNavios' ln = if null (tail ln)
                    then ln
                else if menor' (head tch) tch 
                        then head ln : filaNavios'(tail ln)
                    else filaNavios'((tail ln ++ [head ln]))
        where
            tch = [cheg x | x <- ln]


menor' a tch = a == minimum tch

tempoOcioso :: Berco -> (Berco, [Navio]) -> InfoPorto -> Int
tempoOcioso b la ip = if null (tail ln)
                         then fech b - said (ln!!0) b ip
                    else cheg (ln!!1) - said (ln!!0) b ip + tempoOcioso b (b, tail ln) ip
            where
                ln = (snd la)

bercoOcioso :: [Berco] -> [(Berco, [Navio])] -> InfoPorto -> Int
bercoOcioso lb lla ip = pos tm lto 
                where
                    tm = maximum lto
                    lto = [tempoOcioso b la ip | b <- lb, la <- lla, b == fst la]
                    --FALTA IMPLEMENTAR O CASO EM QUE DOIS BERCOS APRESENTAM SIMULTANEAMENTE O MAIOR TEMPO OCIOSO


--Funcao pos verifica a posicao de dado valor em um vetor, sendo 1 a posicao inicial.
pos x xs = if xs!!0 == x 
              then 1
           else 1 + pos x (tail xs)


idn   (i,_,_,_) = i --identifica o navio
cheg  (_,c,_,_) = c --identifica o inicio da JT do navio
part  (_,_,p,_) = p --identifica o final da JT do navio
carga (_,_,_,c) = c --identifica a carga do navio
idb   (i,_,_) = i --identifica o berco
aber  (_,a,_) = a --identifica o inicio da JT do berco
fech  (_,_,f) = f --identifica o inicio da JT do berco

ta n b ip = ip!!(idb b - 1)!!(idn n - 1)  -- tempo de atendimento
said n b ip = cheg n + ta n b ip --saida apos atendimento
