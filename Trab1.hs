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
                         then tf b ln ip
                    else ten b ln ip + tempoOcioso b (b, tail ln) ip
            where
                ln = (snd la)

--Final do atendimento do ultimo navio ate o fechamento do berco (Tempo de Fechamento - tf)
tf b ln ip = fech b - said (ln!!0) b ip

--Final do atendimento do navio anterior ate a chegada do proximo navio (Tempo Entre Navios - ten)
ten b ln ip = cheg (ln!!1) - said (ln!!0) b ip

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


naviosCandidatosBerco :: [Berco] -> [Navio] -> InfoPorto -> [(Int, [Navio])]
naviosCandidatosBerco lb ln ip = [ (idb b, [n | n <- ln, atendido n b ip]) | b <- lb]

insereNavioBerco :: Navio -> (Berco, [Navio]) -> InfoPorto -> (Int, Int, [Navio])
insereNavioBerco n la ip = (idb b, somac ln', ln')
                    where
                        b = fst la
                        ln' = vln n b ln la ip
                        ln = snd la

--Verificacao preliminar (chama funcao atendido e verifica o tempo ocioso para depois chamar a verificacao de atendimento refinada)
vln :: Navio -> Berco -> [Navio] -> (Berco, [Navio]) -> InfoPorto -> [Navio]
vln n b ln la ip = if not (atendido n b ip) 
                        then ln
                    else if tempoOcioso b la ip < ta n b ip
                            then ln
                        else if vat n b ln ip
                                then filaNavios' (n : ln)
                            else ln
                    

--Verificacao de Atendimento
vat :: Navio -> Berco -> [Navio] -> InfoPorto -> Bool
vat n b ln ip = if said n b ip <= part (ln!!0) - ta n b ip --verifica alocacao na primeira posicao
                    then if vrec n b ln ip                 --verifica alocacao nas posicaoes intermediarias
                            then if said n b ip <= fech b && cheg n < said (ln!!0) b ip --verifica alocacao na ultima posicao
                                    then True
                                 else False
                         else False
                else False
                       
--then if said n b ip <= fech b && cheg n <= said (ln!!0) b ip

--Verifica o encaixe do navio entre atendimentos
vrec n b ln ip =  if null ln
                      then False
                  else if cheg n <= said (ln!!0) b ip && said n b ip <= cheg (ln!!1) && ta n b ip <= ten b ln ip
                      then True
                  else vrec n b (tail ln) ip

--Soma de Carga
somac :: [Navio] -> Int
somac ln = if null ln
                then 0
            else carga (ln!!0) + somac (tail ln)

esperaNavio :: Navio -> (Berco, [Navio]) -> InfoPorto -> Int
esperaNavio n la ip = if not (ln ==  ln')
                            then (said (ln'!!(posn - 2)) b ip) - cheg n 
                      else error ("O navio " ++ show (idn n) ++ " nao foi alocado no berco " ++ show (idb b) ++ ".")
            where    
                ln = snd la
                ln' = trd3 (insereNavioBerco n la ip)
                b = fst la
                posn = pos n ln'

constroiAlocacaoBerco :: Berco -> [Navio] -> InfoPorto -> (Berco, Int, [(Int, Int, Int)])
constroiAlocacaoBerco b ln ip = (b, somac ln', laloc)
                    where
                        --laloc = aloc b ln ip
                        laloc = [(idn n, cheg n, said n b ip) | n <- ln', said (ln'!!0) b ip < part (ln'!!1) - ta (ln'!!1) b ip]
                        ln' = snd ((naviosCandidatosBerco ([b]) ln ip)!!0)



{-aloc b ln ip = if null ln
                    then []
                else if said (ln'!!0) < part (ln'!!1) - ta (ln'!!1) b ip 
                        then ln'!!0 : aloc (tail ln')
                    else 

                        ln' = naviosCandidatosBerco [b] ln ip
-}

idn   (i,_,_,_) = i --identifica o navio
cheg  (_,c,_,_) = c --identifica o inicio da JT do navio (chegada do navio no PORTO)
part  (_,_,p,_) = p --identifica o final da JT do navio (saida do navio no PORTO)
carga (_,_,_,c) = c --identifica a carga do navio
idb   (i,_,_) = i --identifica o berco
aber  (_,a,_) = a --identifica o inicio da JT do berco
fech  (_,_,f) = f --identifica o inicio da JT do berco

ta n b ip = ip!!(idb b - 1)!!(idn n - 1)  -- tempo de atendimento
said n b ip = cheg n + ta n b ip --saida apos atendimento

trd3 (_,_,z) = z

--OBS.: FALTA FAZER VALIDACAO DE DADOS PARA AS ENTRADAS
