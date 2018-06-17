type Navio = (Int, Int, Int, Int) -- (ID, Chegada, Partida, Carga)
type Berco = (Int, Int, Int) -- (ID, Abertura, Fechamento)
type InfoPorto = [[Int]] -- [[tn1b1,tn2b1,...][tn1b2,tn2b2,...]...]    
                         -- tn1b1 -> tempo de atendimento navio1 berco1

atendido :: Navio -> Berco -> InfoPorto -> Bool
atendido n b ip = vercheg n b  
                         && verpart n b ip
                         && ip!!(idb b - 1)!!(idn n - 1) /= 0

vercheg n b = cheg n >= aber b
verpart n b ip = fech b >= cheg n + ip!!(idb b - 1)!!(idn n - 1)
                     -- && part n > fech b 

--filaNavios



idn    (i,_,_,_) = i
cheg  (_,c,_,_) = c
part  (_,_,p,_) = p
carga (_,_,_,c) = c
idb   (i,_,_) = i
aber  (_,a,_) = a
fech  (_,_,f) = f
