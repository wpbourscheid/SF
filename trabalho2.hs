------William Bourscheid-------
----------16104663 ---------

-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      | Var String
      | Soma E E
      | Sub E E
      | Mult E E
   deriving(Eq, Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E       -- Verifica se duas expressões aritméticas são iguais
   deriving(Eq, Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B       -- Do C while
    | RepeatUntil C B  -- Repete C até que B seja verdadeiro
    | CondAtrib B E E E  --- Atribuição condicional, recebe uma expressão booleana, uma variável e duas expressões aritméticas: B |- x := E1,E2, Se B é verdade então x := E1 senâo x:= E2
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E   -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1 := e3 e e2 := e4.
   deriving(Eq, Show)   

-----------------------------------------------------
-----
----- As próximas funções servem para manipular a memória (sigma)
-----
-----------------------------------------------------

--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro (conteúdo da variável):

type Memoria = [(String, Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp", 0), ("y", 0)]

--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10

procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s, i): xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v

--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. 
--- A chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x", 10),("temp", 20),("y", 0)]
---
--- é equivalente a operação exSigma[temp -> 20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s, i): xs) v n
  | s == v     = ((s, n): xs)
  | otherwise  = (s, i): mudaVar xs v n

-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
-------------------------------------

-------- EXP --------

smallStepE :: (E, Memoria) -> (E, Memoria)

-- VAR
smallStepE (Var x, s) = (Num (procuraVar s x), s)

-- SOMA3
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)

-- SOMA2
smallStepE (Soma (Num n) e, s) = 
    let (el, sl) = smallStepE (e,s)
    in (Soma (Num n) el, sl)

-- SOMA1
smallStepE (Soma e1 e2, s) = 
    let (el, sl) = smallStepE (e1, s)
    in (Soma el e2, sl)

-- MULT3    
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)

-- MULT2
smallStepE (Mult (Num n) e, s) = 
    let (el, sl) = smallStepE (e, s)
    in (Mult (Num n) el, sl)

-- MULT1    
smallStepE (Mult e1 e2,s) = 
    let (el,sl) = smallStepE (e1, s)
    in (Mult el e2, sl)

-- SUB3
smallStepE (Sub (Num n1) (Num n2), s) = (Num (n1 - n2), s)

-- SUB2
smallStepE (Sub (Num n) e, s) =
    let (expr, sigma) = smallStepE (e, s)
    in (Sub (Num n) expr, sigma)

-- SUB1
smallStepE (Sub e1 e2, s) =
    let (expr, sigma) = smallStepE (e1, s)
    in (Sub expr e2, sigma)


-------- BOOL --------

smallStepB :: (B, Memoria) -> (B, Memoria)

-- NOT3
smallStepB (Not FALSE, s) = (TRUE, s)

-- NOT2
smallStepB (Not TRUE, s) = (FALSE, s)

-- NOT1
smallStepB (Not b, s) =
   let (bool, sigma) = smallStepB (b, s)
   in (Not bool, sigma)
 
-- AND3
smallStepB (And FALSE b, s ) = (FALSE, s)

-- AND3
smallStepB (And TRUE b, s ) = (b, s)

-- AND1
smallStepB (And b1 b2, s ) =
    let (bool, sigma) = smallStepB (b1, s)
    in (And bool b2, sigma)

-- OR3
smallStepB (Or FALSE b, s ) = (b, s)

-- OR2
smallStepB (Or TRUE b, s ) = (TRUE, s)

-- OR1
smallStepB (Or b1 b2, s ) =
    let (bool, sigma) = smallStepB (b1, s)
    in (Or bool b2, sigma)

-- LEQ3
smallStepB (Leq (Num n1) (Num n2), s) 
   | n1 <= n2 = (TRUE, s)
   | otherwise = (FALSE, s)

-- LEQ2
smallStepB (Leq (Num n) e, s) =
   let (expr, sigma) = smallStepE (e, s)
   in (Leq (Num n) expr, sigma)

-- LEQ1
smallStepB (Leq e1 e2, s) = 
   let (expr, sigma) = smallStepE (e1, s)
   in (Leq expr e2, sigma)


-- IGUAL3
smallStepB (Igual (Num n1) (Num n2), s) 
   | n1 == n2 = (TRUE, s)
   | otherwise = (FALSE, s)

-- IGUAL2
smallStepB (Igual (Num n) e, s) =
   let (expr, sigma) = smallStepE (e, s)
   in (Igual (Num n) expr, sigma)

-- Recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais
-- IGUAL1
smallStepB (Igual e1 e2, s) = 
   let (expr, sigma) = smallStepE (e1, s)
   in (Igual expr e2, sigma)


-------- COMANDOS --------

smallStepC :: (C, Memoria) -> (C, Memoria)

-- IF3
smallStepC (If FALSE c1 c2, s) = (c2, s) 

-- IF2
smallStepC (If TRUE c1 c2, s) = (c1, s) 

-- IF1
smallStepC (If b c1 c2, s) =
   let (bool, sigma) = smallStepB (b, s)
   in (If bool c1 c2, sigma)

-- SEQ2
smallStepC (Seq Skip c2, s) = (c2, s)

-- SEQ1
smallStepC (Seq c1 c2,s) =
    let (cmd, sigma) = smallStepC (c1, s)
    in (Seq cmd c2, sigma)
      
-- ATRIB2
smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)

-- ATRIB1
smallStepC (Atrib (Var x) e, s) =
   let (expr, sigma) = smallStepE (e, s)
   in (Atrib (Var x) expr, sigma)
    
-- WHILE
smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)

-- DoWHILE
smallStepC (DoWhile c b,s) = (Seq c (While b c), s)

-- RepeatUntil C B  -- Repete C até que B seja verdadeiro
smallStepC (RepeatUntil c b, s) = (Seq c (If (Not b) (RepeatUntil c b) Skip), s)

-- CondAtrib B E E E  --- Atribuição condicional, recebe uma expressão booleana, uma variável e duas expressões aritméticas: B |- x := E1,E2, Se B é verdade então x := E1 senâo x:= E2
--ACHEI QUE PRECISARIA DE 3 REGRAS, MAS OCORREU WARNING DE REDUNDÂNCIA E PERCEBI QUE UMA REGRA BASTA
-- CONDATRIB-TRUE
--smallStepC (CondAtrib TRUE (Var x) e1 e2, s) =
--   let (expr, sigma) = smallStepE (e1, s)
--   in (Atrib (Var x) expr, sigma)

--CONDATRIB-FALSE
--smallStepC (CondAtrib FALSE (Var x) e1 e2, s) =
--   let (expr, sigma) = smallStepE (e2, s)
--   in (Atrib (Var x) expr, sigma)

--CONDATRIB1
smallStepC (CondAtrib b (Var x) e1 e2, s) =
   (If b (Atrib (Var x) e1) (Atrib (Var x) e2), s)


-- Swap E E --- recebe duas variáveis e troca o conteúdo delas
smallStepC (Swap (Var x) (Var y), s) = 
    let valX = procuraVar s x
        valY = procuraVar s y
        s1 = mudaVar s x valY
        s2 = mudaVar s1 y valX
        in (Skip, s2)
    

--smallStepC (DAtrrib e1 e2 e3 e4)  -- Dupla atribuição
smallStepC (DAtrrib (Var e1) (Var e2) e3 e4, s) =
    (Seq (Atrib (Var e1) e3) (Atrib (Var e2) e4), s)

----------------------
--  INTERPRETADORES --
----------------------

--- Interpretador para Expressões Aritméticas:

isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False

interpretadorE :: (E, Memoria) -> (E, Memoria)
interpretadorE (e, s) = if (isFinalE e) then (e, s) else interpretadorE (smallStepE (e, s))

--- Interpretador para Expressões Booleanas

isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quando a função smallStepB estiver implementada:

interpretadorB :: (B, Memoria) -> (B, Memoria)
interpretadorB (b, s) = if (isFinalB b) then (b, s) else interpretadorB (smallStepB (b, s))

-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

interpretadorC :: (C, Memoria) -> (C, Memoria)
interpretadorC (c, s) = if (isFinalC c) then (c, s) else interpretadorC (smallStepC (c, s))

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop (Comando Loop ou um While serve?)
--- * Dupla Atribuição
--- * Do While
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x", 3), ("y", 0), ("z", 0)]

exSigma3 :: Memoria
exSigma3 = [("x", 0), ("y", 0), ("z", 0)]

exSigma4 :: Memoria
exSigma4 = [("x", 5), ("y", 0), ("z", 0)]

exSigma5 :: Memoria
exSigma5 = [("x", 10), ("y", 8), ("z", 5)]

exSigmaFibo :: Memoria
exSigmaFibo = [("x", 34), ("y", 21), ("z", 0)]

---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

progExp2 :: E
progExp2 = Sub (Num 3) (Soma (Var "x") (Var "y"))

---
--- Para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")), [("x", 10), ("temp", 0), ("y", 0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1, exSigma)
-- (Num 13, [("x", 10), ("temp", 0), ("y", 0)])

-- *Main> interpretadorE (progExp1, exSigma2)
-- (Num 6, [("x", 3), ("y", 0),("z", 0)])

--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores.

---
--- Exemplos de expressões booleanas: smallStepB (teste1, exSigma) 

teste1 :: B
-- 3+3 <= 2*3
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
-- x+3 <= 2*3
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

---
--- Exemplos de Programas Imperativos:

--para rodar: smallStepC (testec1, exSigma)
testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))


--para rodar: smallStepC (fatorial, exSigma)
fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- Exemplo de programa usando Do-While
-- Do (x = x + 1) while (x <= 5)
programa1 :: C
programa1 = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                    (Leq (Var "x") (Num 5))

-- Exemplo de programa usando If
-- If (x != y) then (testec1) else (y = y * 2; z = y)
programa2 :: C 
programa2 = If (Not (Igual (Var "x")(Var "y"))) 
               testec1 --swap
               (Seq (Atrib (Var "y") (Mult (Var "y") (Num 2)))
                    (Atrib (Var "z") (Var "y")))

-- Exemplo de programa usando Dupla Atribuição
programa3 :: C  
programa3 = DAtrrib (Var "x") (Var "y") (Sub (Num 8) (Num 5)) (Soma (Var "x") (Num 2))


--fibonacci (n - 1) + fibonacci (n - 2)
--Vai calcular a sequencia 10 de Fibonacci: 55
--smallStepE (fibo, exSigmaFibo)
fibo :: E
fibo = (Soma (Sub (Var "x") (Num 1)) (Sub (Var "y") (Num 2)))
