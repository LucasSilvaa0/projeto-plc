type Id = String
type Numero = Double

data Definicao = Def Id Expressao -- antes tava como Termo, que não existe
                 deriving (Show)

data LValue = LVar Id                      -- L-Value de uma variável simples, ex: "x"
            | LAttr Expressao Id           -- L-Value de um atributo, ex: "x" em "p.x"
            | LThis
            deriving (Show)

-- sempre produzem um valor. pode ser um LValue
data Expressao = Lit Numero
               | Var LValue
               | Som Expressao Expressao
               | Mul Expressao Expressao

               | New Id -- não altera o estado, mas altera a heap
               | InstanceOf Expressao Id

               | Lam [Id] Expressao -- corpo de lambda deve ser uma expressão, não um comando
               | CallMethod Expressao Id [Expressao]
               | Apl Expressao [Expressao]
               deriving (Show)

-- muda o estado e controlam o fluxo
data Comando = Atr LValue Expressao
             | Seq Comando Comando
             | If Expressao Comando Comando
             | While Expressao Comando
             | For Comando Expressao Comando Comando
             | Classe Id [Id] [Definicao]
             | CmdExpr Expressao
             deriving (Show)

type Endereco = Int

data Valor = Num Numero                                      -- número
           | BoolVal Bool                                    -- acho que precisa de um bool para if, while, etc.
           | ObjectVal Endereco                              -- ponteiro
           | Fun ([Valor] -> Estado -> (Valor, Estado))      -- função. TODO: precisa receber a heap também
           | Nulo                                            -- resultado de operações sem retorno ou valor nulo
           | Erro String

instance Show Valor where
    show (Num n) = show n
    show (BoolVal b) = show b
    show (ObjectVal e) = "Object@" ++ show e
    show (Fun _) = "<função>"
    show Nulo = "null"
    show (Erro msg) = "Erro: " ++ msg

type Objeto = (Id, [(Id, Valor)])
type Ambiente = [(Id,Valor)]
type Estado = [(Endereco, Objeto)]

getValor :: Id -> Ambiente -> Valor -- busca o valor de uma variável no ambiente
getValor id [] = Erro ("Variavel nao encontrada: " ++ id)
getValor id ((idAtual, val) : resto)
    | id == idAtual = val
    | otherwise     = getValor id resto

setValor :: Id -> Valor -> Ambiente -> Ambiente -- -- atualiza ou adiciona um valor no ambiente
setValor id val [] = [(id, val)]
setValor id val ((idExistente, valExistente):resto) = if id == idExistente then (id, val) : resto else (idExistente, valExistente) : setValor id val resto

intExpressao :: (Maybe Endereco) -> Ambiente -> Estado -> Expressao -> (Valor, Ambiente, Estado)
intExpressao ctx amb est expr = case expr of
    (Lit n) -> (Num n, amb, est)
    (Var (LVar id)) -> (getValor id amb, amb, est)
    (Som e1 e2) ->
        let (v1, amb1, est1) = intExpressao ctx amb est e1
            (v2, amb2, est2) = intExpressao ctx amb1 est1 e2
        in case (v1, v2) of
            (Num n1, Num n2) -> (Num (n1 + n2), amb2, est2)
            _ -> (Erro "Erro de tipo na soma", amb2, est2)

    (Apl (Var (LVar "menorQue")) [arg1, arg2]) ->
        let (v1, amb1, est1) = intExpressao ctx amb est arg1
            (v2, amb2, est2) = intExpressao ctx amb1 est1 arg2
        in case (v1, v2) of
            (Num n1, Num n2) -> (BoolVal (n1 < n2), amb2, est2)
            _ -> (Erro "Argumentos invalidos para o operador 'menorQue'", amb2, est2)

    _ -> (Erro ("Expressao nao implementada para teste: " ++ show expr), amb, est)

intComando :: (Maybe Endereco) -> Ambiente -> Estado -> Comando -> (Ambiente, Estado)
intComando ctx amb est cmd = case cmd of
    (Atr (LVar id) expr) ->
        let (val, ambAposExpr, estAposExpr) = intExpressao ctx amb est expr
        in (setValor id val ambAposExpr, estAposExpr)
    (Seq c1 c2) ->
        let (ambAposC1, estAposC1) = intComando ctx amb est c1
        in intComando ctx ambAposC1 estAposC1 c2
    (While cond corpo) -> intWhile ctx amb est (While cond corpo)
    _ -> error ("Comando nao implementado para teste: " ++ show cmd)

intWhile :: (Maybe Endereco) -> Ambiente -> Estado -> Comando -> (Ambiente, Estado)
intWhile ctx amb est (While cond corpo) =
    let (resultado, ambAposCond, estAposCond) = intExpressao ctx amb est cond
    in case resultado of
        (BoolVal True) ->
            let (ambAposCorpo, estAposCorpo) = intComando ctx ambAposCond estAposCond corpo
            in intWhile ctx ambAposCorpo estAposCorpo (While cond corpo)
        (BoolVal False) -> (ambAposCond, estAposCond)
        _ -> error ("Erro de Tipo: A condição do 'while' deve ser um Booleano, mas foi: " ++ show resultado)

-- i = 10
testeAtr :: Comando
testeAtr = Atr (LVar "i") (Lit 10)

-- i = 10; j = i + 5
testeSeq :: Comando
testeSeq = Seq (Atr (LVar "i") (Lit 10))
               (Atr (LVar "j") (Som (Var (LVar "i")) (Lit 5)))

-- i = 0; while (i < 3) {i = i + 1;}
testeWhile :: Comando
testeWhile = Seq (Atr (LVar "i") (Lit 0))
                 (While (Apl (Var (LVar "menorQue")) [Var (LVar "i"), Lit 3])
                 (Atr (LVar "i") (Som (Var (LVar "i")) (Lit 1))))
