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

-- Busca o valor de uma variável no ambiente
getValor :: Id -> Ambiente -> Valor
getValor id amb = case lookup id amb of
    Just val -> val
    Nothing  -> Erro ("Variavel nao encontrada: " ++ id)

-- Atualiza ou adiciona um valor no ambiente (imutável)
setValor :: Id -> Valor -> Ambiente -> Ambiente
setValor id val [] = [(id, val)]
setValor id val ((idExistente, valExistente):resto) =
    if id == idExistente
    then (id, val) : resto
    else (idExistente, valExistente) : setValor id val resto

intExpressao :: (Maybe Endereco) -> Ambiente -> Estado -> Expressao -> (Valor, Ambiente, Estado)
intExpressao ctx amb est expr = case expr of
    (Lit n) -> (Num n, amb, est)
    (Var (LVar id)) -> (getValor id amb, amb, est)
    (Som e1 e2) ->
        let (v1, _, _) = intExpressao ctx amb est e1
            (v2, _, _) = intExpressao ctx amb est e2
        in case (v1, v2) of
            (Num n1, Num n2) -> (Num (n1 + n2), amb, est)
            _ -> (Erro "Erro de tipo na soma", amb, est)

    -- Caso especial para nossa função "mágica"
    (Apl (Var (LVar "checar_limite")) [arg]) ->
        let (valArg, _, _) = intExpressao ctx amb est arg
        in case valArg of
            -- A "mágica": retorna True se o número for < 3, False caso contrário
            (Num n) -> if n < 3 then (BoolVal True, amb, est) else (BoolVal False, amb, est)
            _ -> (Erro "Argumento invalido para checar_limite", amb, est)

    _ -> (Erro "Expressao nao implementada para teste", amb, est)

intComando :: (Maybe Endereco) -> Ambiente -> Estado -> Comando -> (Ambiente, Estado)
intComando ctx amb est cmd = case cmd of
    (Atr (LVar id) expr) ->
        let (val, ambAposExpr, estAposExpr) = intExpressao ctx amb est expr
        in (setValor id val ambAposExpr, estAposExpr)

    (Seq c1 c2) ->
        let (ambAposC1, estAposC1) = intComando ctx amb est c1
        in intComando ctx ambAposC1 estAposC1 c2

    (While cond corpo) -> intWhile ctx amb est (While cond corpo)

    _ -> error "Comando nao implementado para teste"

intWhile :: (Maybe Endereco) -> Ambiente -> Estado -> Comando -> (Ambiente, Estado)
intWhile ctx amb est (While cond corpo) =
    -- 1. Avalia a expressão da condição no estado atual
    let (resultado, ambAposCond, estAposCond) = intExpressao ctx amb est cond
    in
        -- 2. Analisa o resultado da condição
        case resultado of
            -- 3. CASO 1: A condição é VERDADEIRA
            (BoolVal True) ->
                -- 3a. Executa o corpo do laço
                let (ambAposCorpo, estAposCorpo) = intComando ctx ambAposCond estAposCond corpo
                -- 3b. RECURSÃO: Tenta executar o laço novamente, no novo estado
                in intWhile ctx ambAposCorpo estAposCorpo (While cond corpo)

            -- 4. CASO 2: A condição é FALSA
            (BoolVal False) ->
                -- 4a. O laço termina. Retorna o estado como estava logo após a condição ser avaliada como falsa.
                (ambAposCond, estAposCond)

            -- 5. CASO 3: A condição não resultou em um booleano. É um erro de tipo!
            _ -> error ("Erro de Tipo: A condição do 'while' deve ser um Booleano, mas foi: " ++ show resultado)

main :: IO ()
main = do
    putStrLn "--- Iniciando Teste do While ---"
    
    -- Definindo o programa de teste usando a AST
    let condWhile = Apl (Var (LVar "checar_limite")) [Var (LVar "i")]
    let corpoWhile = Atr (LVar "i") (Som (Var (LVar "i")) (Lit 1))
    let loopWhile = While condWhile corpoWhile
    let initCmd = Atr (LVar "i") (Lit 0)
    let programaTeste = Seq initCmd loopWhile
    
    -- Configurando o ambiente inicial vazio
    let ambienteInicial = []
    let estadoInicial = []
    
    putStrLn $ "Programa a ser executado: " ++ show programaTeste
    
    -- Executando o interpretador
    let (ambienteFinal, estadoFinal) = intComando Nothing ambienteInicial estadoInicial programaTeste
    
    putStrLn "\n--- Execucao Concluida ---"
    putStrLn "Ambiente Final:"
    print ambienteFinal