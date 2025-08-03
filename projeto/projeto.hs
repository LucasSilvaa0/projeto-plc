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
getValor id amb = case lookup id amb of -- usa lookup para procurar o id no ambiente
    Just val -> val
    Nothing  -> Erro ("Variavel nao encontrada: " ++ id)

setValor :: Id -> Valor -> Ambiente -> Ambiente -- -- atualiza ou adiciona um valor no ambiente
setValor id val [] = [(id, val)]
setValor id val ((idExistente, valExistente):resto) =
    if id == idExistente
    then (id, val) : resto
    else (idExistente, valExistente) : setValor id val resto

intExpressao :: (Maybe Endereco) -> Ambiente -> Estado -> Expressao -> (Valor, Ambiente, Estado)
intExpressao ctx amb est expr = case expr of
    (Lit n) -> (Num n, amb, est)

    (Var lvalue) -> case lvalue of
        (LVar id) -> (getValor id amb, amb, est)
        LThis     -> error "This (em Var) ainda nao implementado"
        _         -> error "Var de Atributo (p.x) ainda nao implementado"

    (Som e1 e2) ->
    let (v1, amb1, est1) = intExpressao ctx amb est e1 -- avalia a primeira expressão
    in let (v2, amb2, est2) = intExpressao ctx amb1 est1 e2 -- avalia a segunda expressão no estado resultante da primeira
        in case (v1, v2) of -- verifica se os resultados são números
            (Num n1, Num n2) -> (Num (n1 + n2), amb2, est2) -- soma
            _                -> (Erro "Erro de tipo: A operacao soma espera dois numeros", amb2, est2)
            
    (Mul e1 e2) -> error "Multiplicacao ainda nao implementada"
    (New id) -> error "New ainda nao implementado"
    (InstanceOf e id) -> error "InstanceOf ainda nao implementado"
    (Lam ids corpo) -> error "Definicao de funcao ainda nao implementado"
    (CallMethod objExpr nomeMetodo args) -> error "Chamada de metodo ainda nao implementado"
    (Apl funcExpr args) -> error "Aplicacao de funcao ainda nao implementada"

intComando :: (Maybe Endereco) -> Ambiente -> Estado -> Comando -> (Ambiente, Estado)
intComando ctx amb est cmd = case cmd of
    (Atr lvalue expr) -> case lvalue of
        (LVar id) ->
            let (val, ambAposExpr, estAposExpr) = intExpressao ctx amb est expr -- avalia a expressão da direita para pegar o valor
            in (setValor id val ambAposExpr, estAposExpr) -- atualiza o ambiente com o novo valor para a variável e retorna o estado
        _ -> error "Atribuicao de atributo ou This nao implementado."

    (Seq c1 c2) ->
        let (ambAposC1, estAposC1) = intComando ctx amb est c1 -- executa o primeiro comando
        in intComando ctx ambAposC1 estAposC1 c2 -- executa o segundo comando no estado resultante do primeiro

    (If cond c1 c2) -> error "If ainda nao implementado"
    (While cond corpo) -> intWhile ctx amb est (While cond corpo)
    (For c1 e c2 c3) -> error "For ainda nao implementado"
    (Classe id params defs) -> error "Declaracao de Classe ainda nao implementado"
    (CmdExpr expr) -> error "Comando de Expressao ainda nao implementado"

intWhile :: (Maybe Endereco) -> Ambiente -> Estado -> Comando -> (Ambiente, Estado)
intWhile ctx amb est (While cond corpo) =
    let (resultado, ambAposCond, estAposCond) = intExpressao ctx amb est cond -- avalia a expressão da condição no estado atual
    in
        case resultado of -- analisa o resultado da condição
            (BoolVal True) -> -- se a condição for verdadeira
                let (ambAposCorpo, estAposCorpo) = intComando ctx ambAposCond estAposCond corpo -- executa o corpo
                in intWhile ctx ambAposCorpo estAposCorpo (While cond corpo) -- recursão

            (BoolVal False) -> -- se for falsa
                (ambAposCond, estAposCond) -- o while encerra e retorna o ambiente e estado atuais

            _ -> error ("Erro de Tipo: A condição do while deve ser um Booleano, mas foi: " ++ show resultado)