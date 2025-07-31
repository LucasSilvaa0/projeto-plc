type Id = String
type Numero = Double

data Definicao = Def Id Termo

-- TODO: pode ser this também
data LValue = LVar Id                      -- L-Value de uma variável simples, ex: "x"
            | LAttr Expressao Id           -- L-Value de um atributo, ex: "x" em "p.x"

-- sempre produzem um valor. pode ser um LValue
data Expressao = Lit Numero
               | Var LValue
               | Som Expressao Expressao
               | Mul Expressao Expressao

               | New Id -- não altera o estado, mas altera a heap
               | InstanceOf Expressao Id

               | Lam [Id] Comando
               | CallMethod Expressao Id [Expressao]
               | Apl Expressao [Expressao]

-- muda o estado e controlam o fluxo
data Comando = Atr LValue Expressao
             | Seq Comando Comando
             | If Expressao Comando Comando
             | While Expressao Comando
             | For Comando Expressao Comando Comando
             | Classe Id [Id] [Definicao]
             | CmdExpr Expressao

type Endereco = Int

data Valor = Num Numero                                      -- número
           | ObjectVal Endereco                              -- ponteiro
           | Fun ([Valor] -> Estado -> (Valor, Estado))      -- função. TODO: precisa receber a heap também
           | Nulo                                            -- resultado de operações sem retorno ou valor nulo
           | Erro String

type Objeto = (Id, [(Id, Valor)])
type Ambiente = [(Id,Valor)]
type Estado = [(Endereco, Objeto)]
--int :: Ambiente -> Termo -> Estado -> (Valor, Estado)