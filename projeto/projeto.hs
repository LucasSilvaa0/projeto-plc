type Id = String
type Numero = Double

data Definicao = Def Id Termo

data Termo = Var Id
           | Lit Numero 
           | Som Termo Termo
           | Mul Termo Termo
           | Lam [Id] Termo
           | Apl Termo [Termo]
           | Atr Id Termo
           | Seq Termo Termo

           | If Termo Termo Termo 
           | While Termo Termo
           | For Termo Termo Termo Termo

           | Classe Id [Id] [Definicao]
           | New Id [Termo]
           | GetAttr Id Termo
           | SetAttr Termo Id Termo
           | CallMethod Termo Id [Termo]
           | InstanceOf Termo Id

type Ambiente = [(Id,Valor)]
type Estado = [(Id,Valor)]
-- int :: Ambiente -> [Termo] -> Estado -> (Valor, Estado)