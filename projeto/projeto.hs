import Data.List (intercalate)

type Id = String
type Numero = Double

data LValue = LVar Id
            | LAttr Expressao Id
            | LThis
            deriving (Show)

data Expressao = Lit Numero
                | Var LValue
                | Som Expressao Expressao
                | Sub Expressao Expressao
                | Mul Expressao Expressao
                | Div Expressao Expressao
                | Menor Expressao Expressao
                | Maior Expressao Expressao
                | MaiorIgual Expressao Expressao
                | MenorIgual Expressao Expressao
                | Igual Expressao Expressao
                | New Id
                | InstanceOf Expressao Id
                | Lam [Id] Expressao
                | CallMethod Expressao Id [Expressao]
                | Apl Expressao [Expressao]
                deriving (Show)

data Comando = Atr LValue Expressao
                | Seq Comando Comando
                | If Expressao Comando Comando
                | While Expressao Comando
                | For Comando Expressao Comando Comando
                | Class Id [Id] [Definicao]
                | CmdExpr Expressao
                deriving (Show)

data Definicao = Def Id Expressao
                deriving (Show)

type Endereco = Int

data Valor = Num Numero
            | BoolVal Bool
            | ClassVal [(Id, Valor)]
            | ObjectVal Endereco
            | Fun ([Valor] -> Ambiente -> Estado -> Heap -> (Valor, Ambiente, Estado, Heap))
            | Nulo
            | Erro String

instance Show Valor where
    show (Num n) = "Num " ++ show n
    show (BoolVal b) = "Bool " ++ show b
    show (ClassVal campos) = "{ " ++ intercalate ", " (map (\(id, valor) -> id ++ ": " ++ show valor) campos) ++ " }"
    show (ObjectVal e) = "ObjectRef(" ++ show e ++ ")"
    show (Fun _) = "<function>"
    show Nulo = "nulo"
    show (Erro s) = "Erro: " ++ s


type Objeto = (Id, [(Id, Valor)])
type Ambiente = [(Id, Valor)]
type Estado = [(Id, Valor)]
type Heap = [(Endereco, Objeto)]


isTrue :: Valor -> Bool
isTrue (BoolVal b) = b
isTrue _           = False

intArgumentos :: (Maybe Endereco) -> Ambiente -> Estado -> Heap -> [Expressao] -> ([Valor], Ambiente, Estado, Heap)
intArgumentos ctx amb est heap [] = ([], amb, est, heap)
intArgumentos ctx amb est heap (expr:resto_exprs) =
    let
        (val, amb1, est1, heap1) = intExpressao ctx amb est heap expr
        (vals_resto, amb2, est2, heap2) = intArgumentos ctx amb1 est1 heap1 resto_exprs
    in
        (val : vals_resto, amb2, est2, heap2)

getVariavel :: Id -> Estado -> Valor
getVariavel id est = case lookup id est of
    Just val -> val
    Nothing  -> Erro ("Variavel nao encontrada: " ++ id)

setVariavel :: Id -> Valor -> Estado -> Estado
setVariavel id val [] = [(id, val)]
setVariavel id val ((idExistente, valExistente):resto) =
    if id == idExistente
    then (id, val) : resto
    else (idExistente, valExistente) : setVariavel id val resto

getValor :: Id -> Ambiente -> Estado -> Valor
getValor id amb est = case lookup id amb of
    Just val -> val 
    Nothing  -> getVariavel id est 

intExpressao :: (Maybe Endereco) -> Ambiente -> Estado -> Heap -> Expressao -> (Valor, Ambiente, Estado, Heap)
intComando   :: (Maybe Endereco) -> Ambiente -> Estado -> Heap -> Comando   -> (Ambiente, Estado, Heap)

intExpressao ctx amb est heap expr = case expr of
    (Lit n) -> (Num n, amb, est, heap)

    (Var lvalue) -> case lvalue of
        (LVar id) -> (getValor id amb est, amb, est, heap)
        (LAttr objExpr attrId) ->
            let (objVal, amb1, est1, heap1) = intExpressao ctx amb est heap objExpr -- avalia a expressão do objeto pra encontrar o ponteiro
            in case objVal of
                (ObjectVal end) -> -- se for um ponteiro de objeto
                    case lookup end heap1 of -- usa o endereço para procurar o objeto na heap
                        Just (className, attrList) -> -- se encontrou o objeto e seus atributos
                            case lookup attrId attrList of -- procura o atributo
                                Just attrVal -> (attrVal, amb1, est1, heap1) -- se encontrar, retorna a valor
                                Nothing -> (Erro ("Objeto da classe '" ++ className ++ "' nao possui o atributo '" ++ attrId ++ "'."), amb1, est1, heap1)
                        Nothing -> (Erro ("Ponteiro invalido ou objeto nao encontrado no endereco: " ++ show end), amb1, est1, heap1)
                _ -> (Erro ("Tentativa de acessar atributo de um nao-objeto. Valor encontrado: " ++ show objVal), amb1, est1, heap1)

        LThis -> case ctx of
            Just endereco -> (ObjectVal endereco, amb, est, heap)
            Nothing       -> (Erro "A palavra-chave 'this' so pode ser usada dentro de um metodo de objeto.", amb, est, heap)

    (Som e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
            (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
        in case (v1, v2) of
            (Num n1, Num n2) -> (Num (n1 + n2), amb2, est2, heap2)
            _                -> (Erro "Erro de tipo: A operacao '+' espera dois numeros.", amb2, est2, heap2)
    (Sub e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
            (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
        in case (v1, v2) of
            (Num n1, Num n2) -> (Num (n1 - n2), amb2, est2, heap2)
            _                -> (Erro "Erro de tipo: A operacao '-' espera dois numeros.", amb2, est2, heap2)
    (Mul e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
            (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
        in case (v1, v2) of
            (Num n1, Num n2) -> (Num (n1 * n2), amb2, est2, heap2)
            _                -> (Erro "Erro de tipo: A operacao '*' espera dois numeros.", amb2, est2, heap2)
    (Div e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
            (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
        in case (v1, v2) of
            (Num n1, Num 0)  -> (Erro "Erro: Divisao por zero.", amb2, est2, heap2)
            (Num n1, Num n2) -> (Num (n1 / n2), amb2, est2, heap2)
            _                -> (Erro "Erro de tipo: A operacao '/' espera dois numeros.", amb2, est2, heap2)
    (Menor e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
            (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
        in case (v1, v2) of
            (Num n1, Num n2) -> (BoolVal (n1 < n2), amb2, est2, heap2)
            _                -> (Erro "Erro de tipo: A operacao '<' espera dois numeros.", amb2, est2, heap2)
    (Maior e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
            (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
        in case (v1, v2) of
            (Num n1, Num n2) -> (BoolVal (n1 > n2), amb2, est2, heap2)
            _                -> (Erro "Erro de tipo: A operacao '>' espera dois numeros.", amb2, est2, heap2)
    (Igual e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
            (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
        in case (v1, v2) of
            (Num n1, Num n2)   -> (BoolVal (n1 == n2), amb2, est2, heap2)
            (BoolVal b1, BoolVal b2) -> (BoolVal (b1 == b2), amb2, est2, heap2)
            _                  -> (Erro "Erro de tipo: A operacao '==' so compara numeros ou booleanos.", amb2, est2, heap2)

    (New id) -> intNew ctx amb est heap id

    (InstanceOf e id) -> error "Funcionalidade 'InstanceOf' nao implementada."

    (CallMethod objExpr nomeMetodo args) -> error "Funcionalidade 'Chamada de Metodo' nao implementada."



intComando ctx amb est heap cmd = case cmd of
    (Atr (LVar id) expr) ->
        let (val, ambAposExpr, estAposExpr, heapAposExpr) = intExpressao ctx amb est heap expr
            novoEstado = setVariavel id val estAposExpr
        in (ambAposExpr, novoEstado, heapAposExpr)

    (Seq c1 c2) ->
        let (amb1, est1, heap1) = intComando ctx amb est heap c1
        in intComando ctx amb1 est1 heap1 c2

    (For c1 e c2 c3) ->
        let (amb1, est1, heap1) = intComando ctx amb est heap c1
        in forLoop amb1 est1 heap1
        where
            forLoop amb_loop est_loop heap_loop =
                let (v_cond, amb_cond, est_cond, heap_cond) = intExpressao ctx amb_loop est_loop heap_loop e
                in
                    if isTrue v_cond then
                        let (amb_body, est_body, heap_body) = intComando ctx amb_cond est_cond heap_cond c3
                        in let (amb_update, est_update, heap_update) = intComando ctx amb_body est_body heap_body c2
                        in forLoop amb_update est_update heap_update
                    else
                        (amb_cond, est_cond, heap_cond)

    (While cond corpo) -> intWhile ctx amb est heap (While cond corpo)
    
    (If cond c1 c2) -> error "Funcionalidade 'If' nao implementada."

    (Class id params defs) -> intClass ctx amb est heap (Class id params defs)

    (CmdExpr expr) ->
        let (_, ambFinal, estFinal, heapFinal) = intExpressao ctx amb est heap expr -- avalia a expressão
        in (ambFinal, estFinal, heapFinal) -- o valor final é ignorado, mas o ambiente, estado e heap são atualizados

intNew :: (Maybe Endereco) -> Ambiente -> Estado -> Heap -> Id -> (Valor, Ambiente, Estado, Heap)
intNew ctx amb est heap classId =
    case lookup classId amb of -- procura a definição da classe no ambiente
        Just (ClassVal campos) -> -- se for uma classe
            let novoEnd = length heap + 1 -- Estratégia de alocação simples
                novoObjeto = (classId, campos) -- cria a instância do objeto
                novaHeap = heap ++ [(novoEnd, novoObjeto)] -- adiciona o novo objeto na heap
            in (ObjectVal novoEnd, amb, est, novaHeap) -- retorna o ponteiro para o novo objeto

        Just _ -> (Erro ("'" ++ classId ++ "' nao e uma classe."), amb, est, heap)
        Nothing -> (Erro ("Classe '" ++ classId ++ "' nao foi definida."), amb, est, heap)

intWhile :: (Maybe Endereco) -> Ambiente -> Estado -> Heap -> Comando -> (Ambiente, Estado, Heap)
intWhile ctx amb est heap (While cond corpo) =
    let (resultado, amb1, est1, heap1) = intExpressao ctx amb est heap cond
    in case resultado of
        (BoolVal True) ->
            let (amb2, est2, heap2) = intComando ctx amb1 est1 heap1 corpo
            in intWhile ctx amb2 est2 heap2 (While cond corpo)
        (BoolVal False) -> (amb1, est1, heap1)
        _ -> error ("Erro de Tipo: A condição do 'while' deve ser um Booleano, mas foi: " ++ show resultado)

intClass :: (Maybe Endereco) -> Ambiente -> Estado -> Heap -> Comando -> (Ambiente, Estado, Heap)
intClass ctx amb est heap (Class cid params defs) =
    let campos =
            map (\x -> (x, Nulo)) params
            ++ 
            map (\(Def nome corpo) -> (nome, Fun (\args amb est heap -> let (val, _, _, _) = intExpressao ctx amb est heap corpo in (val, amb, est, heap))) ) defs   

        classVal = ClassVal campos                   
        novoAmbiente = (cid, classVal) : amb         
    in (novoAmbiente, est, heap)

{-
-- i = 10; j = i + 5
testeSeq :: Comando
testeSeq = Seq (Atr (LVar "i") (Lit 10))
               (Atr (LVar "j") (Som (Var (LVar "i")) (Lit 5)))

-- i = 0; while (i < 3) { i = i + 1 }
testeWhile :: Comando
testeWhile = Seq (Atr (LVar "i") (Lit 0))
                 (While (Menor (Var (LVar "i")) (Lit 3))
                        (Atr (LVar "i") (Som (Var (LVar "i")) (Lit 1))))

-- soma = lambda(x, y) = x + y;
-- resultado = soma(10, 5);
testeFuncao :: Comando
testeFuncao =
  Seq
    (Atr (LVar "soma") (Lam ["x", "y"] (Som (Var (LVar "x")) (Var (LVar "y")))))
    (Atr (LVar "resultado") (Apl (Var (LVar "soma")) [Lit 10, Lit 5]))

-- soma = 0;
-- for (i = 0; i < 5; i = i + 1) {
--   soma = soma + i;
-- }
testeFor :: Comando
testeFor =
  Seq
    (Atr (LVar "soma") (Lit 0))
    (For
      (Atr (LVar "i") (Lit 0))
      (Menor (Var (LVar "i")) (Lit 5))
      (Atr (LVar "i") (Som (Var (LVar "i")) (Lit 1)))
      (Atr (LVar "soma") (Som (Var (LVar "soma")) (Var (LVar "i"))))
    )

-- Testando For para fatorial para cobrir os outros operadores

-- fatorial = 1;
-- for (i = 5; i > 0; i = i - 1) {
--   fatorial = fatorial * i;
-- }
testeFor :: Comando
testeFor =
  Seq
    (Atr (LVar "fatorial") (Lit 1))
    (For
      (Atr (LVar "i") (Lit 5))
      (Maior (Var (LVar "i")) (Lit 0))
      (Atr (LVar "i") (Sub (Var (LVar "i")) (Lit 1)))
      (Atr (LVar "fatorial") (Mul (Var (LVar "fatorial")) (Var (LVar "i"))))
    )

testeNew :: Comando -- classe Ponto { x; y }; p = new Ponto();
testeNew = Seq (Class "Ponto" ["x", "y"] [])
               (Atr (LVar "p") (New "Ponto"))

testeErroNew :: Comando -- c = new Carro(); (Carro não foi definida)
testeErroNew = Atr (LVar "c") (New "Carro")

testeTrocaDeClasse :: Comando -- classe Ponto { x; y }; classe Carro { velocidade }; p = new Ponto(); p = new Carro();
testeTrocaDeClasse =
    Seq (Class "Ponto" ["x", "y"] [])
        (Seq (Class "Carro" ["velocidade"] [])
            (Seq (Atr (LVar "p") (New "Ponto"))
                 (Atr (LVar "p") (New "Carro"))
            )
        )
-}