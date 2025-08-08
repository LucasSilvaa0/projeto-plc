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
                | Mul Expressao Expressao 
                | New Id -- retirar [Expressao] pois new não recebe argumentos
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

        LThis -> error "Var para This ainda nao implementado"

    (Som e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
        in let (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
            in case (v1, v2) of
                (Num n1, Num n2) -> (Num (n1 + n2), amb2, est2, heap2)
                _                -> (Erro "Erro de tipo: A operacao soma espera dois numeros.", amb2, est2, heap2)

    (Apl (Var (LVar "menorQue")) [e1, e2]) -> -- Precisa implementar. Está impovisado
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
        in let (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
            in case (v1, v2) of
                 (Num n1, Num n2) -> (BoolVal (n1 < n2), amb2, est2, heap2)
                 _                -> (Erro "Argumentos invalidos para 'menorQue'.", amb2, est2, heap2)

    (Mul e1 e2) -> error "Funcionalidade 'Multiplicacao' nao implementada."

    (New id) -> intNew ctx amb est heap id

    (InstanceOf e id) -> error "Funcionalidade 'InstanceOf' nao implementada."

    (Lam ids corpo) -> error "Funcionalidade 'Definicao de Funcao' (Lam) nao implementada."

    (CallMethod objExpr nomeMetodo args) -> error "Funcionalidade 'Chamada de Metodo' nao implementada."



intComando ctx amb est heap cmd = case cmd of
    (Atr (LVar id) expr) ->
        let (val, ambAposExpr, estAposExpr, heapAposExpr) = intExpressao ctx amb est heap expr
            novoEstado = setVariavel id val estAposExpr
        in (ambAposExpr, novoEstado, heapAposExpr)

    (Seq c1 c2) ->
        let (amb1, est1, heap1) = intComando ctx amb est heap c1
        in intComando ctx amb1 est1 heap1 c2

    (While cond corpo) -> intWhile ctx amb est heap (While cond corpo)
    
    (If cond c1 c2) -> error "Funcionalidade 'If' nao implementada."

    (For c1 e c2 c3) -> error "Funcionalidade 'For' nao implementada."

    (Class id params defs) -> intClass ctx amb est heap (Class id params defs)

    (CmdExpr expr) ->
        let (_, ambFinal, estFinal, heapFinal) = intExpressao ctx amb est heap expr -- avalia a expressão
        in (ambFinal, estFinal, heapFinal) -- o valor final é ignorado, mas o ambiente, estado e heap são atualizados

intNew :: (Maybe Endereco) -> Ambiente -> Estado -> Heap -> Id -> (Valor, Ambiente, Estado, Heap)
intNew ctx amb est heap classId =
    -- 1. Procura a definição da classe no Ambiente.
    case lookup classId amb of
        -- 2. Encontrou! Agora verifica se é de fato um ClassVal.
        Just (ClassVal campos) ->
            -- 3. Pega um novo endereço livre na Heap.
            let novoEnd = length heap + 1 -- Estratégia de alocação simples
                -- 4. Cria a instância do objeto usando os campos definidos na classe.
                novoObjeto = (classId, campos)
                -- 5. Adiciona o novo objeto à Heap.
                novaHeap = heap ++ [(novoEnd, novoObjeto)]
            -- 6. Retorna o ponteiro para o novo objeto. O Ambiente e o Estado não mudam.
            in (ObjectVal novoEnd, amb, est, novaHeap)

        -- Encontrou um nome, mas não é uma classe.
        Just _ -> (Erro ("'" ++ classId ++ "' nao e uma classe."), amb, est, heap)
        -- Não encontrou nenhuma definição com esse nome.
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
-- i = 10
testeAtr :: Comando
testeAtr = Atr (LVar "i") (Lit 10)

-- i = 10; j = i + 5
testeSeq :: Comando
testeSeq = Seq (Atr (LVar "i") (Lit 10))
               (Atr (LVar "j") (Som (Var (LVar "i")) (Lit 5)))

-- i = 0; while (i < 3) {i = i + 1}
testeWhile :: Comando
testeWhile = Seq (Atr (LVar "i") (Lit 0))
                 (While (Apl (Var (LVar "menorQue")) [Var (LVar "i"), Lit 3])
                        (Atr (LVar "i") (Som (Var (LVar "i")) (Lit 1))))

-- class Pessoa { nome; idade; cumprimentar() { ... } aniversario() { ... } }
testeClass :: Comando
testeClass = Seq (Class "Pessoa" ["nome", "idade"]
                [ Def "cumprimentar" (Lam [] (Lit 0))  -- corpo de teste (será ignorado agora)
                , Def "aniversario" (Lam [] (Lit 0))
                ])
                (Class "Aluno" ["nome", "idade", "curso"]
                [ Def "cumprimentar" (Lam [] (Lit 0))  -- corpo de teste (será ignorado agora)
                , Def "aniversario" (Lam [] (Lit 0))
                ])

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