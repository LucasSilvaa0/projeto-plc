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
                | New Id [Expressao] 
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
        _ -> error "Var para This ou Atributo nao implementado"

    (Som e1 e2) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
        in let (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
            in case (v1, v2) of
                (Num n1, Num n2) -> (Num (n1 + n2), amb2, est2, heap2)
                _                -> (Erro "Erro de tipo: A operacao soma espera dois numeros.", amb2, est2, heap2)

    (Apl (Var (LVar "menorQue")) [e1, e2]) ->
        let (v1, amb1, est1, heap1) = intExpressao ctx amb est heap e1
        in let (v2, amb2, est2, heap2) = intExpressao ctx amb1 est1 heap1 e2
            in case (v1, v2) of
                 (Num n1, Num n2) -> (BoolVal (n1 < n2), amb2, est2, heap2)
                 _                -> (Erro "Argumentos invalidos para 'menorQue'.", amb2, est2, heap2)

    -- Implementação de Apl genérica
    (Apl expr_funcao exprs_args) ->
        let
            (val_funcao, amb1, est1, heap1) = intExpressao ctx amb est heap expr_funcao
            (valores_args, amb2, est2, heap2) = intArgumentos ctx amb1 est1 heap1 exprs_args
        in
            case val_funcao of
                (Fun f) -> f valores_args amb2 est2 heap2
                _       -> (Erro "Erro de tipo: Tentativa de chamar algo que nao e uma funcao.", amb2, est2, heap2)
    
    (Lam ids corpo) ->
        let
            funcao_closure = \valores_args amb_chamada est_chamada heap_chamada ->
                if length ids /= length valores_args
                then (Erro ("Numero incorreto de argumentos. Esperava " ++ show (length ids) ++ ", recebeu " ++ show (length valores_args)), amb_chamada, est_chamada, heap_chamada)
                else
                    let
                        bindings = zip ids valores_args
                        novo_ambiente = bindings ++ amb
                    in intExpressao ctx novo_ambiente est_chamada heap_chamada corpo
        in
            (Fun funcao_closure, amb, est, heap)

    (Mul e1 e2) -> error "Funcionalidade 'Multiplicacao' nao implementada."

    (New id args) -> error "Funcionalidade 'New' nao implementada."

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

    (While cond corpo) -> intWhile ctx amb est heap (While cond corpo)
    
    (If cond c1 c2) -> error "Funcionalidade 'If' nao implementada."

    (For c1 e c2 c3) -> error "Funcionalidade 'For' nao implementada."

    (Class id params defs) -> intClass ctx amb est heap (Class id params defs)

    (CmdExpr expr) -> error "Funcionalidade 'Comando de Expressao' (CmdExpr) nao implementada."

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

-- soma = lambda(x, y) = x + y;
-- resultado = soma(10, 5);
testeFuncao :: Comando
testeFuncao = Seq (Atr (LVar "soma") (Lam ["x", "y"] (Som (Var (LVar "x")) (Var (LVar "y")))))
                  (Atr (LVar "resultado") (Apl (Var (LVar "soma")) [Lit 10, Lit 5]))
-}

soma = lambda(x, y) = x + y;
resultado = soma(10, 5);
testeFuncao :: Comando
testeFuncao = Seq (Atr (LVar "soma") (Lam ["x", "y"] (Som (Var (LVar "x")) (Var (LVar "y")))))
                  (Atr (LVar "resultado") (Apl (Var (LVar "soma")) [Lit 10, Lit 5]))