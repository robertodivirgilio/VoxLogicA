(*
module VoxLogicA.TypeInference
open Parser
type Position = string

type Expression = 
    | ECall of Position * string * (Expression list)
    | ENumber of float
    | EBool of bool
    | EString of string

type Command = 
    | Declaration of string * (string list) * Expression    
    | Save of Position * string * Expression 
    | Print of Position * string * Expression
    | Import of string

type Program = Program of list<Command>



type exprType = 
    TNumber | TBool | TString 
type commType=TFun of List<exprType>*exprType 

type typeEnv = list<string>*(string->exprType)

type Alberello<'a>= Foglia of 'a | Nodocall of list<Alberello<'a>>


let alloca: typeEnv->list<string>*typeEnv=
    fun Gamma->
        let l=fst Gamma
        let l1="vuoto"::l
        let Gamma1=(l1,snd Gamma)
        (l,Gamma1)

let aggiorna: typeEnv -> string->exprType->typeEnv =
    fun Gamma str e->
        match Gamma with 
        | (l,fnz)-> let fnz1 l1 = if str=l1 then e else fnz l1 in (l,fnz1) 




(*let Gamma1=snd(alloca Gamma)
                               match fst Gamma1 with
                               |"vuoto"::l-> let l1=str::l
                                             let Gamma2=(l1,snd Gamma1)
                                             Gamma2
*)


let rec concatenate a b=
    match a with
    |[]->b
    |x::resto-> x::(concatenate resto b)                                      
let rec concatenate3 a b c=concatenate (concatenate a  b) c





let rec isinlistexpr (str:Expression) lexpr=  //Ci dice se una lista di stringhe si trova quantomeno nella lista di espressioni
    match lexpr with
    |[]->false
    |(EString str1)::rest->if str=(EString str1) then true else false
    |(ECall (_,_,lista))::rest->(isinlistexpr str lista)&&(isinlistexpr str rest)
    |_::a->isinlistexpr str a

let rec isin (lstr:list<string>) (expr:Expression)= //Ci dice se una lista di stringhe si trova quantomeno nell'espressione
    match lstr with
    |[]->true
    |str::rest->match expr with
                |EString str->if rest=[] then true else false
                |ECall (_,_,lexpr)->isinlistexpr (EString str) lexpr
                |_->false


(*

let rec typer lstr expr Gamma=
    match lstr with
    |[]->[]
    |str::rest->let hexpr= hmExpr str Gamma

let rec evaluate (lst:list<Expression>) G expr=    //valuto expr con i dati in lst, usando G
    match expr with
    |ENumber a-> ENumber a
    |EBool b->EBool b
    |EString s-> EString s
    |ECall (s,f, lexpr)->let z=(search f G)
                         if fst(z) then match lst with
                                        |[]-> ECall (s,f, lexpr)
                                        |a::resto->match lexpr with 
                                                    |[]->failwith "no se puede faire"
                                                    |b::rest1->let c=(evaluate a (b::rest1))
NON SERVE A NULLA, MA AUMENTA LE RIGHE DI CODICE QUINDI FA FIGO
*)  


//Questa funzione unifica le espressioni, ma per definirla ho bisogno di un'unificazione di lista di espressioni, poiche' con ECall ho bisogno di unificare anche gli argomenti che do in pasto alle funzioni
//G e' la memoria, che salva le unificazioni ben riuscite, DA MIGLIORARE:G dovrebbe essere un insieme, quando invece e' una lista, perche'in questo caso salva piu' volte la stessa cosa



let rec search (f:string) G=
        match G with
        |[]-> (false,(ENumber 1))
        |a::rest->if f=fst(a) then (true,snd(a)) else (search f rest)

let rec unifyexpr ta tb G=  
    match ta with
        |[]->match tb with 
                |[]->  (true,G)
                |_->(false,G)
        |(ENumber a)::rest->match tb with 
                            |(ECall(_,g,[]))::rest1->unifyexpr rest rest1 ((g,ENumber a)::G)
                            |(ENumber a)::rest1->unifyexpr rest rest1 G
                            |_->(false,G)
        |(EBool a)::rest->match tb with 
                            |(ECall(_,g,[]))::rest1->unifyexpr rest rest1 ((g,EBool a)::G)
                            |(EBool a)::rest1->unifyexpr rest rest1 G
                            |_->(false,G)
        |(EString s)::rest->match tb with
                            |(ECall(_,g,[]))::rest1->unifyexpr rest rest1 ((g,EString s)::G)
                            |(EString s)::rest1->unifyexpr rest rest1 G
                            |_->(false,G)
        |(ECall (_,f,lexpr))::rest->match lexpr with
                                    |[]->match tb with 
                                            |ECall (s,g,lexpr1)::rest1->match f=g with
                                                                        |true-> match lexpr1 with
                                                                                    |[]->unifyexpr rest rest1 G
                                                                                    |_->(false,G)
                                                                        |_->if (isinlistexpr (EString f) (lexpr1)) then (false,G) else unifyexpr rest rest1 ((f,ECall (s,g,lexpr1))::G)
                                            |a::rest1-> if (isinlistexpr (EString f) (a::[])) then (false,G) else unifyexpr rest rest1 ((f,a)::G)
                                            |_->(false,G)
                                            
                                    |a::resto->let z=(search f G) //Search cerca f in G e restituisce (bool,snd(f,expr))
                                               match  fst(z) with
                                               |true-> match tb with
                                                       |ECall(_,f,lexpr1)::rest1->(unifyexpr (concatenate (a::resto) rest) (concatenate lexpr1 rest1) G)
                                                       |_->failwith"errore"
                                               |_->failwith"non so chi sia f"
let x=unifyexpr [ECall("ciao","x",[])]  [ECall("ciao","f",[ENumber 1])] [("f",ECall("ciao","a",[]))] 

printf "%A" x


let rec unifytipes ta tb G=
    match ta with
    |




(*
let rec unifycomm ta tb G=
    match ta with
    |Declaration (f,lstr,expr)->match tb with
                                      |Declaration (g,lstr1,expr1)->if f=g then if (ugualelunghezza lstr lstr1) then 
    

*)

(*
let y=(evaluate (snd(z)) (a::resto)) //Evaluate da fare, sostituisce nel primo argomento di tipo espressione gli elementi della lista del secondo argomento
                                                              match tb with 
                                                                    |(ECall(_,g,[]))::rest1->if (isinlistexpr (EString g) y) then (false,G) 
                                                                                                                                else let z1=(search g G)
                                                                                                                                     if fst(z1) then (unifyexpr y::rest snd(z1)::rest1 G) else (unifyexpr rest rest1 (g,y)::G)
                                                                    |(Ecall(_,g,lexpr1))::rest1-> if f=g then (unifyexpr (concatenate lexpr rest) (concatenate lexpr1 rest1) G) else (false,G)
                                                                    |_->(false,G)
                                                                else failwith "non so cosa sia f"                         
*)      


let rec f t=
    match t with            //Qui tolgo le etichette di Alberello per fare il patt match giu'
        |[]->[]
        |(Foglia a)::b-> a::(f b)
        |_->failwith"devo considerare ancora questi casi"


let rec hmExpr Gamma (expr:Expression)= 
        match expr with
        | ENumber a ->   TNumber
        | EBool a ->  TBool
        | EString a-> TString
        | ECall (_,str,lexpr)->let s=search str Gamma
                               failwith "da continuare"


             

let rec hmComm  Gamma c=
    match c with
    |Declaration (str,lstr,expr)-> if (isin lstr expr) then TFun (unifyexpr, hmExpr Gamma expr) else failwith "errore"
                    





(*
    let s=List.map (hmExpr Gamma) args
                                let s1=(f s)
                                let FGamma=snd Gamma 
                                let ApplGamma=FGamma str
                                match ApplGamma with
                                |TFun (a,b)->   match a with            //Qui controllo che il tipo degli input della funzione siano uguali al tipo della 
                                                |s1->Foglia b
                                                |_->failwith "errore"
                                |_->failwith "errore"
*)
                   

(*
let rec hmprogr (p:Program) Gamma=
    match p with
    |(Program [])->([],Gamma)
    |(Program (command::commands))->let (c,_)=(hmcom command Gamma)
                                    (c::hmprog(commands,Gamma),Gamma)
    |_->failwith "errore";*)
*)

module VoxLogicA.TypeInference


type Mono= Var of string|Appl of string*list<Mono> //MONOTIPI
type Poly=Mono1 of Mono|Quantifier of list<string>*Poly //POLITIPI




let rec concatenate2 ls1 ls2=
    match ls1 with
    |[]->ls2
    |a::rest->a::(concatenate2 rest ls2)

let rec concatenate lstoflst=
    match lstoflst with
    |[]->[]
    |a::rest->(concatenate2 a (concatenate rest))

let rec find x z=
    match z with
    |[]->false
    |a::resto->if a=x then true else (find x resto)

let rec remove z a x=  //rimuove x dalla lista z (Ho messo x alla fine cos'i da poter usare List.map quando voglio togliere piu' di un termine da z )
    match z with
    |[]->a
    |y::resto->if x=y then (remove resto a x) else (remove  resto (concatenate2 a [y]) x)

let rec convert lst= //converte una lista di Mono in una lista di Mono1 (quindi di Poly)
    match lst with
    |[]->[]
    |(Var a)::rest->(Mono1 (Var a))::(convert rest)
    |(Appl (b,x))::rest->(Mono1 (Appl (b,x)))::(convert rest)

let rec freecalculatorpoly x= // calcola le variabili libere di un termine Poly
        match x with
        |Mono1 a->match a with
                    |Var s->s::[]
                    |Appl (s,lst)->concatenate (List.map freecalculatorpoly  (convert lst))
        |Quantifier (lstr,a)->match lstr with
                                |[]->freecalculatorpoly a
                                |x::resto->let z=(freecalculatorpoly (Quantifier (resto,a)))
                                           if (find x z) then (remove  z [] x) else z



type Context=Gamma of list<string*Poly>

let rec freecalculatorenv G= //calcola le variabili libere di un contesto
    match G with
    |Gamma []->[]
    |Gamma (a::rest)->concatenate2 (freecalculatorpoly (snd(a))) (freecalculatorenv (Gamma rest))

//Denotiamo con una tripla (G,x,e) la frase "G dimostra che x ha tipo e"
// calcola le variabili libere di una coppia (contesto,termine) (ci servira' piu' tardi)
let rec freecalculatormisto (G,x)=
    let fenv=freecalculatorenv G
    let fx=freecalculatorpoly x
    concatenate(List.map (remove fx []) fenv) //POSSIBILE ERRORE: NON SO SE VOGLIO L'ULTIMO TERMINE DI (LIST.MAP...) O LA CONCATENAZIONE: BISOGNA FARE DELLE PROVE


let rec findforlist a b=
        match b with
        |[]-> false 
        |x::rest->match a with
                    |[]->false
                    |y::rest1->if x=y then true else (findforlist rest1 rest)



let rec isamonosubstitution ta tb= //ci dice se ta si ottiene tramite una sostituzione applicata a tb
    match ta with
    |[]-> if tb=[] then true else false
    |(Var s)::rest-> match tb with
                     |(Var t)::rest1->  isamonosubstitution rest rest1 
                     |_-> false 
    |Appl(C,s)::rest->match tb with
                        |Appl(D,t)::rest1-> match C=D with
                                            |true->match s with
                                                        |[]-> if t=[] then (isamonosubstitution rest rest1) else false 
                                                        |a::rest2->match t with 
                                                                    |[]-> false
                                                                    |b::rest3->(isamonosubstitution (concatenate2 rest2 rest) (concatenate2 rest3 rest1))
                                            |false->false
                        |_->false 


let rec isasubstitution ta tb= //Ci dice se posso ottenere ta tramite una sostituzione applicata a tb
 match tb with
    |Mono1 a-> match tb with
                |Mono1 b->if (isamonosubstitution (a::[]) (b::[]))  then true else false 
                |_->false
    |Quantifier (lst,p)-> match lst with
                             |[]-> isasubstitution ta p  
                             |a::rest-> match ta with
                                        |Mono1 b->(isasubstitution ta (Quantifier(rest,p)))
                                        |Quantifier(lst1,q)->match lst1 with 
                                                             |[]-> isasubstitution p tb
                                                             |b::rest1-> if a=b then (isasubstitution (Quantifier (lst1,q)) (Quantifier (lst,p))) else (isasubstitution ta (Quantifier (lst,p)))//Qui dico che i quantificatori devono essere uguali


let rec mgt ta tb=   //Ci dice se ta e' piu' generico di tb
    match ta with
    |(Quantifier (lstr,p))->match tb with
                            |(Quantifier (lstr1,q))-> if (findforlist lstr1 (freecalculatorpoly ta)) then false else if (isasubstitution q ta) then true else false
                            |Mono1 q-> mgt p tb
    |Mono1 a->match tb with
                |Mono1 b-> if (isamonosubstitution (b::[]) (a::[])) then true else false 
                |_->false 

//tipofunzione=Appl("freccia",[tipo1;tipo2])

let rec secondfindlist f lst=
    match lst with
    |[]->failwith "errore"
    |a::resto->match a with
                |(g,q)->if g=f then q else (secondfindlist f resto)
               
type Position = string

type Expression = 
    | ECall of Position * string * (Expression list)
    | ENumber of float
    | EBool of bool
    | EString of string

type Command = 
    | Declaration of string * (string list) * Expression    
    | Save of Position * string * Expression 
    | Print of Position * string * Expression
    | Import of string

type Program = Program of list<Command>

(*
type Mono= Var of string|Appl of string*list<Mono> //MONOTIPI
type Poly=Mono1 of Mono|Quantifier of list<string>*Poly //POLITIPI
*)
let rec formanormale (Quantifier (lstr,p))=
    match p with
    |Mono1 a->Quantifier (lstr,p)
    |Quantifier (lstr1,q)-> formanormale (Quantifier (concatenate2 lstr lstr1,q))


let rec convert a=
    match a with
    |Appl ("*",[])->[]
    |Appl ("*",b::rest)->(Mono1 b)::convert(Appl ("*",rest))
    |_->failwith "questa funzione non serve ad altro"

let rec mgtforlist ls1 ls2= //ci dice se tutti gli elemeti di ls1 sono piu' generali degli elementi di ls2
    match ls1 with 
    |[]-> match ls2 with 
            |[]->true
            |_->false
    |a::rest-> match ls2 with
                |b::rest1 ->if (mgt a b) then mgtforlist rest rest1 else false
                |_->false 


let rec Rulesystemexpr (Gamma G) t=
    match t with
    |ENumber a->Mono1 (Var "Number")
    |EBool b->Mono1(Var "Boolean")
    |EString s->Mono1(Var "String")
    |ECall (_,f,lexpr)-> match (find f ((List.map fst ) G)) with
                            |true ->match (secondfindlist f G)  with
                                    |Quantifier (lstr,p)-> let (Quantifier (lstr1,mono1))=formanormale(Quantifier (lstr,p))//pattern matching completo invece, viene chiamata solo in questa eventualita'
                                                           match mono1 with
                                                            |Mono1(Appl ("freccia",[a;b]))->match ((List.map (Rulesystemexpr (Gamma G))) lexpr) with
                                                                                            |[]->Quantifier(lstr1,Mono1(Appl ("freccia",[a;b])))//  Ho messo questo risultato perche' se qualcuno chiama f senza parametri (ma e' memorizzata) allora vuole vedere che f ha tipo funzione che va da a a b
                                                                                            |x1::rest->if mgtforlist (convert a) (x1::rest) then (Mono1 b) else failwith "errore"                                                           
                                                            |_->failwith "stai applicando su qualcosa che non e' una funzione"
                                    |Mono1(Appl ("freccia",[a;b]))->match ((List.map (Rulesystemexpr (Gamma G))) lexpr) with
                                                                    |[]->Mono1(Appl ("freccia",[a;b]))//  Ho messo questo risultato perche' se qualcuno chiama f senza parametri (ma e' memorizzata) allora vuole vedere che f ha tipo funzione che va da a a b
                                                                    |x1::rest->if mgtforlist (convert a) (x1::rest) then (Mono1 b) else failwith "errore"
                                    |_->failwith "stai applicando qualcosa a un oggetto che non e' una funzione"
                            |false->match lexpr with
                                    |[]->Quantifier(["f"],Mono1 (Var f)) //HM dice che nel momento di una definizione bisogna essere "Piu' generici possibile"
                                    |_-> failwith "Non so di cosa tu stia parlando" //avviene quando chiami una funzione che non e' nel contesto

let rec proiezione1 lst=
    match lst with
    |[]->[]
    |a::rest->a


let rec find1 lst z (Gamma G)= //cerca la lista di stringhe in z e fornisce un tipo alle sue componenti
    match lst with
    |[]->z
    |a::resto->match z with
               |Mono1(Var str)->match  find str G with
                                |true-> let y=(secondfindlist str G)
                                        match y with
                                        |Mono1(_)->Mono1(Appl("*",y,resto))





let rec Rulesystemcomm (Gamma G) t=
    match t with 
    |Declaration (s,lstr,expr)->let z= Rulesystemexpr (Gamma G) expr
                                let y=find1 lstr z (Gamma G)
                                Mono1(Appl("freccia",[y,z]))
    |Print (_,s,expr)->let z= Rulesystemexpr (Gamma G) expr
                       