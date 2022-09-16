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
    |Mono1 (Appl("*",[a;_]))->a
    |Mono1(Var "Finelista")->Var "Finelista"
    |_->failwith "errore"

let rec find1 lst z (Gamma G)= //cerca la lista di stringhe in z e fornisce un tipo alle sue componenti
    match lst with
    |[]->Mono1(Var "Finelista")
    |a::resto->match z with
               |Mono1(Var str)->match  find str G with
                                |true-> match str=a with
                                        |true->let y=(secondfindlist str G)
                                               match y with
                                                |Mono1(b)->Mono1(Appl("*",[b;proiezione1 (find1 resto z (Gamma G))]))
                                                |Quantifier(lst,p)->let y1=formanormale y
                                                                    match y1 with
                                                                    |Quantifier(lst1,po)->match po with
                                                                                          |Mono1 q->Quantifier(lst1,Mono1(Appl("*",[q;proiezione1 (find1 resto z (Gamma G))])))
                                                                                          |_->failwith "errore impossibile"
                                                                    |_->failwith "errore impossibile"//teoricamente non dovrebbe mai entrare in questo errore

                                        |false->Quantifier([a],Mono1(Appl("*",[Var "a";proiezione1 (find1 resto z (Gamma G))]))) //nel momento in cui a non e' presente in z, essa e' una variabile libera
                                |_->failwith "errore no so di cosa tu stia parlando" //avviene quando chiami una funzione che non e' nel contesto
                |Mono1(Appl(str,lst))-> match str=a with
                                        |true->let y=(secondfindlist str G)
                                               match y with
                                                |Mono1(b)->Mono1(Appl("*",[b;proiezione1 (find1 resto z (Gamma G))]))
                                                |Quantifier(lst,p)->let y1=formanormale y
                                                                    match y1 with
                                                                    |Quantifier(lst1,po)->match po with
                                                                                          |Mono1 q->Quantifier(lst1,Mono1(Appl("*",[q;proiezione1 (find1 resto z (Gamma G))])))
                                                                                          |_->failwith "errore impossibile"
                                                                    |_->failwith "errore impossibile"
                                        |_->failwith "funzione da continuare"

                                        


let rec Rulesystemcomm (Gamma G) t=
    match t with 
    |Declaration (s,lstr,expr)->let z= Rulesystemexpr (Gamma G) expr
                                let y=find1 lstr z (Gamma G)
                                Mono1(Appl("freccia",[y,z]))
    |Print (_,s,expr)->let z= Rulesystemexpr (Gamma G) expr
                       