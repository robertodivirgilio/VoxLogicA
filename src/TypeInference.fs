type Mono= Var of string|Appl of string*list<Mono> //MONOTIPI
type Poly=Mono1 of Mono|Quantifier of list<string>*Poly //POLITIPI
type Context=Gamma of list<string*Poly>

               
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


let newvar (r:string)= r+r

let rec fst1 a=
    match a with
    |(b,_,_)->b

let rec snd1 a=
    match a with
    |(_,b,_)->b
 
let rec trd1 a=
    match a with
    |(_,_,b)->b

let rec concatenate2 ls1 ls2=
    match ls1 with
    |[]->ls2
    |a::rest->a::(concatenate2 rest ls2)

let rec concatenate lstoflst=
    match lstoflst with
    |[]->[]
    |a::rest->(concatenate2 a (concatenate rest))

let rec formanormale p=
    match p with
    |Mono1 a->p
    |Quantifier (lstr,q)-> match q with
                            |Mono1 a->p
                            |Quantifier(lstr1,r)->formanormale (Quantifier (concatenate2 lstr lstr1,r))

let rec findincontext r (Gamma G)=
    match G with
    |[]->false 
    |(a,b)::rest-> if a=r then true else findincontext r (Gamma rest)

let rec find x z=
    match z with
    |[]->false
    |a::resto->if a=x then true else (find x resto)

let rec sfoltisci lst= //toglie elementi che si ripetono da una lista
    match lst with
    |[]->[]
    |a::rest->if find a rest then sfoltisci rest else a::(sfoltisci rest)

let rec secondfindlist f lst=
    match lst with
    |[]->failwith "errore1"
    |a::resto->match a with
                |(g,q)->if g=f then q else (secondfindlist f resto)
               
let rec variabilidi ta= //dice quali sono le variabili di ta
    match ta with
    |Var s->[s]
    |Appl("->",[a;b])->sfoltisci(concatenate2 (variabilidi a) (variabilidi b))
    |_->failwith "errore2"

let rec fstfind s G=
    match G with
    |[]->false
    |(a,b)::resto-> if s=a then true else fstfind s resto

let rec remove z a x=  //rimuove x dalla lista z (Ho messo x alla fine cos'i da poter usare List.map quando voglio togliere piu' di un termine da z )
    match z with
    |[]->a
    |y::resto->if x=y then (remove resto a x) else (remove  resto (concatenate2 a [y]) x)

let rec Applicazione lst=
    match lst with
    |x::rest->match (formanormale x) with
                |Mono1(r)->Appl("->",[r;Applicazione rest])
                |Quantifier(lst,Mono1(r))->Appl("->",[r;Applicazione rest])
                |_->failwith "non so che altri casi mettere"
    |_->failwith "non puoi mai stare qui"

let rec quantificatoridi p=
     match p with
     |Quantifier(lst,p)->lst
     |Mono1(r)->[]

let rec Uguaglianza a s G=
    match a with
    |Var e->match s with
            |Var f-> match e with
                     |"Intero"-> match f with
                                    |"Intero"->(true,G)
                                    |"Booleano"->(false,G)
                                    |"Stringa"->(false,G)
                                    |_->(true,(f,Mono1(Var "Intero"))::G)
                     |"Booleano"->match f with
                                    |"Intero"->(false,G)
                                    |"Booleano"->(true,G)
                                    |"Stringa"->(false,G)
                                    |_->(true,(f,Mono1(Var "Booleano"))::G)
                     |"Stringa"->match f with
                                    |"Intero"->(false,G)
                                    |"Booleano"->(false,G)
                                    |"Stringa"->(true,G)
                                    |_->(true,(f,Mono1(Var "Stringa"))::G)
                     |_->match f with
                            |"Intero"-> (true,(e,Mono1(Var"Intero"))::G)
                            |"Booleano"->(true,(e,Mono1(Var"Booleano"))::G)
                            |"Stringa"-> (true,(e,Mono1(Var"Stringa"))::G)
                            |_->(true,G)
            |Appl("->",[x;y])->(true,(e,Quantifier(variabilidi (Appl("->",[x;y])),Mono1(Appl("->",[x;y]))))::G)
            |_->failwith "errore3"
    |Appl("->",[x;y])->match s with
                        |Var t->match t with
                                |"Intero"->failwith "errore4"
                                |"Booleano"->failwith "errore5"
                                |"Stringa"->failwith "errore6"
                                |_->Uguaglianza s a G
                        |Appl("->",[x1;y1])->(fst(Uguaglianza x x1 G)&&fst(Uguaglianza y y1 (snd(Uguaglianza x x1 G))),sfoltisci(concatenate2(concatenate2 (snd(Uguaglianza x x1 G)) (snd(Uguaglianza y y1 G))) G))
                        |_->failwith "errore7"
    |_->failwith "errore8"

let rec Valutamono r s (Gamma G)=
    match r with
        |Var r1->match r1 with
                    |"Intero"->(r,G,false)
                    |"Booleano"->(r,G,false)
                    |"Stringa"->(r,G,false)
                    |_->match fstfind r1 G with
                        |true->match formanormale(secondfindlist r1 G)with
                                |Mono1(r2)->Valutamono r2 s (Gamma G)
                                |Quantifier(lst,Mono1(r2))->Valutamono r2 s (Gamma G)
                                |_->(r,G,false)
                        |false->let r2=newvar r1
                                (Var r2,(r1,(Quantifier(variabilidi (Appl("->",[s;Var r2])),Mono1(Appl("->",[s;Var r2])))))::G,true)
        |Appl("->",[a;b])->if fst(Uguaglianza a s G) then (b,snd(Uguaglianza a s G),true) else (r,G,false)//forse qui bisogna valutare b nel nuovo ambiente
        |_->failwith "errore9"


let rec Valuta1 tipo x (Gamma G)=
    let tipo1=formanormale tipo
    let x1=formanormale x
    match tipo1 with
        |Mono1(r)->match x1 with
                    |Quantifier(lst,Mono1(r1))->failwith "non posso sostituire"
                    |Mono1(r1)-> if trd1(Valutamono r r1 (Gamma G)) then (Mono1(fst1(Valutamono r r1 (Gamma G)))),snd1(Valutamono r r1 (Gamma G)) else failwith "errore18"
                    |_->failwith "errore10"
        |Quantifier(lst,Mono1(r))->match  x1 with 
                                    |Mono1(s)->if trd1(Valutamono r s (Gamma G)) then (Quantifier(variabilidi(fst1(Valutamono r s (Gamma G))),Mono1(fst1(Valutamono r s (Gamma G)))),snd1(Valutamono r s (Gamma G))) else failwith "errore19"
                                    |Quantifier(lst,Mono1(s))->if trd1(Valutamono r s (Gamma G)) then (Quantifier(variabilidi(fst1(Valutamono r s (Gamma G))),Mono1(fst1(Valutamono r s (Gamma G)))),snd1(Valutamono r s (Gamma G))) else failwith "errore20"
                                    |_->failwith "errore11"
        |_->failwith "errore12"



let rec Valuta tipo lst (Gamma G)=
    match lst with
    |[]->(tipo,G)
    |x::rest-> (Valuta (fst(Valuta1 tipo x (Gamma G))) rest (Gamma (snd(Valuta1 tipo x (Gamma G)))))
                                                                                                 

let rec Rulesystemexpr (Gamma G) t=
    match t with
    |ENumber l->(Mono1(Var "Intero"),G)
    |EBool b-> (Mono1(Var "Booleano"),G)
    |EString s-> (Mono1(Var "Stringa"),G)
    |ECall(_,s,lst)-> match (fstfind s G) with
                      |false->let tipo_s=(Quantifier([s],Mono1(Var s)))
                              let lst1=(List.map fst) (List.map (Rulesystemexpr (Gamma G)) lst)
                              Valuta tipo_s lst1 (Gamma G)
                      |true-> let tipo_s=formanormale(secondfindlist s G)
                              let lst1=(List.map fst) (List.map (Rulesystemexpr (Gamma G)) lst)
                              Valuta tipo_s lst1 (Gamma G)

let rec ricercalista lst G=
    match lst with
    |[]-> true 
    |x::rest->if fstfind x G then (ricercalista rest G) else false 

let rec ricercalista1 lst G a=
    match lst with
    |[]->a
    |x::rest->if fstfind x G then (ricercalista1 rest G ((secondfindlist x G)::a)) else (ricercalista1 rest G (Quantifier([x],Mono1(Var x))::a))

let rec secondfindlist1 lst G=
    match lst with
    |[]->[]
    |x::rest->(secondfindlist x G)::(secondfindlist1 rest G)

let rec reverse lst a=
    match lst with
    |[]->a
    |x::rest->reverse rest (x::a)

let rec Ritrova m1 G=
    match m1 with
    |Var s->match s with
            |"Intero"->([],Var s)
            |"Booleano"->([],Var s)
            |"Stringa"->([],Var s)
            |_->match  fstfind s G with 
                    |true->match formanormale(secondfindlist s G) with
                            |Mono1(roba)->([],roba)
                            |Quantifier(lst2,Mono1(roba))->(lst2,roba)
                            |_->failwith "errore13"
                    |false->([s],Var s)
    |Appl("->",[a;b])->(sfoltisci(concatenate2 (fst(Ritrova a G)) (fst(Ritrova b G))),Appl("->",[snd(Ritrova a G);snd(Ritrova b G)]))
    |_->failwith "errore14"
                                    

let rec Creaappl lst tipo (Gamma G)=
    let z=formanormale(tipo)
    match z with
    |Quantifier(lst1,Mono1 m)->match lst with
                                |[]->tipo
                                |x::rest->let z1=formanormale x
                                          match z1 with
                                          |Quantifier(lst2,Mono1 m1)->Creaappl rest (Quantifier(sfoltisci(concatenate [fst(Ritrova m1 G);lst2;lst1]),Mono1(Appl("->",[snd(Ritrova m1 G);m])))) (Gamma G)
                                          |Mono1(m1)->Creaappl rest (Quantifier(sfoltisci (concatenate2 (fst(Ritrova m1 G)) lst1) ,Mono1(Appl("->",[snd(Ritrova m1 G);m])))) (Gamma G)
                                          |_->failwith "errore15"
    |Mono1(m)->match lst with
                |[]->tipo
                |x::rest->let z1=formanormale x
                          match z1 with
                          |Quantifier(lst2,Mono1 m1)->Creaappl rest (Quantifier(variabilidi (Appl("->",[snd(Ritrova m1 G);snd(Ritrova m G)])),Mono1(Appl("->",[snd(Ritrova m1 G);snd(Ritrova m G)])))) (Gamma G)
                          |Mono1(m1)->Creaappl rest (Mono1(Appl("->",[snd(Ritrova m1 G);snd(Ritrova m G)]))) (Gamma G)
                          |_->failwith "errore16"
    |_->failwith "errore17"

let rec Rulesystemcom t (Gamma G)=
    match t with
    |Declaration(s,lst,expr)->let (tipoe,G1)=Rulesystemexpr (Gamma G) expr
                              match lst with
                              |[]->tipoe,G1
                              |x::rest->(Creaappl (ricercalista1 lst G1 []) tipoe (Gamma G1)),G1
    |_->failwith "gli altri comandi non sono stati ancora tipati per bene"
                                        
                                                
                                        

let x=Declaration("h",["f";"g"],ECall("","f",[ECall("","g",[])]))

printfn("%A") (Rulesystemcom x (Gamma []))