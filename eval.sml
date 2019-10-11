(* do not change any of this *)
exception RuntimeError of string

local
    val globals: (string * int) list ref = ref []
    val functions: (string * (string list) * expression) list ref = ref []
in
    fun globalGet key =
        let fun f [] = NONE
              | f ((k,v)::tail) = if k = key then SOME v else f tail
        in f (!globals) end

    fun globalSet key value =
        let fun f [] = [(key, value)]
              | f ((k,v)::tail) = if k = key then (k,value)::tail else (k,v)::f tail
        in globals := f (!globals) end

    fun functionGet name =
        let fun f [] = NONE
              | f ((def as (k,_,_))::tail) = if k = name then SOME def else f tail
        in f (!functions) end

    fun functionSet (def as (name, _, _)) =
        let fun f [] = [def]
              | f ((elt as (k,_,_))::tail) = if k = name then def::tail else elt::f tail
        in functions := f (!functions) end

    fun rhoGet [] _ = NONE
      | rhoGet ((key, value)::tail) name =
            if key = name then SOME value else rhoGet tail name

    fun rhoSet [] key value = [(key, value)]
      | rhoSet ((elt as (k, v)) :: tail) key value =
            if key = k then (key, value) :: tail else elt :: rhoSet tail key value

    fun rhoContains rho name =
        case rhoGet rho name of SOME _ => true | NONE => false
end

(* your code goes here *)
fun eval (rho, ValExp n ) = (rho, n)
    | eval (rho, VarExp name) = 
        (case rhoGet rho name of
            SOME v => (rho, v)
            | NONE => ( case globalGet name of 
                SOME v => (rho,v)
                | NONE => raise (RuntimeError "Not found in local or global") )) (* set runtime error here *)
    
    | eval (rho, IfExp(condition, thenpart, elsepart)) =
        ( case eval (rho, condition) of 
            (rho, result) => (rho, result)
            | (rho,0) => eval (rho,elsepart)
            | (rho, _) => eval (rho, thenpart) )

    | eval (rho, WhileExp (condition, body)) =
        ( case eval (rho, condition) of 
            (rho,0) => (rho,0)
            |(rho,1) => (case eval (rho,body) of 
                (rho,b)=> eval (rho, WhileExp (condition, body)) )) 

    | eval (rho, SetExp(name, exp)) =
        (case eval (rho, exp) of
            (rho,v)=> (case rhoContains rho name of
                true => ( (rhoSet rho name v), v )(* not sure if correct *)
                | false => (globalSet name v; (rho,v)) ))

    | eval (rho, BeginExp (x::xs)) =
        if xs = [] then eval (rho,x) else (eval (rho,x); eval(rho, BeginExp xs) ) (* not sure if correct *)

    | eval (rho, BinaryBuiltinExp(oper, lhs, rhs)) =
        (case eval (rho, lhs) of 
            (rho,L) => (case eval (rho, rhs) of
                (rho,R) => (case oper of
                    "+" => (rho, L + R) 
                    | "-" => (rho, L - R)
                    | "*" => (rho, L * R) (* needs / *)
                    | "/" => (rho, L div R)
                    | "=" => if L = R then (rho,1) else (rho,0)
                    | ">" => if L > R then (rho,1) else (rho,0)
                    | "<" => if L < R then (rho,1) else (rho,0) )))

    | eval (rho, UnaryBuiltinExp(str, exp)) =
        (case eval (rho, exp) of
            (rho,v) => (case str of 
                "print" => (print ((Int.toString v) ^ "\n");(rho, 0)) ))

    |eval (rho, ApExp(str, lst)) =
        (case functionGet str of
            SOME (name,parm,exp) => (case List.length parm = List.length lst of
                true => (rho, (case eval ((
                    let fun fr lho (P::Px) (E::Ex) =
                        (case eval (rho, E) of 
                            (_,Z) => fr ((P,Z)::lho) Px Ex )
                    | fr lho [] _ = lho

                    in fr [] parm lst
                    end), exp) of
                        (_,I) => I) ) (*[(parm,eval(exp))]*)
                | false => raise (RuntimeError "len args != len formal params") )
            |NONE => raise (RuntimeError "function not found") )

    | eval (rho,exp) = 
        (print (expressionToRepr exp ^ "\n");
        (rho, 0))
