datatype 'a seq = Empty | Cons of 'a * (unit -> 'a seq);
fun seqFrom i = Cons(i,fn() => seqFrom(i+1));
val nat = seqFrom 1;
fun takeSeq(0,_) = []
   |takeSeq(_,Empty) = []
   |takeSeq(i,Cons(n,s)) = n::takeSeq(i-1,s());
val test = takeSeq(10000,nat);
fun id(x) = x;
fun f(x) = (~4*x)+9;
fun g(x) = (2*x)-7;
fun fg(x) = (~8*x) + 37;
fun gf(x) = (~8*x) + 11;
(map (fn x=> f(g(x))) test) = (map (fn x=> fg(x)) test);
(map (fn x=> g(f(x))) test) = (map (fn x=> gf(x)) test);
}
\putfig{.55}{.32}{.35}{15-21SML}

\begin{comment}

fun element(e,[]) = false 
  | element(e,(x::xs)) = x=e orelse element(e,(xs));
fun subset([],[]) = true
  | subset([],(x::xs)) = true
  | subset((x::xs),[]) = false
  | subset((x::xs),(y::ys)) = element(x,(y::ys)) andalso subset(xs,(y::ys));
fun onepair(x,[]) = [] | onepair(x,y::ys) = (x,y)::onepair(x,ys)

fun cartesian([],ys) = []
   |cartesian(x::xs,ys) = onepair(x,ys) @ cartesian(xs,ys);











(* reflexive relation *)
val r = [(1,1),(2,2)]; (* if r is relation on A = [1,2] *)
fun unzip [] = ([],[]) | unzip((x,y)::zs) =
    let val(xs,ys) = unzip(zs)
    in (x::xs,y::ys) end;
unzip(r);
fun domain(r) = #1(unzip(r));
domain(r);
(* identity of A=[1,2] = [(1,1),(2,2)] *)
fun identity([]) = [] | identity(x::xs) = (x,x)::identity(xs);
fun reflexive([]) = true 
    | reflexive(r) = 
        let val d = domain(r)
            val i = identity(d)
        in subset(i,r) end;

reflexive(r); (* true or false *)
val r = [(1,1),(2,3)];
reflexive(r);
val r = [(1,1),(2,2),(1,2)];
reflexive(r);
val A = [1,2]; val B = [1,2];
reflexive(cartesian(A,B));
fun twodivides(x) = x mod 2 = 0;
twodivides(1); twodivides(2); twodivides(6);
val C = [1,2,3,4]; (* expect [2,4] *)
List.filter twodivides C;
fun divides(x,y) = y mod x = 0;
List.filter divides r;
reflexive( List.filter divides r);















(* symmetric relation *)
val r = [(1,3),(3,1)]; (* if r is relation on A = [1,3] *)
(* symet of A=[1,2] = [(1,2),(2,1)] *)
fun symet([]) = [] | symet((x,y)::zs) = (y,x)::symet(zs);
fun symmetric([]) = true 
    | symmetric(r) = 
        let val t = symet(r)
        in subset(t,r) end;












(* transitive relation *)
val r = [(1,2),(2,3),(1,3)]; (* if r is relation on A = [1,3] *)
(* symet of A=[1,2] = [(1,2),(2,1)] *)
fun transi([]) = [] | transi((x,y)::zs) = (y,)::transi(zs);
val poo = transi(r);
fun symmetric([]) = true 
    | symmetric(r) = 
        let val t = symet(r)
        in subset(t,r) end;









(* anti-symmetric relation *)
val r = [(1,2),(3,1)]; (* if r is relation on A = [1,3] *)
(* symet of A=[1,2] = [(1,2),(2,1)] *)
fun asymet([]) = [] 
    | asymet((a,b)::zs) = if a = b then asymet(zs) else (b,a)::asymet(zs);
fun asymmetric([]) = true 
    | asymmetric(r) = 
        let val t = asymet(r)
        in not(subset(t,r)) end;
asymmetric(r);






fun greater(x,y) = x >= y;
val test = takeSeq(10000,nat);
val X = cartesian(C,C);
val Test = List.filter greater X;
reflexive(Test);
symmetric(Test);
transitive(Test); (* Doesn't Work *)
