(* how to define a set in SML? *)
(* use a list *)
val A = [3,1,2]; val B = []; val C = [2,3,1]; val D = [2,3,3,1,4];
fun empty([]) = true | empty(_) = false;
empty(A); empty(B);
(* determine if some value is an element of a set *)
fun element(e,[]) = false 
  | element(e,(x::xs)) = x=e orelse element(e,(xs));
element(3,A); element(4,A); element(4,C); element(3,C);

(*Is one set a subset of another*)
fun subset([],[]) = true
  | subset([],(x::xs)) = true
  | subset((x::xs),[]) = false
  | subset((x::xs),(y::ys)) = element(x,(y::ys)) andalso subset(xs,(y::ys));
subset(A,C); subset(B,A); subset(A,B); subset(A,D); subset(C,D);



(* Are two sets equal *)
fun equal(xs,ys) = subset(xs,ys) andalso subset(ys,xs);
equal(A,B); equal(A,C); equal(A,D);
val A = [1,3,5]; val B = [5,3,1];
equal(A,B);
fun properSubset(x,y) = subset(x,y) andalso not (equal(x,y));
subset(A,B);
properSubset(A,B);
val x = 5;
val A = []; val B = [x];
subset(A,B);

(* powerset set of all subsets *)
PolyML.print_depth 128;
val A = [1,3,5]; val B = [1,2]; 
fun union([],ys) = ys | union(x::xs,ys) = 
                        if element(x,ys) then union(xs,ys)
                        else x::union(xs,ys);
union(A,B);
fun insert(x,[]) = [] | insert(x,y::ys) = union([x],y)::insert(x,ys);
insert(3,[B]);
fun powerset([]) = [[]] | powerset(x::xs) =
                          union(insert(x,powerset(xs)),powerset(xs));
powerset(A);
(* cartesian product  set of pairs or tupels of any size *)
val A = [1,2]; val B = [1,2];

fun onepair(x,[]) = [] | onepair(x,y::ys) = (x,y)::onepair(x,ys)

fun cartesian([],ys) = []
   |cartesian(x::xs,ys) = onepair(x,ys) @ cartesian(xs,ys);

cartesian(A,B);
cartesian(A,C);
