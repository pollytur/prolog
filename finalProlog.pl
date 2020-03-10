% assumption - we always occur at (0,0)
% map edges (0,0), (0, 9), (9, 0), (9,9)
% only one throw per search run is valid 
% diagonal throws are valid
% for random search if the player meets org - player dies
% and if he/she tries to go out of map bound  - player dies

% Note that the path output looks like [[], actual path | something], 
% the empty list in the beginning and everything after |can be neglected, 
% it is shown just because I have used difference list for my inner implementation.

% map is insolvable if in order to find touchdown we have to make diagonal move
% this is tested in test one

%   TESTS TO CHECK THAT ALL FEATURES WORK
% sample test will orks in angle 
% should fail as diagonal moves are not allowed
% Test 1
% o(0,3).
% o(1,3).
% o(2,3).
% o(3,2).
% o(3,1).
% o(3,0).
% t(4,4).

% default behaviour for backtracking is moving up and right
% but there are cases when left or down move is unavoidable
% Tests 2 and 3 are to check this
% o(0,3).
% o(1,3).
% o(3,2).
% o(3,1).
% o(3,0).
% t(0,5).

% Test 3
% o(0,3).
% o(1,3).
% o(3,2).
% o(3,1).
% o(3,0).
% t(7,1).

% Test 4 checks if we calculate passing the ball to the other person in the cell correctly
% e.g. is this step really free
% h(0,1).
% h(1,0).
% t(3,3).

% Test 5 - touchdown is surrounded by orks 
% t(7,4).
% o(7,3).
% o(6,3).
% o(8,3).
% o(7,5).
% o(6,5).
% o(8,5).
% o(8,4).
% o(6,4).
% h(2,3).
% h(9,8).


% Test 6 - no touchdown
% o(1,1).
% o(5,8).
% h(3,3).
% h(7,4).
% h(9,9).
% h(0,2).
% h(8, 0).


% Test 7 - touchdown is the start point
%t(0,0). 

% Test 8 - path for touchdown is all with humans
% t(4,9).
% h(0,1).
% h(1,1).
% h(1,2).
% h(2,2).
% h(2,3).
% h(2,4).
% h(2,5).
% h(1,5).
% h(1,6).
% h(0,6).
% h(1,6).
% h(1,7).
% h(2,7).
% h(2,8).
% h(3,8).
% h(3,9).
% h(4,9).

% Test 9 - a person on touchdown and random orks, partially blocking throws interesting for throwing
% t(6,3).
% h(6,3).
% h(0,3).
% h(6,0).
% o(3,0).
% o(5,4).
% o(3,3).

% Test 10 - touchdown is neigboor cell
% t(0,1).

% Test 11 - human out of boarders
%t(4,5).
%h(15, 0).

% Test 12 checks if we calculate passing the ball to the other person in the cell correctly
% e.g. is this step really free
% h(0,1).
% h(1,0).
% h(2,2).
% h(1,1).
% t(3,3).

%Test 13 - through ball and there is a human on touchdown
%t(2,2).
%h(2,2).


%%  TESTS TO ANALYZE AND COMPARE ALGORYTHMS
%Test one
%o(0,3).
%o(1,3).
%o(3,0).
%o(3,1).
%o(3,2).
%t(4,4).
%%Test two
%o(0,3).
%o(1,3).
%o(3,0).
%o(3,1).
%o(3,2).
%t(5,0).
%%Test three
%o(0,3).
%o(1,3).
%o(3,0).
%o(3,1).
%o(3,2).
%h(4,5).
%t(6,6).
%%Test four
%o(0,3).
%o(1,3).
%o(3,0).
%o(3,1).
%o(3,2).
%t(8,9).
%%test five
%h(3,3).
%t(4,4).
%o(0,-1).
%o(0,3).
%o(1,3).
%o(2,3).
%o(3,2).
%o(3,1).
%o(3,0).
%%test six
%o(0,4).
%o(1,4).
%o(3,3).
%o(3,1).
%o(3,2).
%t(6,3).
%%test seven
%o(0,4).
%o(1,4).
%o(3,3).
%o(3,1).
%o(3,2).
%t(6,3).
%%Test eight
%t(0,2).
%%Test nine
%t(5,5).

:- use_module(library(random)).
% random test
o(0,3).
o(1,3).
o(3,0).
o(3,1).
o(3,2).
h(4,5).
t(7,7).



% to algorythn to work with any maps
h(-10,-10).
o(-10,-10).
t(-5,5).


is_ork(X, Y):- o(X, Y).

is_human(X, Y):-h(X, Y).

is_fin(X, Y):- t(X, Y).


%todo to change 5 to 10
is_in_bound(X, Y):- X<10, -1<X, -1<Y, Y<10.

append_to_list(List,Item) :-
    List = [Start|[To_add|Rest]],
    nonvar(Start),
    (var(To_add),To_add=Item;append_to_list([To_add|Rest],Item)).




dfs((X, Y), T, Path, C, R) :- 
    %print("__X="), print(X), print("__Y="), print(Y),
    (is_in_bound(X, Y)-> true;false),
    append_to_list(Path, (X,Y)),
    Xp is X+1,
    Yp is Y+1,
    Xm is X-1,
    Ym is Y-1,

    % for tests when we can see 2 cells forward
    %Xpp is X+2,
    %Ypp is Y+2,
    %Xmm is X-2,
    %Ymm is Y-2,

    (is_human(X,Y) ->  A is C; A is C+1), 
    (is_ork(X,Y) ->false;true),
    ((   is_fin(X, Y)-> R is C;
                ((is_fin(Xp, Y)->dfs((Xp, Y), T, Path, A, R); false);
                (is_fin(X, Yp)->dfs((X, Yp), T, Path, A,R); false);
                (is_fin(X, Ym)->dfs((X, Ym), T, Path, A, R); false);
                (is_fin(Xm, Yp)->dfs((Xm, Y), T, Path, A,R); false);


                % for tests when we can see 2 cells forward 
               %(is_fin(Xpp, Y)-> ( backtr((Xp, Y), T, Cl, Path, A, R)); false);
               %(is_fin(X, Ypp)-> backtr((X, Yp), T, Cl, Path, A, R); false);
               %(is_fin(X, Ymm)-> backtr((X, Ym), T, Cl, Path, A, R); false);
               %(is_fin(Xmm, Y)-> backtr((Xm, Y), T, Cl, Path, A, R); false);

                 (   (   T=0 ->  
                            ( %write("in T=0__"), write(Y),  
                            (throw(0,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R );false);
                            (throw(1,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R );false); 
                            (throw(2,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R );false);
                            (throw(3,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R );false);
                            (throw(4,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R ) ;false);
                            (throw(5,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R );false);
                            (throw(6,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R );false);
                            (throw(7,(X,Y), (X_f, Y_f))-> dfs((X_f, Y_f),1,Path,A,R );false));
                            (false ));
                            (is_human(X, Yp)->dfs((X, Yp), T, Path, A, R); 
                            (is_human(Xp, Y)->dfs((Xp, Y), T, Path, A,R); 
                            (dfs((X, Yp), T, Path, A, R); 
                            dfs((Xp, Y), T, Path, A,R))))
                        )));
            
    
    ((X=0, Y=0)->(   write("in backracking"),backtr((X,Y),T, [], Path, C, R));false)).
    

% T indicates if we still can make a throw
% Closed is a list of visited items
backtr((X, Y), T, Closed, Path, C, R):-
    % print("X="), print(X), print("Y="),print(Y), 
    append(Closed, [(X, Y)], Cl),
    %write(Closed),
    append_to_list(Path, (X,Y)),
    Xp is X+1,
    Yp is Y+1,
    Xm is X-1,
    Ym is Y-1,

    % for tests when we can see 2 cells forward
    %Xpp is X+2,
    %Ypp is Y+2,
    %Xmm is X-2,
    %Ymm is Y-2,
    
    (is_human(X,Y) ->  A is C; A is C+1), 
    (is_ork(X,Y) ->false; true),
    (is_in_bound(X, Y)->
            ( is_fin(X, Y)-> (R is C); 
            (  (is_fin(Xp, Y)-> ( backtr((Xp, Y), T, Cl, Path, A, R)); false);
               (is_fin(X, Yp)-> backtr((X, Yp), T, Cl, Path, A, R); false);
               (is_fin(X, Ym)-> backtr((X, Ym), T, Cl, Path, A, R); false);
               (is_fin(Xm, Y)-> backtr((Xm, Y), T, Cl, Path, A, R); false);

               % for tests when we can see 2 cells forward 
               %(is_fin(Xpp, Y)-> ( backtr((Xp, Y), T, Cl, Path, A, R)); false);
               %(is_fin(X, Ypp)-> backtr((X, Yp), T, Cl, Path, A, R); false);
               %(is_fin(X, Ymm)-> backtr((X, Ym), T, Cl, Path, A, R); false);
               %(is_fin(Xmm, Y)-> backtr((Xm, Y), T, Cl, Path, A, R); false);

                (T=0->( 
                      %write("in T=0"), write(Y), 
                      (throw(0, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (throw(1, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (throw(2, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (throw(3, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (throw(4, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (throw(5, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (throw(6, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (throw(7, (X,Y), (X_f, Y_f))-> backtr((X_f, Y_f), 1, Cl, Path, A, R);false);
                      (member((X,Yp), Closed)->false; backtr((X, Yp), T, Cl, Path, A, R));
                      (member((Xp,Y), Closed)->false; backtr((Xp, Y), T, Cl, Path, A, R));
                      (member((X,Ym), Closed)->false; backtr((X, Ym), T, Cl, Path, A, R));
                      (member((Xm,Y), Closed)->false; backtr((Xm, Y), T, Cl, Path, A, R))      
 
                      );
                      (
                     
                      (member((X,Yp), Closed)->false; backtr((X, Yp), T, Cl, Path, A, R));
                      (member((Xp,Y), Closed)->false; backtr((Xp, Y), T, Cl, Path, A, R));
                      (member((X,Ym), Closed)->false; backtr((X, Ym), T, Cl, Path, A, R));
                      (member((Xm,Y), Closed)->false; backtr((Xm, Y), T, Cl, Path, A, R)) )   
                )
            
            
            ));
    false).

% for dir - 0 is up, 1 is right, 2 is down, 3 is left
% - 4 is up-left, 5 is up-right, 6 is down-right, 7 is down-left

throw(Dir, (X,Y), (X_f, Y_f)):-
    Xp is X+1,
    Yp is Y+1,
    Xm is X-1,
    Ym is Y-1,
    (is_in_bound(X,Y) ->  true; false), 
    (is_ork(X,Y) ->false;true),
    ((is_human(X,Y) -> (X_f is X, Y_f is Y); false);
    (Dir =0 ->  throw(Dir, (X,Yp),  (X_f, Y_f));
                            ( Dir=1 ->  throw(Dir, (Xp, Y), (X_f, Y_f));
                                (Dir=2 ->  throw(Dir, (X, Ym),  (X_f, Y_f)); 
                                (Dir=3 ->  throw(Dir, (X, Ym),  (X_f, Y_f)); 
                                (Dir=4 ->  throw(Dir, (Xm, Yp),  (X_f, Y_f)); 
                                (Dir=5 ->  throw(Dir, (Xp, Yp),  (X_f, Y_f)); 
                                (Dir=6 ->  throw(Dir, (Xp, Ym),  (X_f, Y_f)); 
                                throw(Dir, (Xm, Ym),  (X_f, Y_f)))))))))).



%for dir - 0 is up, 1 is right, 2 is down, 3 is left - throwing
%for dir - 4 is up, 5 is right, 6 is down, 7 is left - step
% T is flag if something was thrown 
% 0 - nothing was thrown, 1 - was
% Num is to check that we have not made more than 100 steps
random_search((X, Y),T,Num, Path, C, R):-
    is_in_bound(X,Y),
    (Num<100-> New_num is Num+1; false),
    (append_to_list(Path, (X,Y)),
    (is_ork(X, Y)->(false, print("Died of ork"));true),
   ((is_fin(X, Y) -> R is C+1; false);
    ((is_human(X, Y)-> B is C; B is C+1),
    %   will return from 0 to 11 but not 8
    (T=0->random(0,12, A);random(8,12, A)), 
    ((A=0->(throw(0, (X, Y),(X_f, Y_f))-> random_search((X_f, Y_f),1, New_num, Path, B, R); false); false);
    (A=1->(throw(1, (X, Y),(X_f, Y_f)) -> random_search((X_f, Y_f),1, New_num, Path, B, R); false); false);
    (A=2->(throw(2, (X, Y),(X_f, Y_f)) -> random_search((X_f, Y_f),1, New_num,Path, B, R); false); false);
    (A=3->(throw(3, (X, Y),(X_f, Y_f)) -> random_search((X_f, Y_f),1,New_num, Path, B, R); false); false);
    (A=4->(throw(4, (X, Y),(X_f, Y_f)) -> random_search((X_f, Y_f),1,New_num, Path, B, R); false); false);
    (A=5->(throw(5, (X, Y),(X_f, Y_f)) -> random_search((X_f, Y_f),1, New_num,Path, B, R); false); false);
    (A=6->(throw(6, (X, Y),(X_f, Y_f)) -> random_search((X_f, Y_f),1,New_num, Path, B, R); false); false);
    (A=7->(throw(7, (X, Y),(X_f, Y_f)) -> random_search((X_f, Y_f),1, New_num,Path, B, R); false); false);
    (A=8->(Yp is Y+1,  random_search((X, Yp),T,New_num, Path, B, R) ); false);
    (A=9->(Xp is X+1,  random_search((Xp, Y),T,New_num, Path, B, R) ); false);
    (A=10->(Ym is Y-1, random_search((X, Ym),T,New_num, Path, B, R) ); false);
    (A=11->(Xm is Y+1, random_search((Xm, Y),T,New_num, Path, B, R) ); false ))))).


main :-
    X is 0,
    Y is 0,
    C is 0,
    T is 0, 
    N is 0,
    Path0 = [[]|_],
    Path1 = [[]|_],
    Path2 = [[]|_],
    Closed =[],
    
    write("Backtracking  "),
    time(backtr((X, Y), T, Closed, Path0, C, R1)->  (write(R1), write(Path0) );write("No solution  ")),
    
    write("Random search  "),
    time(random_search((X, Y),T, N, Path1, C, R2)-> (write(R2), write(Path1));write("No solution  ")),

    write("Improved backtracking  "),
    time(dfs((X,Y), T, Path2, C, R)-> (write(R), write(Path2));write("No solution") ).







