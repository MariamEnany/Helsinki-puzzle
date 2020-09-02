%This is the final Project done by 4 Team members 
%Edited by Hoda Amr Elhemaly
%Concepts of Programming Languages Spring 2019
 %%%%%The Helsinki Puzzle%%%%%%%
%Given a square grid of size N, where the horizontal rows are numbered 1 to N from top to bottom and the vertical columns are numbered 1 to N from left to right.
%You must place a number in each cell of the N by N grid such that :-
%       Each row is unique.
%       Each row is exactly equal to one of the columns, however, it must not be the column with the same index as the row.
%       If X is the largest number you place in the grid, then you must also place 1,2,...,X-1, where the condition X  N is satisfied.

%We were to solve this puzzle purely through Prolog. We were not allowed to use any clpfd libaries. 
% Our solution utilized both techniques,unification and generate-and-test.


           %grid_build

grid_build(N,M):-
   M=[H|T],
   length( M,N),
   length( H,N),
   grid_build1(N,T).

grid_build1(N,[]).   
grid_build1(N,[H|T]):-
 length( H,N),
 grid_build1(N,T).

member_list([],_). 
member_list([X|Y],F):-
  member(X,F) ,
  member_list(Y,F).

                  %grid_gen

grid_gen(N,M):-
    grid_build(N,M),    
     num_gen(1,N,F),
     gridH(M,F).

gridH([],_).   
gridH([H|T],Y):-
 member_list(H,Y),
     gridH(T,Y).

                      %num_gen

num_gen(X,X,[X]).
num_gen(F,L,[F|T]):-
 F=<L,
 F1 is F+1,
 num_gen(F1,L,T).


get_max([H|T],X):-
  get_max1([H|T],[],[Y|Z]),
   max_list([Y|Z],X).
get_max1([],L,L).
get_max1([H|T],L,R1):-
      max_list(H,M),
      append(L,[M],R), 
      get_max1(T,R,R1).

                   %check_num_grid

all_in_grid([H|T],1).
all_in_grid([H|T],X):-
      X1 is X-1,
      member(X1,H);
       X1 is X-1,
     member(X1,T),
      all_in_grid(T,X1).
      
check_num_grid([H|T]):-
        get_max([H|T],X),
        length([H|T],M),
         X=<M,
        all_in_grid([H|T],X).


distinct_rows([]).
distinct_rows([[H|T]]).
distinct_rows([H1,H2|T]):-
     \+ lists_equal(H1,H2),
      distinct_rows([H1|T]),
      distinct_rows([H2|T]).

lists_equal(L,[]).
lists_equal([H1|T1],[H2|T2]):-
          H1 == H2,
          lists_equal(T1,T2).
 
getFirst([],[]).
getFirst([[H|T]|T2],M):-
  Z=H,
  getFirst(T2,Z2),
  M=[Z|Z2].
deleteFirst([],[]).
deleteFirst([[H|T]|T2],M):-
 Z=T,
 deleteFirst(T2,Z2),
 M=[Z|Z2].

                             %trans

trans([[]|_],[]).
trans(M,M1):-
 M=[H1|T1],
 H1=[X|Y],
 getFirst(M,H),
 deleteFirst(M,D1),
 trans(D1,Tr1),
 M1=[H|Tr1].
  
                  % distinct_columns

distinct_columns(M):-
 trans(M,M1),
 distinct_rows(M1).

insert(X,[],[X]).
insert(X,[H|T],[X,H|T]).
insert(X,[H|T],[H|NT]):- 
  insert(X,T,NT).
perm([],[]).
perm([H|T],L) :- 
 perm(T,P), 
 insert(H,P,L).

                  %acceptable_permutation

acceptable_permutation([],[]).
acceptable_permutation(L,R) :-
 L=[H1|T1],
 R=[H2|T2],
 
 perm(L,R),
 equal(L,R).
equal([],[]).  
equal([H1|T1],[H2|T2]):-
  H1\==H2,
  equal(T1,T2).

equal_lists([],[]).
equal_lists([H1|T1],[H2|T2]):-
   H1 == H2,
   equal_lists(T1,T2).
acceptable_distribution([]).

                          %acceptable_distribution

acceptable_distribution(M):-
  M=[H1|T1],
  trans(M,M1),
  M1=[H2|T2],
  \+ equal_lists(H1,H2),
  acceptable_distribution(T1).

                                      % row_col_match

row_col_match(M):-
 trans(M,M1), 
 row_col_helper1(M,M1).

row_col_helper1([],_).
row_col_helper1([H1|T1],[H2|T2]):-
 member(H1,[H2|T2]),
 row_col_helper1(T1,[H2|T2]).
 
                                               %helsinki

helsinki(N,G):-
 grid_build(N,G),
 grid_gen(N,G),
 check_num_grid(G),
 acceptable_distribution(G),
 distinct_rows(G),
 distinct_columns(G),
 row_col_match(G).
