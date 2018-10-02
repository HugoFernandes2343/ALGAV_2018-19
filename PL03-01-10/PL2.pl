%1.a) Média de lista de inteiros:

conta([],0).
conta([_|L],C):-conta(L,C1), C is C1+1.

soma([],0).
soma([X|L],S):-soma(L,S1), S is S1+X.

media([X|L]):- soma([X|L],S), conta([X|L],C), Res is S/C, write('Média='), write(Res).

%1.b) Menor valor de uma lista

min([Min],Min).
min([L,R|N],Min):- L<R ,min([L|N],Min).
min([L,R|N],Min):- L>=R ,min([R|N],Min).



%1.c) Pares e Impares

pi([],P,I):-!, conta(P,P1), conta(I,I1),
    write(('Pares'=P1)), nl,
    write(('Impares'=I1)).
pi([X|L],P,I):- 0 is X mod 2, pi(L,[X|P],I).
pi([X|L],P,I):- 1 is X mod 2, pi(L, P, [X|I]).

%1.d) Elementos repetidos

verifica(E,[E|_]).
verifica(_,[X|L]):- verifica(X,L).
elRepetidos([X|L]):- verifica(X,L).

%1.e) Menor elemento da lista para a frente

menorFrente(L):-min(L,Min), delete_one(Min,L,Res), write([Min|Res]).

%1.f) Concatenar 2 listas

concat(L,A,X):-append(L,A,X).

%1.g) Linearizar uma lista

lin([],[]):-!.
lin([Head|Tail],R):-!,lin(Head,H1),lin(Tail,T1),append(H1,T1,R).
lin(L,[L]).

%1.h) apagar primeira ocurrencia

delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail).
delete_one(Term, [Head|Tail], [Head|Result]) :-  delete_one(Term, Tail, Result).

%1.I) eleminar todas as ocurrencias,

delMember(_, [], []).
delMember(X, [X|Xs], Y) :-
    delMember(X, Xs, Y).
delMember(X, [T|Xs], [T|Y]) :-
    dif(X, T),
    delMember(X, Xs, Y).


% 1.j)substituir (X - elemento a ser substituido, Y - elemento que vai
% substituir X)

sub(_,_,[],[]):-!.
sub(X,Y,[X|Xs],R):-!,sub(X,Y,Xs,R2), append([Y],R2,R).
sub(X,Y,[T|Xs],R):-!,sub(X,Y,Xs,R2), append([T],R2,R).

%1.k)Inserir um elemento numa dada posição numa lista


element_at(E, 0, [_|Ls], [E|Ls]).
element_at(E, s(X), [L|Ls0], [L|Ls]):-element_at(E, X, Ls0, Ls).


%1.l)Inverter

inv([],[]).
inv([H|T],L):-inv(T,R),append(R,[H],L).
