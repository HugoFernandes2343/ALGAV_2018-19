continente(europa).
continente(africa).
continente(asia).
continente(oceania).
continente(america).

pais(portugal, europa, 11).
pais(alemanha, europa, 200).
pais(franca, europa, 140).
pais(usa, america, 320).
pais(canada, america, 220).
pais(mexico,america, 170).
pais(botswana, africa, 3).

fronteira(alemanha, franca).
fronteira(usa, canada).
fronteira(usa, mexico).

vizinho(P1,P2):- fronteira(P1,P2);fronteira(P2,P1).

comPais(C):- pais(_,C,_).

contSemPaises(C):- continente(C), not(comPais(C)).

semVizinhos(L):- pais(L,_,_), not(vizinho(L,_)).

chegoLaFacil(P1,P2):- vizinho(P1,P2);vizinho(P1,C),vizinho(C,P2).

potencia(_,0,1):-!.
potencia(X,N,P):- N1 is N-1, potencia(X,N1,P1), P is X*P1.

fatorial(0,1):-!.
fatorial(N,F):-N1 is N-1, fatorial(N1,F1), F is N*F1.

somatorio(F, F, F).
somatorio(F, A, X) :- F1 is F+1, F < A, somatorio(F1, A, X0), X is X0 + F. % vai somando 1 a 1 os F's até que F = A.

div( A , B , Q , R ) :- A > 0 , B > 0 , resto( A , B , 0 , Q , R ).                        %A é B sao os numeros que serao usados na divisão dividendo e divisor, Q é o quociente, R é o resto
resto( A , B , Q , Q , A ) :- A < B.
resto( A , B , T , Q , R ) :- A >= B , T1 is T+1 , A1 is A-B , resto( A1 , B , T1 , Q , R ).
% enquanto A for maior ou igual a B a variavel T1 vai ser utilizada para
% guardar o resultado, quando A for menor que B significa que ja nao é
% divisivel pelo B ou seja o numero que sobra em A é o resto% e o numero
% em T é o quociente que passa para a variavel Q quando A passa a ser
% menor que B.

primo(N) :-                 % N é primo
  N > 1,                    % N > 1
  nao_divisivel(N, 2).      % N é nao divisivel por numeros entre 1 e o proprio N, nao inclusive

nao_divisivel(N, D) :-      % N é nao divisivel de D a N-1 se
  N =< D.                   % D >= N

nao_divisivel(N, D) :-      % N é não divisivel de D a N-1 se
  N > D,                    % N > D,
  N mod D =\= 0,            % N é nao divisivel por D
  D1 is D + 1,              % N é nao divisivel de D+1 a N-1
  nao_divisivel(N, D1).

mdc(0, X, X) :-!.
mdc(X, 0, X) :-!.
mdc(X, X, X) :-!.
mdc(A, B, X) :- B>A, Y is B-A, mdc(A, Y, X).
mdc(A, B, X) :- B<A, Y is A-B, mdc(Y, B, X).

% o que este snippet de codigo faz, é vai subtraindo o maior numero ao
% menor até um dos resultados ser 0, quando o resultado for 0 o numero
% diferente de 0 e o X serao iguais por exemplo,
%  1024 , 4096 seria 4096-1024 = 3062; 3062-1024 = 2048; 2048 - 1024 =
%  1024; 1024-1024 = 0, o mdc de 1024 e 4096 é 1024.
