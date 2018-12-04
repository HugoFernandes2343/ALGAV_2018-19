coluna(4).
coluna(5).
coluna(6).
coluna(7).
coluna(8).

jogador(p,b).
jogador(b,p).
input_jogador(x, p).
input_jogador(o, b).

empty_board([[0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0],
             [0,0,0,p,b,0,0,0],
             [0,0,0,b,p,0,0,0],
             [0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0]]).

direcoes([[-1,-1],  %NW
          [-1,0],   %N
          [-1,1],   %NE
          [0,-1],   %W
          [0,1],    %E
          [1, -1],  %SW
          [1,0],    %S
          [1,1]]).  %SE

is_empty([]).

% play
% Start the game.
play :-
    nl,
    write('===================='), nl,
    write('=      Othello     ='), nl,
    write('===================='), nl, nl,
    write('Nota: p começa o jogo'), nl,
    playerMark.

	% playAskColor
% Ask the color for the human player and start the game with it.
playerMark:-
	  repeat,
	  nl, write('Cor do jogador? (x ou o)'), nl,
	  read(Input), nl,
          input_jogador(Input, Player),
          empty_board(B),
	  draw(B), nl,
          play([p, B], Player).



%quando é o jogador a jogar
play([HumanPlayer, Tab], HumanPlayer):- jogadasPossiveis(HumanPlayer, Tab, Poss),
     (is_empty(Poss) ->
     (   writeln('Sem Jogadas Possiveis.'), ProxTab = Tab);
     (
       nl, write('Próxima jogada?'),nl,!,
       repeat,
       (lerJogada(Coord) ->
          ((member(Coord, Poss) ->
              (!)
              ;
              (writeln('Jogada não permitida'), false)
           ))
          ;
          (writeln('Posição Inválida'), false)
       ),
       joga(Coord, HumanPlayer, Tab, ProxTab),
       writeln('O teu tabuleiro: '),
       draw(ProxTab),nl
     )),
     jogador(HumanPlayer, Player),
     (scoreboard(ProxTab)->
       (writeln('Fim do jogo.'),!)
       ;
       (!,play([Player,ProxTab], HumanPlayer) )
     ).





%recebe board, coverte de lista de listas para lista verifica cada casa
calcPontuacao(Tab,PlayerP,PlayerB, CasasLivres):-flatten(Tab,List), calcPontuacao1(List, PlayerP, PlayerB, CasasLivres).
calcPontuacao1([],0,0,0).
calcPontuacao1([H|T], PlayerP, PlayerB, CasasLivres):-calcPontuacao1(T, PlayerP1, PlayerB1, CasasLivres1),
    (H = 0 ->
    (   CasasLivres is CasasLivres1 + 1);
    (   CasasLivres is CasasLivres1)),
    (H = p ->
    (   PlayerP is PlayerP1 + 1);
    (   PlayerP is PlayerP1)),
    (H = b ->
    (   PlayerB is PlayerB1 + 1);
    (   PlayerB is PlayerB1)).

%Recebe pontuacao dos dois jogadores e define o vencedor
decideVencedor(PlayerP, PlayerB, Res):-
    Pont is PlayerB - PlayerP,
    (Pont > 0 ->
    (   Res = 'p ganha!');
    (   (Pont < 0)->
        (   Res = 'b ganha!');
        (   Res = 'Empate!'))
    ).

% read user input and convert from coordinates to numbers
lerJogada([Lin,Col]):-read(Input), string_chars(Input,[Lin1,Col1]), atom_number(Col1,Col),
    linha(Lin1,Lin), coluna(Col).

%joga na posicao dada, false se a posicao nao for valida
joga(Coord, Player, Tab, ProxTab):-
    direcoes(Dir),
    retornaCasa(Coord, Tab, 0),
    joga1(Coord, Dir, Player, Tab, ProxTab1),
    Tab\=ProxTab1,
    pinta(ProxTab1, Player, Coord, ProxTab).
joga1(_, [], _,B,B):-!.
joga1(Coord,[Dir|T], Player, Tab, ProxTab):-
    joga1(Coord, T, Player, Tab, ProxTab1),
    (
     ( retornaCasa(Coord, Tab, 0),
       pintaDir(Coord,  Dir, Player, ProxTab1, ProxTab) )
      ;
      ProxTab = ProxTab1
    ).

pintaDir([Lin, Col], [DirL, DirC], Player, Tab, ProxTab):-
    %coordenadas da nova posicao
    NovaLin is Lin + DirL, NovaCol is Col + DirC,
    %verificar limites das novas coordenadas
    (NovaLin > 0, NovaLin < 9), (NovaCol > 0, NovaCol < 9),
    %casa a seguir tem de tera cor do outro jogador
    jogador(Player, Outro),
    retornaCasa([NovaLin, NovaCol],Tab, Outro),
    %coordenadas da posicao a seguir a nova
    NovaLin1 is NovaLin + DirL, NovaCol1 is NovaCol +DirC,
    %verificar limites
    (NovaLin1 > 0, NovaLin1 < 9), (NovaCol1 > 0, NovaCol1 < 9),
    %se o conteudo da casa for 0 ( vazio ), entao falha
    \+retornaCasa([NovaLin1, NovaCol1], Tab, 0),
    (
	  %se a casa current + Dir + Dir for igual ao jogador entao jogada é possivel, pinta casa current + dir e termina
     ( retornaCasa([NovaLin1, NovaCol1], Tab, Player),
       pinta(Tab, Player, [NovaLin, NovaCol], ProxTab) )
     ;
	 %se for igual ao outro jogador, pinta e tenta mais a frente
     ( retornaCasa([NovaLin1, NovaCol1], Tab, Outro),
       pinta(Tab, Player, [NovaLin, NovaCol], ProxTab1),
       pintaDir([NovaLin, NovaCol], [DirL, DirC], Player, ProxTab1, ProxTab) )
    ).






%show(+Board)
% Show the board to current output.
draw([[A1, A2, A3, A4, A5, A6, A7, A8],
      [B1, B2, B3, B4, B5, B6, B7, B8],
      [C1, C2, C3, C4, C5, C6, C7, C8],
      [D1, D2, D3, D4, D5, D6, D7, D8],
      [E1, E2, E3, E4, E5, E6, E7, E8],
      [F1, F2, F3, F4, F5, F6, F7, F8],
      [G1, G2, G3, G4, G5, G6, G7, G8],
      [H1, H2, H3, H4, H5, H6, H7, H8]]):-
    write('      '),write(' 1   2   3   4   5   6   7   8 '),nl,
    write('     ---------------------------------'), nl,
    write('   '),write('a'), write(' | '),
    show2(A1), write(' | '), show2(A2), write(' | '), show2(A3), write(' | '), show2(A4), write(' | '),
    show2(A5), write(' | '), show2(A6), write(' | '), show2(A7), write(' | '), show2(A8), write(' | '), nl,
    write('     ---------------------------------'), nl,
    write('   '), write('b'), write(' | '),
    show2(B1), write(' | '), show2(B2), write(' | '), show2(B3), write(' | '), show2(B4), write(' | '),
    show2(B5), write(' | '), show2(B6), write(' | '), show2(B7), write(' | '), show2(B8), write(' | '), nl,
    write('     ---------------------------------'), nl,
    write('   '), write('c'), write(' | '),
    show2(C1), write(' | '), show2(C2), write(' | '), show2(C3), write(' | '), show2(C4), write(' | '),
    show2(C5), write(' | '), show2(C6), write(' | '), show2(C7), write(' | '), show2(C8), write(' | '), nl,
    write('     ---------------------------------'), nl,
    write('   '), write('d'), write(' | '),
    show2(D1), write(' | '), show2(D2), write(' | '), show2(D3), write(' | '), show2(D4), write(' | '),
    show2(D5), write(' | '), show2(D6), write(' | '), show2(D7), write(' | '), show2(D8), write(' | '), nl,
    write('     ---------------------------------'), nl,
    write('   '), write('e'), write(' | '),
    show2(E1), write(' | '), show2(E2), write(' | '), show2(E3), write(' | '), show2(E4), write(' | '),
    show2(E5), write(' | '), show2(E6), write(' | '), show2(E7), write(' | '), show2(E8), write(' | '), nl,
    write('     ---------------------------------'), nl,
    write('   '), write('f'), write(' | '),
    show2(F1), write(' | '), show2(F2), write(' | '), show2(F3), write(' | '), show2(F4), write(' | '),
    show2(F5), write(' | '), show2(F6), write(' | '), show2(F7), write(' | '), show2(F8), write(' | '), nl,
    write('     ---------------------------------'), nl,
    write('   '), write('g'), write(' | '),
    show2(G1), write(' | '), show2(G2), write(' | '), show2(G3), write(' | '), show2(G4), write(' | '),
    show2(G5), write(' | '), show2(G6), write(' | '), show2(G7), write(' | '), show2(G8), write(' | '), nl,
    write('     ---------------------------------'), nl,
    write('   '), write('h'), write(' | '),
    show2(H1), write(' | '), show2(H2), write(' | '), show2(H3), write(' | '), show2(H4), write(' | '),
    show2(H5), write(' | '), show2(H6), write(' | '), show2(H7), write(' | '), show2(H8), write(' | '), nl,
    write('     ---------------------------------'), nl.



% show2(+Term)
% Write the term to current outupt
% Replace 0 by ' '.
show2(X) :-
    X = 0, !,
    write(' ').

show2(X) :-
    X = b, !,
    write('O').

show2(X) :-
    X = p, !,
    write('X').

