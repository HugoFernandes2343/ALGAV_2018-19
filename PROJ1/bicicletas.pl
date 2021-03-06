


:-dynamic(elemento/1).
:-dynamic(custo/2).

:-dynamic(componente/2).

/*
Especificacao estrutural dos componentes de uma bicicleta tipo.

A bem da simplicidade não são considerados diferentes tipos de
bicicletas (estrada, btt, contra-relógio, etc), por outro foram
consideradas simplificações na estrutura e detalhe.

*/


% componente(ElementoX,ElementoY,Qtd).
% ElementoX utiliza na sua estrutura o ElementoY na quantiadde Qtd
%


componente(bicicleta,quadro,1).
componente(bicicleta,roda,2).
componente(bicicleta,conjunto_travagem,1).
componente(bicicleta,conjunto_transmissao,1).
componente(bicicleta,conjunto_selim,1).
componente(bicicleta,direccao,1).
componente(quadro,tubo_superior,1).
componente(quadro,tubo_diagonal,1).
componente(quadro,tubo_selim,1).
componente(quadro,escora_diagonal,1).
componente(quadro,escora_horizontal,1).
componente(quadro,forqueta_frontal,1).
componente(roda,pneu,1).
componente(roda,aro,1).
componente(roda,valvula,1).
componente(roda,cubo,1).
componente(roda,aperto_rapido,1).
componente(roda,raio,30).
componente(conjunto_travagem,travao_direito,1).
componente(conjunto_travagem,travao_esquerdo,1).
componente(travao_esquerdo,manete,1).
componente(travao_esquerdo,cabo,1).
componente(travao_esquerdo,bicha,1).
componente(travao_esquerdo,disco,1).
componente(travao_esquerdo,pastilha,2).
componente(travao_direito,manete,1).
componente(travao_direito,cabo,1).
componente(travao_direito,bicha,1).
componente(travao_direito,disco,1).
componente(travao_direito,pastilha,2).
componente(conjunto_transmissao,pedaleiro,1).
componente(pedaleiro,pedal,1).
componente(pedaleiro,braco_pedal,1).
componente(pedaleiro,rolamento,1).
componente(pedaleiro,prato,1).
componente(conjunto_transmissao,corrente,1).
componente(conjunto_transmissao,desviador_traseiro,1).
componente(conjunto_transmissao,desviador_dianteiro,1).
componente(conjunto_transmissao,cassete,1).
componente(conjunto_transmissao,mudanças_dianteira,1).
componente(mudanças_dianteira,manete_dianteira,1).
componente(mudanças_dianteira,bicha,1).
componente(mudanças_dianteira,cabo,1).
componente(conjunto_transmissao,mudanças_traseira,1).
componente(mudanças_traseira,manete_traseira,1).
componente(mudanças_traseira,bicha,1).
componente(mudanças_traseira,cabo,1).
componente(conjunto_selim,selim,1).
componente(conjunto_selim,espigao,1).
componente(conjunto_selim,aperto_rapido_selim,1).
componente(direccao,caixa_direccao,1).
componente(direccao,guiador,1).
componente(direccao,avanco_guiador,1).


% 2 utilizamos dois predicados para criar as listas de elemX e elemY e
% ordena-las removendo os elementos duplicados, juntamos com append,
% fazemos sort as listas, e geramos os elementos

elsY(L):-findall(Element,componente(_,Element,_),L1), sort(L1,L).
elsX(L):-findall(Element,componente(Element,_,_),L1), sort(L1,L).
gera([]).
gera([X|L]):-assert(elemento(X)),gera(L).
gerar_elemento():-elsX(X),elsY(Y),append(X,Y,Res1),sort(Res1,Res),gera(Res).



% 3 utilizando o predicado que nos devolve a lista de X, depois usamos
% member para dizer que Elemento tem que pertencer a essa lista, depois
% vamos buscar a lista de Y's e verificamos que Elemento nao pertence

produto_final(Elemento):-elsX(Lx),member(Elemento,Lx), elsY(Ly), \+member(Elemento,Ly).

% 4 utilizando o predicado que nos devolve a lista de Y, usamos member
% para dizer que Elemento tem de pertencer a essa lista, depois vamos
% buscar a lista de X's e verificamos que Elemento nao pertence

produto_base(Elemento):- elsY(Ly), member(Elemento,Ly), elsX(Lx),\+member(Elemento,Lx).

% 5 modificar o produto final removendo o /+ para ver que X tambem
% pertence aos Y's
produto_intermedio(Elemento):-elsX(Lx),member(Elemento,Lx), elsY(Ly), member(Elemento,Ly).

% 6 usamos o algoritimo de BFS para calcular a partir de bicicleta ate
% cada um dos pontos e dps subtraimos os resultados um ao outro e o
% valor absoluto dessa conta 顯 nivel

%BFS algoritmo
bfs(Orig,Dest,Cam):- bfs2(Dest,[[Orig]],Cam).

%condicao final: destino = n󠠠cabe衠do caminho atual
bfs2(Dest,[[Dest|T]|_],Cam):-
    %caminho atual estᡩnvertido
    reverse([Dest|T],Cam).

bfs2(Dest,[LA|Outros],Cam):-
     LA=[Act|_],
     %calcular todos os nos adjacentes nao visitados e
     %gerar um caminho novo c/ cada no e caminho atual
     findall([X|LA],(Dest\==Act, componente(Act,X,_),\+ member(X,LA)),Novos),
     %novos caminhos sao colocados no final da lista
     %p/ posterior exploracao
     append(Outros,Novos,Todos),
     %chamada recursica
     bfs2(Dest,Todos,Cam).


count([],0).
count([_|T],N):- count(T,N1), N is N1+1.

nivel(ElemX,ElemY,Nvl):- produto_final(X),bfs(X,ElemX,CamX),count(CamX,NX), bfs(X,ElemY,CamY),count(CamY,NY) ,Nvl is abs( NX - NY).


%7
elementosProf(_,[],(_,0),(_,0)).
elementosProf(Elem, [H|T], (Max1,Prof1), (Max2,Prof2)):-
    elementosProf(Elem,T,(Max1d,Prof1d),(Max2d,Prof2d)),
    bfs(Elem,H,Cam),
    count(Cam,Cnt),
    (Cnt>Prof1d ->
    (   Max1 = H, Prof1 = Cnt, Max2 = Max2d, Prof2 = Prof2d);
    (   Max1 = Max1d, Prof1 = Prof1d, (Cnt>Prof2d ->
        (   Max2 = H, Prof2 = Cnt);
        (   Max2 = Max2d, Prof2 = Prof2d)))).


dois_mais_profundos(ElementoRaiz,(EMax1,Prof1),(EMax2,Prof2)):-
    listaProds(ElementoRaiz, AllList),
    listaBase(AllList, AllBase,_),
    elementosProf(ElementoRaiz, AllBase, (EMax1,Prof1),(EMax2,Prof2)).




%8

reg_custo(Elemento,Custo):- custo(Elemento,_) -> (retract(custo(Elemento,_)), assert(custo(Elemento,Custo))) ; assert(custo(Elemento,Custo)).

%9
%retorna lista de todos os compenentes e subcomponentes de um produto
list([],[]).
list([H|T],L):-findall(Y, componente(H,Y,_), L1),
    list(L1,L2), list(T,L3),
    append(L2,L3,R1), append([H],R1,L).
listaProds(Elemento,L):- list([Elemento],L).

%filtra lista de elementos sem custo
semCusto([],[]).
semCusto([H|T],LSemCusto):-
    ((\+custo(H,_);custo(H,0))->
    (   LSemCusto = [H|Sem]) ;
    (   LSemCusto = Sem)), semCusto(T, Sem).

%verifica se todos os elementos de uma lista tem custo
verificaCusto([]).
verificaCusto([H|T]):-custo(H,_),verificaCusto(T).

%filtra lista de elementos base
listaBase([],[],[]).
listaBase([H|T],Base,Resto):-
    (produto_base(H) ->
    (   Base = [H|B], listaBase(T,B,Resto));
    (   Resto = [H|R], listaBase(T,Base,R))).

%os componentes base que nao tem custo ficam com o custo a 0
geraCustoBase([]).
geraCustoBase([H|T]):-
    reg_custo(H,0),
    geraCustoBase(T).


%soma custo dos elementos de uma lista
somaCusto(_,[],0).
somaCusto(X,[H|T],Valor):-somaCusto(X,T,Valor1), custo(H,V), componente(X,H,Qtd),
    Vcomp is V*Qtd,
    Valor is Valor1 + Vcomp.

%calcula custo de um produto baseado nos seus componentes
calcCusto([]).
calcCusto([H|T]):- findall(Y,componente(H,Y,_),L),
    ((verificaCusto(L)) ->
    (   somaCusto(H,L,Valor), reg_custo(H,Valor),calcCusto(T)) ;
    (   append(T,[H],L1),calcCusto(L1))).

%apaga o custo dos elementos que tem valor 0
apagaCustos([]).
apagaCustos([H|T]):-(custo(H,0) ->
                    (   retract(custo(H,0)), apagaCustos(T));
                    (   apagaCustos(T))).

calc_custo(Elemento, Valor, ListaSemCusto):-
    %poe em AllChildsList a lista de todos os componentes e subcomponentes do produto Elemento
    listaProds(Elemento,AllChildsList),
    %filtra AllChildsList de forma a ficar em ListaTotalSemCusto os produtos sem custo atribuido
    semCusto(AllChildsList,ListaTotalSemCusto),
    %filtra ListaTotalSemCusto de forma a ficar em ListaSemCusto1 os produtos base sem custo atribuido
    listaBase(ListaTotalSemCusto,ListaSemCusto1,_),
    %Gera um custo 0 para todos os elementos na lista ListaSemCusto1
    geraCustoBase(ListaSemCusto1),
    %filtra AllChildsList de forma a ficar em Resto os produtos sem custo que nao sao base
    listaBase(AllChildsList,_,Resto),
    %para cada elemento da lista Resto calcula o seu custo baseado nos seus subcomponentes
    calcCusto(Resto),
    %poe o custo do Elemento em Valor
    custo(Elemento,Valor),
    %retira repeticoes da lista ListaSemCusto
    sort(ListaSemCusto1,ListaSemCusto),
    %apaga custos de valor 0
    apagaCustos(AllChildsList).

%10
%
somaQtd([_|[]],1).
somaQtd([H,M|T],Soma):-somaQtd([M|T], Soma1), componente(H,M,V), Soma is Soma1*V.

checkCusto(E, Valor):- custo(E,_)-> (custo(E,Valor));(Valor is 0).

constroiListaElementos(_,_,[],[]).
constroiListaElementos(X,Nr,[H|T], L):-constroiListaElementos(X,Nr,T,L1),
    bfs(X,H,Cam),
    somaQtd(Cam,Qtd1),Qtd is Qtd1*Nr,
    checkCusto(H,V1), Custo is V1*Qtd,
    append([[H,Qtd,Custo]],L1,L).

compareList(R, [_,_,C1],[_,_,C2]):- C1>=C2 -> R = < ; R = > .
ordenaLista(L,Res):-predsort(compareList,L,Res).

imprime([]).
imprime([H|T]):-write(H),nl,imprime(T).

lista_materiais(Elemento,Qtd):-
    print(Elemento : Qtd),nl,
    print('-------------------------------------'),nl,
    listaProds(Elemento,AllList),
    listaBase(AllList,ListaBase,_),
    constroiListaElementos(Elemento,Qtd,ListaBase,L),
    ordenaLista(L,Res),imprime(Res).


%11
%Verifica se a lista esta vazia
list_empty([]).

% para cada produto faz um findall dos seus componentes e depois chama
% formatList para essa lista de componentes
formatList([],[]).
formatList([H|T],L):- findall(Y,componente(H,Y,_),L1), formatList(L1,Flist1),
    ((list_empty(Flist1)) ->
    (   L2 = (H)) ;
    (   L2 = (H,Flist1))),
    formatList(T,Flist2), L = [L2|Flist2].
produto_arvore(Elemento,L):- formatList([Elemento],L).


%12

checkQtd(E, Valor):- componente(_,E,_)-> (componente(_,E,Valor));(Valor = ' ').

writeForm(E,Tab,Oi,Ob,Oq):-
    ((Oi = 'off', produto_intermedio(E)) ->
    (   true); ((Ob = 'off', produto_base(E)) ->
               (   true) ; ((Oq = 'on') ->
                           (   tab(Tab), write(E), write('   '),checkQtd(E,Qtd), writeln( Qtd));
                           (   tab(Tab), writeln(E))))).


escreveListaForm([],_,_,_,_).
escreveListaForm([H|T],Tab,Oi,Ob,Oq):-findall(Y,componente(H,Y,_),L1),
    writeForm(H,Tab,Oi,Ob,Oq), Tab1 is Tab+2,
    escreveListaForm(L1,Tab1,Oi,Ob,Oq),
    escreveListaForm(T,Tab,Oi,Ob,Oq).


listar_arvore_indentada(Elemento,Op_pi,Op_pb,Op_qtd):-
    escreveListaForm([Elemento], 0,Op_pi,Op_pb,Op_qtd).



%13 predicado de imprimir

guardarBaseConhecimento(Nome):- tell(Nome), listing, told.
