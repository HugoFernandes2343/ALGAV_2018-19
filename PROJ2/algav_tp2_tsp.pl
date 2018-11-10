
% -----------------------------------------------------------------------
% Trabalho prático: factos de cidades com localização baseada em
% latitude e longitude e predicado auxiliar para calcular a distância
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

%city(name,latitude,longitude)
/*city(a,4,4).
city(b,8,4).
city(c,9,6).
city(d,4,9).
city(e,7,6).*/
city(brussels,50.8462807,4.3547273).
city(tirana,41.33165,19.8318).
city(andorra,42.5075025,1.5218033).
city(vienna,48.2092062,16.3727778).
city(minsk,53.905117,27.5611845).
city(sarajevo,43.85643,18.41342).
city(sofia,42.6976246,23.3222924).
city(zagreb,45.8150053,15.9785014).
city(nicosia,35.167604,33.373621).
city(prague,50.0878114,14.4204598).
/*city(copenhagen,55.6762944,12.5681157).
city(london,51.5001524,-0.1262362).
city(tallinn,59.4388619,24.7544715).
city(helsinki,60.1698791,24.9384078).
city(paris,48.8566667,2.3509871).
city(marseille,43.296386,5.369954).
city(tbilisi,41.709981,44.792998).
city(berlin,52.5234051,13.4113999).
city(athens,37.97918,23.716647).
city(budapest,47.4984056,19.0407578).
city(reykjavik,64.135338,-21.89521).
city(dublin,53.344104,-6.2674937).
city(rome,41.8954656,12.4823243).
city(pristina,42.672421,21.164539).
city(riga,56.9465346,24.1048525).
city(vaduz,47.1410409,9.5214458).
city(vilnius,54.6893865,25.2800243).
city(luxembourg,49.815273,6.129583).
city(skopje,42.003812,21.452246).
city(valletta,35.904171,14.518907).
city(chisinau,47.026859,28.841551).
city(monaco,43.750298,7.412841).
city(podgorica,42.442575,19.268646).
city(amsterdam,52.3738007,4.8909347).
city(belfast,54.5972686,-5.9301088).
city(oslo,59.9138204,10.7387413).
city(warsaw,52.2296756,21.0122287).
city(lisbon,38.7071631,-9.135517).
city(bucharest,44.430481,26.12298).
city(moscow,55.755786,37.617633).
city(san_marino,43.94236,12.457777).
city(edinburgh,55.9501755,-3.1875359).
city(belgrade,44.802416,20.465601).
city(bratislava,48.1483765,17.1073105).
city(ljubljana,46.0514263,14.5059655).
city(madrid,40.4166909,-3.7003454).
city(stockholm,59.3327881,18.0644881).
city(bern,46.9479986,7.4481481).
city(kiev,50.440951,30.5271814).
city(cardiff,51.4813069,-3.1804979).*/

%  dist_cities(brussels,prague,D).
%  D = 716837.
dist_cities(C1,C2,Dist):-
    city(C1,Lat1,Lon1),
    city(C2,Lat2,Lon2),
    distance(Lat1,Lon1,Lat2,Lon2,Dist).

degrees2radians(Deg,Rad):-
	Rad is Deg*0.0174532925.

% distance(latitude_first_point,longitude_first_point,latitude_second_point,longitude_second_point,distance
% in meters)
distance(Lat1, Lon1, Lat2, Lon2, Dis2):-
	degrees2radians(Lat1,Psi1),
	degrees2radians(Lat2,Psi2),
	DifLat is Lat2-Lat1,
	DifLon is Lon2-Lon1,
	degrees2radians(DifLat,DeltaPsi),
	degrees2radians(DifLon,DeltaLambda),
	A is sin(DeltaPsi/2)*sin(DeltaPsi/2)+ cos(Psi1)*cos(Psi2)*sin(DeltaLambda/2)*sin(DeltaLambda/2),
	C is 2*atan2(sqrt(A),sqrt(1-A)),
	Dis1 is 6371000*C,
	Dis2 is round(Dis1).

% distance(50.8462807,4.3547273,50.0878114,14.4204598,D).
% Online: http://www.movable-type.co.uk/scripts/latlong.html
%


%----------------------------Iteração 1------------------------------------

/*
 usa findall com o permutation para gerar uma lista de todos os caminhos
 possiveis, so funciona pois todas as cidades estao interligadas.

baralha(Baralhar,Resultado):-findall(X,permutation(Baralhar,X),Resultado).

cidades(Origem,ListaCidades):-findall(X,(city(X,_,_),X \= Origem),ListaCidades1),baralha(ListaCidades1,ListaCidades).

processa(_,[],[],0).
processa(Origem, [H|T], ListaCaminhos,Distancia):-
    processa(Origem,T,ListaCaminhos1,Distancia1),
    append([Origem],H,Temp1),append(Temp1,[Origem],CamCompleto),
    calcCusto(CamCompleto,Valor),
    ((Distancia1 = 0; Valor < Distancia1)->
    (   ListaCaminhos = H, Distancia = Valor) ;
    (   ListaCaminhos = ListaCaminhos1, Distancia = Distancia1)).
*/

%bfs

bnb(Orig,Dest,Cam,Custo):- bnb2(Dest,[(0,[Orig])],Cam,Custo).
%condicao final: destino = nó à cabeça do caminho actual

bnb2(Dest,[(Custo,[Dest|T])|_],Cam,Custo):-
%caminho actual está invertido
reverse([Dest|T],Cam).

bnb2(Dest,[(Ca,LA)|Outros],Cam,Custo):-
    LA=[Act|_],
%calcular todos os nodos adjacentes nao visitados e
%gerar tuplos c/ um caminho novo juntado o nodo + caminho actual
% o custo de cada caminho é o custo acumulado + peso do ramo
    findall((CaX,[X|LA]),(Dest\==Act,dist_cities(Act,X,CustoX),\+ member(X,LA),CaX is CustoX + Ca),Novos),
%os novos caminhos sao adicionados aos caminhos não explorados
    append(Outros,Novos,Todos),
%a ordenação (não sendo o mais eficiente) garante que
% o melhor caminho fica na primeira posição
    sort(Todos,TodosOrd),
%chamada recursiva
    bnb2(Dest,TodosOrd,Cam,Custo).

%calcula o custo de um caminho
calcCusto([_|[]],0).
calcCusto([H,S|T],Valor):- calcCusto([S|T],Valor1), dist_cities(H,S, Dist), Valor is Valor1 + Dist.

% retorna uma lista de cidades, uma lista de cidades excepto a
% indicada, uma lista de cidades excepto as indicadas numa lista
cidades(Lista):-findall(X,city(X,_,_),Lista).
cidades(Lista, Cidade):-findall(X,(city(X,_,_),X \= Cidade),Lista).
cidadesL(Lista, Excl):-findall(X,(city(X,_,_), \+member(X,Excl)),Lista).

%encontra numa lista a cidade mais proxima da indicada
cidMaisProxima(_,[],_,0).
cidMaisProxima(Cid,[H|T],Res,Dist):- cidMaisProxima(Cid,T,Res1,Dist1), dist_cities(Cid,H,D),
    ((D < Dist1; Dist1 = 0) ->
    (   Dist = D, Res =H);
    (   Dist = Dist1, Res = Res1)).

% usa bnb para procurar todos os caminhos possiveis, filtrando os que
% nao passam em todas as cidades,
caminho(Orig,Dest,Cam,Custo):- cidades(L), length(L,Nr),
    findall(Cam1,(bnb(Orig,Dest,Cam1,_),length(Cam1,Nr)),[CamSemFim|_]),
    append(CamSemFim,[Orig],Cam),
    calcCusto(Cam,Custo).

tsp1(Origem, ListaCaminhos, Distancia):-
    %Poe em Lista todas as cidades possiveis menos a original
    cidades(Lista,Origem),
    %ve qual a cidade mais proxima da Inicial
    cidMaisProxima(Origem,Lista,CidMaisProx,_),
    %faz o caminho inicial-(todas)-cidadeMaisProxima-inicial e calcula a distancia
    caminho(Origem, CidMaisProx, ListaCaminhos, Distancia),!.

it1Tempo(Origem,Lista,Dist):-time(tsp1(Origem,Lista,Dist)),!.

/*
 * Analise tempo it1
 *  5 cidades - 0.000s
 *  6 cidades - 0.003s
 *  7 cidades - 0.026s
 *  8 cidades - 0.395s
 *  9 cidades - 21.946s
 * 10 cidades - 2187.965s/36,4min
*/


%----------------------------Iteração 2------------------------------------

caminhoMaisProx(_,[],[]).
caminhoMaisProx(Cid,CidadesAVisitar,Caminho):-
    cidMaisProxima(Cid,CidadesAVisitar,Cid1,_),
    findall(X,(member(X,CidadesAVisitar), X \== Cid1),Lista),
    caminhoMaisProx(Cid1,Lista,Caminho1), append([Cid1],Caminho1,Caminho).


tsp2(Origem, ListaCaminhos, Dist):-
    cidades(ListaCidades,Origem),
    caminhoMaisProx(Origem,ListaCidades,CaminhoInc),
    append([Origem],CaminhoInc,L1),
    append(L1,[Origem],ListaCaminhos),
    calcCusto(ListaCaminhos,Dist),!.


%----------------------------Iteração 3------------------------------------


% Given three colinear points p, q, r, the function checks if
% point q lies on line segment 'pr'
%onSegment(P, Q, R)
onSegment((PX,PY), (QX,QY), (RX,RY)):-
    QX =< max(PX,RX),
    QX >= min(PX,RX),
    QY =< max(PY,RY),
    QY >= min(PY,RY).

% To find orientation of ordered triplet (p, q, r).
% The function returns following values
% 0 --> p, q and r are colinear
% 1 --> Clockwise
% 2 --> Counterclockwise
orientation((PX,PY), (QX,QY), (RX,RY), Orientation):-
	Val is (QY - PY) * (RX - QX) - (QX - PX) * (RY - QY),
	(Val == 0, !, Orientation is 0;
	Val >0, !, Orientation is 1;
        Orientation is 2).
orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4):-
    orientation(P1, Q1, P2,O1),
    orientation(P1, Q1, Q2,O2),
    orientation(P2, Q2, P1,O3),
    orientation(P2, Q2, Q1,O4).

% The main function that returns true if line segment 'p1q1'
% and 'p2q2' intersect.
doIntersect(P1,Q1,P2,Q2):-
    % Find the four orientations needed for general and
    % special cases
	orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4),
	(
    % General case
    O1 \== O2 , O3 \== O4,!;
    % Special Cases
    % p1, q1 and p2 are colinear and p2 lies on segment p1q1
    O1 == 0, onSegment(P1, P2, Q1),!;
    % p1, q1 and p2 are colinear and q2 lies on segment p1q1
    O2 == 0, onSegment(P1, Q2, Q1),!;
    % p2, q2 and p1 are colinear and p1 lies on segment p2q2
    O3 == 0, onSegment(P2, P1, Q2),!;
     % p2, q2 and q1 are colinear and q1 lies on segment p2q2
    O4 == 0, onSegment(P2, Q1, Q2),!).


%----------------------------------------------------------------------------------------------------
% rGraph(Origin,UnorderedListOfEdges,OrderedListOfEdges)
%
% Examples:
% ---------
% ?- rGraph(a,[[a,b],[b,c],[c,d],[e,f],[d,f],[e,a]],R).
%
% ?- rGraph(brussels,[[vienna, sarajevo], [sarajevo, tirana],[tirana,sofia], [sofia, minsk], [andorra,brussels],[brussels,minsk],[vienna,andorra]],R).
%
rGraph(Orig,[[Orig,Z]|R],R2):-!,
	reorderGraph([[Orig,Z]|R],R2).
rGraph(Orig,R,R3):-
	member([Orig,X],R),!,
	delete(R,[Orig,X],R2),
	reorderGraph([[Orig,X]|R2],R3).
rGraph(Orig,R,R3):-
	member([X,Orig],R),
	delete(R,[X,Orig],R2),
	reorderGraph([[Orig,X]|R2],R3).


reorderGraph([],[]).
reorderGraph([[X,Y],[Y,Z]|R],[[X,Y]|R1]):-
	reorderGraph([[Y,Z]|R],R1).
reorderGraph([[X,Y],[Z,W]|R],[[X,Y]|R2]):-
	Y\=Z,
	reorderGraph2(Y,[[Z,W]|R],R2).
reorderGraph2(_,[],[]).
reorderGraph2(Y,R1,[[Y,Z]|R2]):-
	member([Y,Z],R1),!,
	delete(R1,[Y,Z],R11),
	reorderGraph2(Z,R11,R2).
reorderGraph2(Y,R1,[[Y,Z]|R2]):-
	member([Z,Y],R1),
	delete(R1,[Z,Y],R11),
	reorderGraph2(Z,R11,R2).


converteSegmentos([_|[]], []).
converteSegmentos([H,S|T], Res):-converteSegmentos([S|T],Res1),append([[H,S]],Res1,Res).
converteCaminhos([[H,T]],[H,T]).
converteCaminhos([[H1|_]|T],Res):-converteCaminhos(T,Res1), append([H1],Res1,Res).

cruzaVerif([H,T],L):-(member(H,L);member(T,L)).

cruza([Seg1a,Seg1b],[Seg2a,Seg2b]):-
    city(Seg1a,Seg1ax,Seg1ay),
    city(Seg1b,Seg1bx,Seg1by),
    city(Seg2a,Seg2ax,Seg2ay),
    city(Seg2b,Seg2bx,Seg2by),
    (doIntersect((Seg1ax,Seg1ay),(Seg1bx,Seg1by),(Seg2ax,Seg2ay),(Seg2bx,Seg2by)) ->
    (   cruzaVerif([Seg1a,Seg1b],[Seg2a,Seg2b]) -> (!,false);(!,true));
    (   !,false)).

% verifica se um segmento cruza com algum segmento de uma lista, retorna
% false se nao cruzar e true se cruzar
verif(_,[],[]):-!,false.
verif(Seg,[H|T],Res):-
    (cruza(Seg,H) ->
    (   !,Res = [Seg,H],true);
    (   verif(Seg,T,Res))).
% verifica todos os segmentos de uma lista, os primeiros segmentos
% cruzados que encontrar sao guardados
verificaCaminho([],_):-!,false.
verificaCaminho([H,S|T],Res):-
    (verif(H,[S|T],Res1) ->
    (   !,Res = Res1,true);
    (   verificaCaminho([S|T],Res))).

%verifica se o segmento dado exista na lista Excl
naoMembro([H,T],Excl):-(\+member([H,T],Excl), \+member([T,H],Excl)).

% resolve cruzamentos, começa por pegar na primeira cidade do segmento
% A, verifica qual das cidades do Segmento B é mais proxima e gera 1 de
% 2 combinacoes possiveis
% De seguida verifica se o par de segmentos gerado nao existe no
% caminho, usando a 2 combinaçao possivel se ja existir
resolveCruzamentos([[H1,T1],[H2,T2]],Excl,Res):-
    dist_cities(H1,H2,C1),
    dist_cities(H1,T2,C2),
    (C1<C2 ->
    (   (naoMembro([H1,H2],Excl), naoMembro([T1,T2],Excl)) ->
        (   Res=[[H1,H2],[T1,T2]]);
        (   Res=[[H1,T2],[H2,T1]]));
    (   (naoMembro([H1,T2],Excl), naoMembro([H2,T1],Excl)) ->
        (   Res=[[H1,T2],[H2,T1]]);
        (   Res=[[H1,H2],[T1,T2]]))).

% percorre o caminho verificando se existe cruzamentos, caso exista
% resolve com o predicado acima e chama-se a si mesmo outra vez com o
% novo caminho
constroi(Cam,Res):-
    (verificaCaminho(Cam,Seg) ->
    (   resolveCruzamentos(Seg,Cam,Resolv), findall(X,(member(X,Cam),\+member(X,Seg)),PathInc), append(PathInc,Resolv,Res1), constroi(Res1,Res));
    (   !,Res=Cam,true)).


tsp3(Cid,Cam,Dist):- tsp2(Cid,Path1,_),
    converteSegmentos(Path1,Path),
    constroi(Path,Res),
    (rGraph(Cid,Res,Cam1) ->
    (   converteCaminhos(Cam1,Cam));
    (   Cam1 = Res, converteCaminhos(Cam1,Cam))),
    calcCusto(Cam,Dist),!.




%----------------------------Iteração4-------------------------------------

%parametros


geracoes(200).
populacao(200).
crossover(0.7).
mutation(0.3).


gera(Origem):-
    %gera populacao inicial
    gera_populacao(Origem,Pop),
    %verifica o nr das geracoes
    geracoes(Grc),
    %passa para a geracao seguinte
    gera(Pop,Grc,0,_,0).


gera(_,0,_,_,_):-!.
gera(Pop,Geracoes,Contador,MelhorCamSempre,MelhorDistSempre):-
    %avalia a populacao currente
    avalia_populacao(Pop,[[Dist,Cam]|_], Media,Pop1,PopProb),
    %verifica se o recorde de melhor distancia foi batido
    ((Dist<MelhorDistSempre;MelhorDistSempre==0)->
    (   MelhorCamSempre1 =Cam, MelhorDistSempre1 = Dist);
    (   MelhorCamSempre1=MelhorCamSempre, MelhorDistSempre1=MelhorDistSempre)),
    %imprime valores
    write('Geracao: '), writeln(Contador),
    write('Media desta geracao: '), writeln(Media),
    write('Melhor Caminho desta Geracao: '), writeln(Cam),
    write('Melhor Distancia desta Geracao: '), writeln(Dist),
    writeln('---------------------------------'),
    write('Melhor Caminho: '), writeln(MelhorCamSempre1),
    write('Melhor Distancia: '), writeln(MelhorDistSempre1),nl,
    %gera nova geracao
    gera_geracao(Pop1,PopProb,NewPop),
    %diminui contador
    Geracoes1 is Geracoes - 1,
    %aumenta nr de geracoes
    Contador1 is Contador + 1,!,
    %repete para a nova geracao
    gera(NewPop,Geracoes1,Contador1,MelhorCamSempre1,MelhorDistSempre1).



%--------------Gera Populacao Inicial--------------

gera_populacao(Origem,Pop):-
    %verifica qual o tamanho de uma populacao
    populacao(TamPop),
    %ve quantas cidades sao usadas
    findall(City,city(City,_,_),ListaCidades),
    length(ListaCidades,NrCidades),
    %gera populacao
    gera_populacao(Origem,TamPop,ListaCidades,NrCidades,Pop).

%gera Populacao inicial, usa como um dos membros o resultado da it3
gera_populacao(Origem,1,_,_,Pop):-tsp3(Origem,Cam,_),Pop = [Cam],!.
%gera_populacao(_,0,_,_,[]):-!.
gera_populacao(Origem,TamPop,ListaCidades,NrCidades,Pop):-
    TamPop1 is TamPop-1,
    gera_populacao(Origem,TamPop1,ListaCidades,NrCidades,Pop1),
    %gera_individuo(Origem,Pop,Ind),
    gera_individuo(Origem,Ind),
    append([Ind],Pop1,Pop).

%gera Individuo de forma random
gera_individuo(Origem,Ind):-
    findall(City,(city(City,_,_), City\==Origem), CidadesMenosOriginal),
    random_permutation(CidadesMenosOriginal,Inc),
    append([Origem|Inc],[Origem],Ind).

%--------------Avalia Populacao--------------
% comparacao para ordenar a lista de caminhos de forma ao mais curto
% ficar em 1º lugar
compareList(R, [C1,_],[C2,_]):- C1>=C2 -> R = > ; R = < .
ordenaLista(L,Res):-predsort(compareList,L,Res).

% avalia uma populacao, recebe uma lista de listas de caminhos e calcula
% a distancia de cada um deles, construindo uma lista (Res) em que cada
% elemento tem o formato [Dist,[Cam]], assim como a soma total da
% distancia de todos os caminhos, a soma de todos os fitness e o nr de
% caminhos
avalia([],[],0,0,0).
avalia([H|T],Res,SomaDist,SomaFit,Nr):-avalia(T,Res1,SomaDist1,SomaFit1,Nr1), calcCusto(H,Dist),
    append(Res1,[[Dist,H]],Res), SomaDist is SomaDist1 + Dist, Nr is Nr1+1, FitCurr is 1/Dist, SomaFit is SomaFit1 + FitCurr.

% normaliza a lista, transforma a distancia de cada caminho na sua
% probabilidade de ser escolhido. Retorna uma lista de caminhos e uma
% lista de probabilidades
normaliza([],_,[],[]).
normaliza([[Dist,S]|T],Fitness,ListaPop,ListaProb):-
    normaliza(T,Fitness,ListaPop1,ListaProb1),
    FitCurr is 1 / Dist, %Fitness é maior quanto mais baixa for a distancia
    Norm is FitCurr / Fitness,
    append([S],ListaPop1,ListaPop),
    append([Norm],ListaProb1,ListaProb).

%
avalia_populacao(Pop, PopAval, Media,ListaPop,ListaProb):-
    avalia(Pop,PopNotSort,SomaDist,SomaFit,Nr),
    Media is SomaDist / Nr,
    ordenaLista(PopNotSort,PopAval),
    normaliza(PopAval,SomaFit,ListaPop,ListaProb).


%--------------Gerar Descendentes--------------


% Escolhe um elemento de uma lista dada a sua probabilidade, passada
% noutra lista.
% (stackoverflow.com/questions/50250234)
choice([X|_], [P|_], Cumul, Rand, X) :-
    Rand < Cumul + P.
choice([_|Xs], [P|Ps], Cumul, Rand, Y) :-
    Cumul1 is Cumul + P,
    Rand >= Cumul1,
    choice(Xs, Ps, Cumul1, Rand, Y).
choice([X], [P], Cumul, Rand, X) :-
    Rand < Cumul + P.
choice(Xs, Ps, Y) :- random(R), choice(Xs, Ps, 0, R, Y).


% gera descendentes , verifica a probabilidade de crossover e de
% mutacao, escolhe um pai da geracao anterior com base na sua fitness,
% testa a probabilidade de haver crossover e caso haja escolhe um
% segundo pai para fazer crossover. Caso falhe entao testa a
% rpobabilidade de haver mutacao. Em ultimo caso e falhando as duas
% probabilidades acima copia esse parente da geracao anterior para a
% proxima
gera_descendentes(0,_,_,[]):-!.
gera_descendentes(Nr,Pais,Prob,NovaPop):-
    crossover(CrossProb),
    mutation(MutProb),
    Nr1 is Nr-1,
    gera_descendentes(Nr1,Pais,Prob,NovaPop1),
    choice(Pais,Prob,A),
    (maybe(CrossProb)->
    (   choice(Pais,Prob,B), crossover(A,B,Desc));
    (   (maybe(MutProb) ->
        (   mutateCam(A,Desc));
        (   Desc = A)))),
    append([Desc],NovaPop1,NovaPop).

%
gera_geracao(Pais,Prob,NovaPop):-
        populacao(Nr),
        gera_descendentes(Nr,Pais,Prob,NovaPop).

% crossover e feito gerando um nr entre 1 e tamanho maximo do caminho.
% Depois seleciona do principio do parente A ate ao nr gerada,
% preenchendo o resto do novo caminho com o resto do parente B
crossover([H|T],B,Res):-
    cidades(L),length(L,Max1),Max is Max1 - 1,
    random(1,Max,CrossPoint),
    sublist([H|T],1,CrossPoint,Inc),
    findall(X,(member(X,B),\+member(X,Inc)),Resto),
    append(Inc,Resto,Res1),
    append(Res1,[H],Res).


%retorna uma SubLista no intervalo dado
%(stackoverflow.com/questions/20765479)
sublist(List, From, Count, SubList) :-
    To is From + Count - 1,
    findall(E, (between(From, To, I), nth1(I, List, E)), SubList).



%--------------Mutation--------------

%troca dois elementos de uma lista dando o seu indice
%(stackoverflow.com/questions/26834387)
swap(As,I,J,Cs) :-
   same_length(As,Cs),
   append(BeforeI,[AtI|PastI],As),
   append(BeforeI,[AtJ|PastI],Bs),
   append(BeforeJ,[AtJ|PastJ],Bs),
   append(BeforeJ,[AtI|PastJ],Cs),
   length(BeforeI,I),
   length(BeforeJ,J).

%gera dois nr entre 1 e Max
gera2Nr(Max,A,B):- findall(X,(between(1,2,_),random(1,Max,X)), [A,B]).
% usa predicado acima para gerar dois numeros e depois troca os dois
% elementos que estao nesses indices
mutateCam(Cam,New):-cidades(L),length(L,Max),gera2Nr(Max,A,B),swap(Cam,A,B,New).

mutateList(_,[],[]).
mutateList(Prob,[H|T],NewPop):-mutateList(Prob,T,NewPop1),
    (maybe(Prob) ->
    (   mutateCam(H,New), append([New],NewPop1,NewPop));
    (   append([H],NewPop1,NewPop))).

mutate(Pop,NewPop):-
    mutation(MutProb),
    mutateList(MutProb,Pop,NewPop).


