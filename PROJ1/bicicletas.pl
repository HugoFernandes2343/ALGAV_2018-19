:-dynamic(elemento/1).
:-dynamic(custo/2).

:-dynamic(componente/2).

/*
Especificacao estrutural dos componentes de uma bicicleta tipo.

A bem da simplicidade nÃ£o sÃ£o considerados diferentes tipos de
bicicletas (estrada, btt, contra-relÃ³gio, etc), por outro foram
consideradas simplificaÃ§Ãµes na estrutura e detalhe.

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
