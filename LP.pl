:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas

% Este predicado define a vizinhanca de uma celula (L, C).
%
% Argumentos:
%   - (L, C): As coordenadas da celula.
%   - V: A lista de coordenadas que representa a vizinhanca.
vizinhanca((L, C), V):-
    L_Acima is L - 1, 
    C_Esquerda is C - 1,
    C_Direita is C + 1,
    L_Abaixo is L + 1,
    append([(L_Acima, C), (L, C_Esquerda)], [(L, C_Direita), (L_Abaixo, C)], V).

% Predicado que calcula a vizinhanca alargada de uma celula numa grelha.
%
% Argumentos:
%   - (L, C): As coordenadas da celula.
%   - VA: A lista de coordenadas que representa a vizinhanca alargada.
vizinhancaAlargada((L, C), VA):-
    L_Acima is L - 1,
    C_Esquerda is C - 1,
    C_Direita is C + 1,
    L_Abaixo is L + 1,
    append([(L_Acima, C_Esquerda), (L_Acima, C), (L_Acima, C_Direita), (L, C_Esquerda)],
        [(L, C_Direita), (L_Abaixo, C_Esquerda), (L_Abaixo, C), (L_Abaixo, C_Direita)], VA).

% Este predicado retorna todas as celulas de um tabuleiro.
%
% Argumentos:
%   - Tabuleiro: O tabuleiro representado como uma lista de listas.
%   - Celulas: A lista de coordenadas que representa todas as celulas do tabuleiro.
todasCelulas(Tabuleiro, Celulas):-
    length(Tabuleiro, Tamanho),
    findall((Linha, Coluna), (between(1, Tamanho, Linha), between(1, Tamanho, Coluna)), Celulas).

% Este predicado retorna todas as celulas de um tabuleiro que contem um determinado objeto ou uma variavel.
%
% Argumentos:
%   - Tabuleiro: O tabuleiro representado como uma lista de listas.
%   - Celulas: A lista de coordenadas que representa as celulas que contem o objeto.
%   - Objecto: O objeto a ser procurado nas celulas do tabuleiro.
todasCelulas(Tabuleiro, Celulas, Objecto) :-
    nonvar(Objecto),
    findall((Linha, Coluna), (nth1(Linha, Tabuleiro, ListaLinha), nth1(Coluna, ListaLinha, Resultado), Resultado == Objecto), Celulas).
todasCelulas(Tabuleiro, Celulas, Objecto) :-
    var(Objecto),
    findall((Linha, Coluna), (nth1(Linha, Tabuleiro, ListaLinha), nth1(Coluna, ListaLinha, Vazio), var(Vazio)), Celulas).


% Este predicado eh uma funcao extra que calcula a contagem de objetos de um tabuleiro
% nas linhas de uma matriz.
%
% Argumentos:
%   - Tabuleiro: O tabuleiro representado como uma lista de listas.
%   - ContagemLinhas: A lista que armazena a contagem de objetos em cada linha do tabuleiro.
%   - ContagemColunas: A lista que armazena a contagem de objetos em cada coluna do tabuleiro.
%   - Objecto: O objeto a ser contado no tabuleiro.
calculaObjectos([],[],_).
calculaObjectos([PrimeiraLinha|Resto], [Numero|ContagemLinhas], Objecto):-
	nonvar(Objecto),
    findall(Elemento, (member(Elemento, PrimeiraLinha), Elemento == Objecto), TodosObjectos),
    length(TodosObjectos, Numero),
    calculaObjectos(Resto, ContagemLinhas, Objecto).
calculaObjectos([PrimeiraLinha|Resto], [Numero|ContagemLinhas], Objecto):-
	var(Objecto),
    findall(Elemento, (member(Elemento, PrimeiraLinha), var(Elemento)), TodosObjectos),
    length(TodosObjectos, Numero),
    calculaObjectos(Resto, ContagemLinhas, Objecto).

% Este predicado calcula a contagem de objetos de um tabuleiro, tanto nas linhas quanto nas colunas.
%
% Argumentos:
%   - Tabuleiro: O tabuleiro representado como uma lista de listas.
%   - ContagemLinhas: A lista que armazena a contagem de objetos em cada linha do tabuleiro.
%   - ContagemColunas: A lista que armazena a contagem de objetos em cada coluna do tabuleiro.
%   - Objecto: O objeto a ser contado no tabuleiro.
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto):-
    transpose(Tabuleiro, TabuleiroTransposto), % O transpose eh usado para calcular os objetos nas 
    calculaObjectos(Tabuleiro, ContagemLinhas, Objecto), % colunas como se fosse uma linha
    calculaObjectos(TabuleiroTransposto, ContagemColunas, Objecto).

% Este predicado verifica se uma celula em um tabuleiro esta vazia.
%
% Argumentos:
%   - Tabuleiro: O tabuleiro representado como uma lista de listas.
%   - (L, C): As coordenadas da celula.
celulaVazia(Tabuleiro, (L, C)):-
    length(Tabuleiro, Comprimento),
    (L<1; L>Comprimento; C<1; C>Comprimento),!. % Verifica se as coordenadas estao dentro dos limites do tabuleiro.
celulaVazia(Tabuleiro, (L, C)):-
    nth1(L, Tabuleiro, ListaLinha),
    nth1(C, ListaLinha, Vazio),
    var(Vazio). % Verifica se a celula esta vazia (variavel nao instanciada).

% Este predicado insere um objeto numa celula do tabuleiro, desde que a celula esteja vazia.
%
% Argumentos:
%   - Tabuleiro: O tabuleiro representado como uma lista de listas.
%   - TendaOuRelva: O objeto a ser inserido na celula.
%   - (L, C): As coordenadas da celula.
insereObjectoCelula(Tabuleiro, _, (L, C)):-
    \+ celulaVazia(Tabuleiro, (L, C)), !.
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) :-
    nth1(L, Tabuleiro, LinhaAntiga),
    nth1(C, LinhaAntiga, Elem),
    Elem = TendaOuRelva.

% Este predicado insere um objeto entre duas posicoes em uma linha do tabuleiro.
%
% Argumentos:
%   - Tabuleiro: O tabuleiro representado como uma lista de listas.
%   - TendaOuRelva: O objeto a ser inserido nas celulas.
%   - (L, C1): As coordenadas da primeira celula.
%   - (L, C2): As coordenadas da ultima celula.
insereObjectoEntrePosicoes([], _, _, _).
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    C1 < C2,
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C1)),
    ProximoC is C1 + 1,
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, ProximoC), (L, C2)).
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    C1 = C2,
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C1)).

% Este predicado eh uma funcao extra que insere um objeto em uma lista de listas de acordo com uma lista de indices.
%
% Argumentos:
%   - T: A lista de listas original.
%   - ListaIndices: A lista de indices que determina onde o objeto serah inserido.
%   - Tamanho: O tamanho do tabuleiro.
%   - Objecto: O objeto a ser inserido.
%   - Resultado: A lista de listas resultante apos a insercao do objeto.
insereObjectoBaseadoLista(T, [], _, _, T).
insereObjectoBaseadoLista(T, [Primeiro|Resto], Tamanho, Objecto, Resultado) :-
    insereObjectoEntrePosicoes(T, Objecto, (Primeiro, 1), (Primeiro, Tamanho)),
    Resultado = T,
    insereObjectoBaseadoLista(T, Resto, Tamanho, Objecto, Resultado).

% Este predicado define a regra "relva" que verifica se uma celula contem relva.
%
% Argumentos:
%   - Puzzle: As informacoes do puzzle.
relva((T, L, C)):-
    calculaObjectosTabuleiro(T, ContagemLinhas, ContagemColunas, t),
    findall(Indice, (nth1(Indice, ContagemLinhas, E1), nth1(Indice, L, E2), E1 == E2), IndicesLinha),
    length(T, Tamanho),
    insereObjectoBaseadoLista(T, IndicesLinha, Tamanho, r, _),
    transpose(T, TTransposto),
    findall(Indice, (nth1(Indice, ContagemColunas, E1), nth1(Indice, C, E2), E1 == E2), IndicesColuna),
    insereObjectoBaseadoLista(TTransposto, IndicesColuna, Tamanho, r, _),
    transpose(TTransposto, Reverter),
    T = Reverter.

% Este predicado eh uma funcao extra que verifica se uma celula contem relva ou estah vazia.
%
% Argumentos:
%   - T: O tabuleiro representado como uma lista de listas.
%   - ListaCoordenadas: A lista de coordenadas a serem verificadas.
relvaOuVazio(_, []).
relvaOuVazio(T, [(L, C)|Resto]):-
    (celulaVazia(T, (L, C)) ; nth1(L, T, ListaLinha), nth1(C, ListaLinha, r)), !,
    relvaOuVazio(T, Resto).
    

% Este predicado eh uma funcao extra que filtra as coordenadas que estao dentro do tabuleiro.
%
% Argumentos:
%   - Lista: A lista de coordenadas a serem filtradas.
%   - Tamanho: O tamanho do tabuleiro.
%   - Resultado: A lista de coordenadas resultante apos a filtragem.
filtrarCoordenadas(Lista, Tamanho, Resultado) :-
    exclude(coordenadaForaTabuleiro(Tamanho), Lista, Resultado).

% Este predicado eh uma funcao extra que verifica se uma coordenada estah fora do tabuleiro.
%
% Argumentos:
%   - Tamanho: O tamanho do tabuleiro.
%   - (X, Y): As coordenadas a serem verificadas.
coordenadaForaTabuleiro(Tamanho, (X, Y)) :-
    (X > Tamanho ; X < 1 ; Y >  Tamanho; Y < 1).

% Este predicado eh uma funcao extra que verifica se uma vizinhanca eh inacessivel.
%
% Argumentos:
%   - T: O tabuleiro representado como uma lista de listas.
%   - Tamanho: O tamanho do tabuleiro.
%   - Vizinhanca: A vizinhanca a ser verificada.
verificaInacessivel(T, Tamanho, Vizinhanca):-
    filtrarCoordenadas(Vizinhanca, Tamanho, Resultado),
    relvaOuVazio(T, Resultado), !.
    
% Este predicado eh uma funcao extra que obtem os indices de elementos em uma lista.
%
% Argumentos:
%   - Lista: A lista de elementos.
%   - ListaCompleta: A lista completa onde os elementos serao buscados.
%   - Indices: A lista de indices correspondentes aos elementos encontrados.
obterIndices([], _, []).
obterIndices([Primeiro|Resto], ListaCompleta, Indices) :-
    findall(Indice, nth1(Indice, ListaCompleta, Primeiro), IndiceElemento),
    obterIndices(Resto, ListaCompleta, RestoIndices),
    append(IndiceElemento, RestoIndices, Indices).

% Este predicado verifica e marca as celulas inacessiveis de um tabuleiro., ou seja, que nao tem arvores
% na sua vizinhanca.
%
% Argumentos:
%   - T: O tabuleiro representado como uma lista de listas.
inacessiveis(T):-
    length(T, Tamanho),
	todasCelulas(T, Vazios, _),
	maplist(vizinhanca, Vazios, ListaVizinhancas),
    include(verificaInacessivel(T, Tamanho), ListaVizinhancas, VizinhancasEspacosInacessiveis),
	obterIndices(VizinhancasEspacosInacessiveis, ListaVizinhancas, Indices),
    findall(Elemento, (member(Indice, Indices), nth1(Indice, Vazios, Elemento)), ListaInacessiveis),
    maplist(insereObjectoCelula(T, r), ListaInacessiveis).

% Este predicado aproveita as informacoes das contagens de linhas e colunas para preencher o tabuleiro com tendas.
%
% Argumentos:
%   - Puzzle: As informacoes do puzzle.
aproveita((T, L, C)):-
    calculaObjectosTabuleiro(T,ContagemLinhas,ContagemColunas,_),
    findall(Indice, (nth1(Indice, ContagemLinhas, E1), nth1(Indice, L, E2), E1 == E2), IndicesLinha),
    length(T, Tamanho),
    insereObjectoBaseadoLista(T, IndicesLinha, Tamanho, t, _),
    transpose(T, TTransposto),
    findall(Indice, (nth1(Indice, ContagemColunas, E1), nth1(Indice, C, E2), E1 == E2), IndicesColuna),
    insereObjectoBaseadoLista(TTransposto, IndicesColuna, Tamanho, t, _),
    transpose(TTransposto, Reverter),
    T = Reverter.

% Este predicado eh uma funcao extra que filtra as coordenadas vazias de uma lista, removendo as coordenadas 
% que estao fora do tabuleiro e mantendo apenas as coordenadas vazias.
%
% Argumentos:
%   - T: O tabuleiro representado como uma lista de listas.
%   - Tamanho: O tamanho do tabuleiro.
%   - Lista: A lista de coordenadas a serem filtradas.
%   - Resultado: A lista de coordenadas vazias resultante apos a filtragem.
filtraVazios(T, Tamanho, Lista, Resultado):-
    filtrarCoordenadas(Lista, Tamanho, SemCoordenadasFora),
    include(celulaVazia(T), SemCoordenadasFora, Resultado).

% Este predicado limpa as vizinhancas de tendas no tabuleiro.
%
% Argumentos:
%   - Puzzle: As informacoes do puzzle.
limpaVizinhancas((T,_,_)):-
    length(T, Tamanho),
    todasCelulas(T, Tendas, t),
    maplist(vizinhancaAlargada, Tendas, ListaVizinhancas),
    flatten(ListaVizinhancas, ListaAplanada),
    list_to_set(ListaAplanada, ListaSemDuplicatas),
    filtraVazios(T, Tamanho, ListaSemDuplicatas, ListaCoordenadasVazias),
    maplist(insereObjectoCelula(T, r), ListaCoordenadasVazias).

% Este predicado eh uma funcao extra que verifica se uma determinada coordenada contem uma tenda no tabuleiro.
%
% Argumentos:
%   - T: O tabuleiro representado como uma lista de listas.
%   - Coordenadas: As coordenadas a serem verificadas.
temTenda(T, Coordenadas) :-
    member((X, Y), Coordenadas),
    nth1(X, T, Linha),
    nth1(Y, Linha, Objecto),
    Objecto == t.

% Este predicado preenche a unica hipotese de uma vizinhanca de uma arvore, ou seja, 
% uma arvore que tem apenas um espaco vazio na sua vizinhanca.
%
% Argumentos:
%   - Puzzle: As informacoes do puzzle.
unicaHipotese((T, _, _)):-
    length(T, Tamanho),
    todasCelulas(T, Arvores, a),
    maplist(vizinhanca, Arvores, ListaVizinhancas),
    exclude(temTenda(T), ListaVizinhancas, ListaVizinhancasSemTenda),
    maplist(filtraVazios(T, Tamanho), ListaVizinhancasSemTenda, VizinhancasFiltradas),
    include([X]>>(length(X, 1)), VizinhancasFiltradas, APreencher),
    flatten(APreencher, ListaAplanada),
    maplist(insereObjectoCelula(T, t), ListaAplanada).

% Este predicado eh uma funcao extra que encontra os elementos comuns entre duas listas.
%
% Argumentos:
%   - Lista1: A primeira lista.
%   - Lista2: A segunda lista.
%   - Resultado: A lista resultante contendo os elementos comuns.
elementoComum(Lista1, Lista2, Resultado):-
    findall(Coordenada, (member(Coordenada, Lista1), memberchk(Coordenada, Lista2)), Resultado).

% Este predicado eh uma funcao extra que verifica se um elemento estah presente em alguma lista de uma lista de listas.
%
% Argumentos:
%   - Elemento: O elemento a ser verificado.
%   - Listas: A lista de listas onde serah feita a verificacao.
estaEmAlgumaLista(Elemento, Listas) :-
    member(Lista, Listas),
    member(Elemento, Lista).

% Este predicado valida se cada arvore esta associada a uma tenda.
%
% Argumentos:
%   - ListaArvores: A lista de coordenadas das arvores no tabuleiro.
%   - ListaTendas: A lista de coordenadas das tendas no tabuleiro.
valida(ListaArvores, ListaTendas):-
    length(ListaArvores, Tamanho),
    length(ListaTendas, Tamanho).
valida(ListaArvores, ListaTendas):-
    length(ListaArvores, Tamanho),
    maplist(vizinhanca, ListaArvores, Vizinhancas),
    maplist(elementoComum(ListaTendas), Vizinhancas, ListaComuns),
    include([X]>>(length(X, Len), Len > 1), ListaComuns, AMais),
    include([X]>>(length(X, 1)), ListaComuns, Length1),
    flatten(AMais, AMaisAplanado),
    flatten(Length1, Length1Aplanado),
    subtract(AMaisAplanado, Length1Aplanado, Resultado),
    list_to_set(Resultado, ResultadoFiltrado),
    append(Length1Aplanado, ResultadoFiltrado, ListaJuntas),
    length(ListaJuntas, Tamanho).

% Este predicado eh uma funcao extra que encontra os espaços vazios e tenta colocar tendas neles.
%
% Argumentos:
%   - T: O tabuleiro representado como uma lista de listas.
tentativaEErro(T):-
    todasCelulas(T, CelulasVazias, _),
    member(X, CelulasVazias),
    insereObjectoCelula(T, t, X).

% Este predicado resolve o puzzle.
%
% Argumentos:
%   - Puzzle: As informacoes do puzzle.
resolve((T, L, C)):-
    calculaObjectosTabuleiro(T, L, C, t),
    todasCelulas(T, ListaArvores, a), 
    todasCelulas(T, ListaTendas, t),
    valida(ListaArvores, ListaTendas), !.
resolve((T, L, C)):-
    todasCelulas(T, Vazias, _),
    inacessiveis(T), 
    relva((T, L, C)), 
    aproveita((T, L, C)), 
    limpaVizinhancas((T, L, C)), 
    unicaHipotese((T, L, C)), 
    todasCelulas(T, VaziasFinal, _),
    (Vazias == VaziasFinal ->
    tentativaEErro(T);true),
    todasCelulas(T, ListaArvores, a), 
    todasCelulas(T, ListaTendas, t), 
    \+ (valida(ListaArvores, ListaTendas)), !, 
    resolve((T, L, C)). 

