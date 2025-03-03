% lp24 - ist194158 - projecto 
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Atenção: nao deves copiar nunca os puzzles para o teu ficheiro de código
% Nao remover nem modificar as linhas anteriores. Obrigado.
% Segue-se o código
%%%%%%%%%%%%

% ------------------------------------ VISUALIZAÇÃO ------------------------------------

/*visualiza/2 é um predicado que é verdade se Lista é uma lista e a aplicação deste pre-
dicado permitir escrever, por linha, cada elemento Palavra da lista Lista.*/
visualiza([]).

visualiza([Palavra|Lista]) :-
    writeln(Palavra), 
    visualiza(Lista).

/*visualizaLinha/2 é um predicado que é verdade se Lista é uma lista e a aplicação deste 
predicado permitir escrever (por linha) cada elemento Palavra da lista Lista após o res-
pectivo número da linha em causa (inicializado a 1), um ':' e um espaço.*/
visualizaLinha(Lista) :- 
    visualizaLinha(Lista, 1).

visualizaLinha([], _).

visualizaLinha([Palavra|Lista], Num) :-
    format('~w: ~w~n', [Num, Palavra]), NumNovo is Num + 1,
    visualizaLinha(Lista, NumNovo).

% ---------------------------- INSERÇÃO DE ESTRELAS E PONTOS ---------------------------

/*insereObjecto/3 é um predicado que é verdade se o tabuleiro Tabuleiro for o resultado
de inserir o objecto Obj nas coordenadas (L, C). Caso a coordenada (L, C) não se encon-
trar dentro dos limites do Tabuleiro, o predicado não faz nada.*/
insereObjecto((L, C), Tabuleiro, _) :- 
    coordenadasForaAux((L, C), Tabuleiro), !.

insereObjecto((L, C), Tabuleiro, Obj) :- 
    obtemObjetoAux(Tabuleiro, Var, (L, C)),
    (Var = Obj; member(Var, [p, e])), !.

/*insereVariosObjectos/3 é um predicado que é verdade se ListaCoord é uma lista de coor-
denadas e ListaObjs a lista de objectos a inserir nas coordenadas de ListaCoord do tabu-
leiro Tabuleiro, que é o resultado da aplicação do predicado. Caso as listas ListaCoord 
e ListaObjs tenham tamanhos diferentes, o predicado falha.*/
insereVariosObjectos([], _, []) :- !.

insereVariosObjectos([(L, C)|ListaCoord], Tabuleiro, [Obj|ListaObjs]) :-
    length(ListaCoord, N), length(ListaObjs, M), N =:= M,
    insereObjecto((L, C), Tabuleiro, Obj),
    insereVariosObjectos(ListaCoord, Tabuleiro, ListaObjs).

/*inserePontosVolta/2 é um predicado que é verdade se o tabuleiro Tabuleiro for o resul-
tado da inserção de pontos (p) nas coordenadas vizinhas de (L, C) (cima, baixo, direita,
esquerda e diagonais). Caso (L, C), ou alguma coordenada vizinha de (L, C), não estiver 
dentro dos limites de Tabuleiro, o predicado não falha: apenas são colocados pontos nas 
cordenadas que são válidas.*/
inserePontosVolta(Tabuleiro, (L, C)) :-
    coordenadasForaAux(Tabuleiro, (L, C)), !.

inserePontosVolta(Tabuleiro, (L, C)) :-
    maplist(encontraProxCoordAux((L, C)), [(-1,-1),(-1,0),(-1,1),
                                           ( 0,-1),       ( 0,1),
                                           ( 1,-1),( 1,0),( 1,1)], Coords),
    inserePontos(Tabuleiro, Coords).

/*inserePontos/2 é um predicado que é verdade se ListaCoord for uma lista de coordenadas
e o tabuleiro Tabuleiro for o resultado de inserir pontos nas coordenadas de ListaCoord.
Se alguma coordenada de ListaCoord não pertencer a Tabuleiro, o predicado não falha, mas 
apenas se colocam pontos nas coordenadas que são válidas.*/
inserePontos(_, []) :- !.

inserePontos(Tabuleiro, [(L, C)|ListaCoord]) :-
    insereObjecto((L, C), Tabuleiro, p),
    inserePontos(Tabuleiro, ListaCoord).

% -------------------------------------- CONSULTAS -------------------------------------

/*objectosEmCoordenadas/3 é um predicado verdadeiro se ListaObjs for a lista de objectos 
(pontos, estrelas ou variáveis) do tabuleiro Tabuleiro, nas coordenadas dadas pela lista 
Coords, e apresentados segundo a ordem das coordenadas de Coords. O predicado falha caso
alguma coordenada da lista Coords não pertencer a Tabuleiro.*/
objectosEmCoordenadas([], _, []) :- !.

objectosEmCoordenadas([(L, C)|Coords], Tabuleiro, [Obj|ListaObjs]) :-
    \+ coordenadasForaAux((L, C), Tabuleiro),
    obtemObjetoAux(Tabuleiro, Obj, (L, C)),
    objectosEmCoordenadas(Coords, Tabuleiro, ListaObjs).

/*coordObjectos/5 é verdade se Tabuleiro for um tabuleiro, Coords uma lista de coordena-
das e CoordObj é a sublista de Coords que contém as coordenadas dos objetos do tipo Obj, 
pela ordem em que aparecem no tabuleiro. Num é o número de objectos Obj encontrados e a
lista CoordObjs está ordenada por linha e coluna.*/
coordObjectos(Obj, Tabuleiro, Coords, CoordsObj, Num) :- 
    coordObjectos(Obj, Tabuleiro, Coords, CoordsObj, [], Num, 0).

coordObjectos(_, _, [], CoordsObj, CoordsObj, Num, Num) :- !.

coordObjectos(Obj, Tabuleiro, [(L, C)|Coords], CoordsObj, CoordsObjAux, Num, NumAux) :-
    \+ coordenadasForaAux((L, C), Tabuleiro),
    obtemObjetoAux(Tabuleiro, ObjAux, (L, C)),
    (ObjAux == Obj ; (var(Obj), var(ObjAux))),
    NumAuxNovo is NumAux + 1,
    append(CoordsObjAux, [(L, C)], CoordsObjAuxNovo),
    !,
    coordObjectos(Obj, Tabuleiro, Coords, CoordsObj, CoordsObjAuxNovo, Num, NumAuxNovo).

coordObjectos(Obj, Tabuleiro, [_|Coords], CoordsObj, CoordsObjAux, Num, NumAux) :-
    coordObjectos(Obj, Tabuleiro, Coords, CoordsObj, CoordsObjAux, Num, NumAux).

/*coordenadasVars/2 é um predicado que é verdade se Tabuleiro é um tabuleiro e CoordVars
a lista de coordenadas ordenadas por linha e coluna de Tabuleiro que contêm variáveis.*/
coordenadasVars([], []) :- !.

coordenadasVars(Tabuleiro, CoordVars) :- 
    length(Tabuleiro, N),
    coordLinhas(N, CoordTabLinha),
    flatten(CoordTabLinha, CoordTab),
    obtemVarsCoordAux(Tabuleiro, CoordTab, CoordVars).

% ------------------------------------- ESTRATÉGIAS ------------------------------------

/*fechaListaCoordenadas/2 é um predicado verdadeiro se Coords for uma lista de coordena-
das de uma única linha/coluna/região do tabuleiro Tab e que são fechadas se:
--> a lista já tem duas estrelas: nas restantes variáveis são colocados pontos (p).
--> a lista tem apenas uma variável e uma estrela: na variável sobrante é colocada a es-
trela (e) em falta e respectivos pontos (p) à volta.
--> a lista tem apenas duas variáveis e nenhuma estrela: nas variáveis sobrantes são co-
locadas as estrelas (e) em falta e respectivos pontos (p) à volta de cada uma.
----------------------------------------------------------------------------------------
O tabuleiro Tab é resultado da aplicação do predicado. Se a lista Coords não cumprir ne-
nhum dos requisitos acima, o predicado não faz nada.*/
fechaListaCoordenadas(_, []) :- !.

fechaListaCoordenadas(Tab, Coords) :-
    obtemVarsCoordAux(Tab, Coords, Vars), obtemEstrelasCoordAux(Tab, Coords, Estrelas),
    length(Vars, V), length(Estrelas, E),
    (
        (E == 2, inserePontos(Tab, Vars));
        (((V == 2, E == 0) ; (V == 1, E == 1)), insereEstrelasAux(Tab, Vars));
        (V >= 0) % neste caso, o predicado nao faz nada
    ), !.

/*fecha/2 é um predicado verdadeiro se ListaListasCoord for uma lista de listas de coor-
denadas (linhas, colunas ou regiões) do tabuleiro Tabuleiro e que serão fechadas pela a-
plicação do predicado fechaListaCoordenadas/2. O tabuleiro Tabuleiro é o resultado da a-
plicação deste predicado, com as respectivas linhas/colunas/regiões fechadas. O predica-
do não falha se alguma lista de ListaListasCoord não puder ser fechada.*/
fecha(_, []) :- !.

fecha(Tabuleiro, [Lista|ListaListasCoord]) :-
    fechaListaCoordenadas(Tabuleiro, Lista),
    fecha(Tabuleiro, ListaListasCoord).

/*encontraSequencia/4 é um predicado que é verdade se a sequência de coordenadas Seq com
tamanho N, cujos valores no tabuleiro Tabuleiro correspondem a variáveis, é uma sublista 
da lista de coordenadas ListaCoord. O predicado falha se:
--> o número de variáveis em ListaCoord for superior ou inferior a N.
--> existir uma ou mais estrelas em ListaCoord.
--> o número de variáveis em ListaCoord for N mas estas não forem sequenciais, ou seja,
estão separadas por um ou mais pontos (numa linha, coluna ou região).*/
encontraSequencia(Tabuleiro, N, ListaCoord, Seq) :- 
    encontraSequencia(Tabuleiro, N, 0, ListaCoord, [], Seq).

encontraSequencia(_, N, N, [], Seq, Seq) :- !.

encontraSequencia(Tabuleiro, N, NAux, [(L, C)|ListaCoord], SeqAux, Seq) :-
    \+ coordenadasForaAux((L, C), Tabuleiro),
    obtemObjetoAux(Tabuleiro, Obj, (L, C)), Obj == p,
    (NAux =:= 0 ; NAux =:= N), !,
    encontraSequencia(Tabuleiro, N, NAux, ListaCoord, SeqAux, Seq).

encontraSequencia(Tabuleiro, N, NAux, [(L, C)|ListaCoord], SeqAux, Seq) :-
    \+ coordenadasForaAux((L, C), Tabuleiro),
    obtemObjetoAux(Tabuleiro, Obj, (L, C)), var(Obj),
    NAuxNovo is NAux + 1, NAuxNovo =< N,
    append(SeqAux, [(L, C)], SeqAuxNovo), !,
    encontraSequencia(Tabuleiro, N, NAuxNovo, ListaCoord, SeqAuxNovo, Seq).

/*aplicaPadraoI/2 é um predicado que é verdade se a lista [(L1, C1), (L2, C2), (L3, C3)] 
for uma lista de coordenadas sequenciais numa mesma linha ou numa mesma coluna e o tabu-
leiro Tabuleiro é o resultado de colocar uma estrela (e) em (L1, C1) e (L3, C3) e respe-
tivos pontos (p) à volta de cada estrela.*/
aplicaPadraoI(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3)]) :-
    (
        (L2 =:= L1, L3 =:= L2, C2 is C1 + 1, C3 is C2 + 1);
        (C2 =:= C1, C3 =:= C2, L2 is L1 + 1, L3 is L2 + 1)
    ), !,
    insereEstrelasAux(Tabuleiro, [(L1, C1), (L3, C3)]).

/*aplicaPadroes/2 é um predicado que é verdade se ListaListasCoord for uma lista de lis-
tas de coordenadas e, após a aplicação do predicado, ter-se-ão encontrado sequências de 
tamanho 3 e aplicado o aplicaPadraoI/2, ou ter-se-ão encontrado sequências de tamanho 4 
e aplicado o aplicaPadraoT/2. O tabuleiro Tabuleiro resulta da aplicação do predicado.
O predicado não falha se nalguma lista de coordenadas não houver uma sequência válida.*/
aplicaPadroes(_, []) :- !.

aplicaPadroes(Tabuleiro, [Lista|ListaListasCoord]) :-
    encontraSequencia(Tabuleiro, 3, Lista, Seq), aplicaPadraoI(Tabuleiro, Seq),
    aplicaPadroes(Tabuleiro, ListaListasCoord), !.

aplicaPadroes(Tabuleiro, [Lista|ListaListasCoord]) :-
    encontraSequencia(Tabuleiro, 4, Lista, Seq), aplicaPadraoT(Tabuleiro, Seq),
    aplicaPadroes(Tabuleiro, ListaListasCoord), !.

aplicaPadroes(Tabuleiro, [_|ListaListasCoord]) :-
    aplicaPadroes(Tabuleiro, ListaListasCoord).

% ---------------------------------- APOTEOSE FINAL ------------------------------------

/*resolve/2 é verdade se Estruturas for uma estrutura (i.e. lista de listas das regiões)
e Tabuleiro for o tabuleiro resultante da aplicação dos predicados aplicaPadroes/2 e de
fecha/2 até já não ser possível alterar mais o tabuleiro. O predicado, porém, não garan-
te a resolução total do tabuleiro, sendo possível o tabuleiro ficar incompleto.*/
resolve(Estruturas, Tabuleiro) :-
    coordenadasVars(Tabuleiro, CoordVarsAntigo),
    resolve(Estruturas, Tabuleiro, CoordVarsAntigo).
    
resolve(Estruturas, Tabuleiro, CoordVarsAntigo) :-
    coordTodas(Estruturas, CoordTodas),
    aplicaPadroes(Tabuleiro, CoordTodas), fecha(Tabuleiro, CoordTodas),
    coordenadasVars(Tabuleiro, CoordVarsNovo), CoordVarsNovo \== CoordVarsAntigo,
    resolve(Estruturas, Tabuleiro, CoordVarsNovo), !.

resolve(_, Tabuleiro, CoordVarsAntigo) :-
    coordenadasVars(Tabuleiro, CoordVarsNovo),
    CoordVarsNovo == CoordVarsAntigo, 
    !. % não foi possível alterar mais o tabuleiro


% ------------------------------------- AUXILIARES -------------------------------------

/*encontraProxCoordAux/3 é um predicado auxiliar tal que (L, C) é uma coordenada e (X, Y)
é um vetor e a coordenada (NovaL, NovaC) é o resultado de somar (X, Y) a (L, C).
--> Usado no predicado inserePontosVolta/2.*/
encontraProxCoordAux((L, C), (X, Y), (NovaL, NovaC)) :-
    NovaL is L + X, NovaC is C + Y.

/*obtemVarsCoordAux/3 é um predicado auxiliar que, dado o tabuleiro Tab e as coordenadas 
Coord, a lista Vars será a lista de coordenadas de Coords cujo objecto é uma variável.
--> Usado nos predicados coordenadasVars/2 e fechaListaCoordenadas/2.*/
obtemVarsCoordAux(Tab, Coord, Vars) :-
    findall((L, C), (member((L, C), Coord), obtemObjetoAux(Tab, Obj, (L, C)), var(Obj)), Vars).

/*obtemEstrelasCoordAux/3 é um predicado auxiliar que, dado o tabuleiro Tab e as coorde-
nadas Coord, a lista Estrelas será a lista de coordenadas cujo objecto é uma estrela.
--> Usado no predicado fechaListaCoordenadas/2.*/
obtemEstrelasCoordAux(Tab, Coord, Estrelas) :-
    findall((L, C), (member((L, C), Coord), obtemObjetoAux(Tab, Obj, (L, C)), Obj == e), Estrelas).

/*insereEstrelasAux/2 é um predicado auxiliar que, dado o tabuleiro Tabuleiro, vai inse-
rir estrelas nas coordenadas da lista ListaCoord e respetivos pontos em volta.
--> Usado nos predicados fechaListaCoordenadas/2 e aplicaPadraoI/2.*/
insereEstrelasAux(_, []) :- !.
insereEstrelasAux(Tabuleiro, [(L, C)|ListaCoord]) :-
    insereObjecto((L, C), Tabuleiro, e),
    inserePontosVolta(Tabuleiro, (L, C)),
    insereEstrelasAux(Tabuleiro, ListaCoord), !.

/*coordenadasForaAux/2 é um predicado auxiliar que é verdade se as coordenadas (L, C) se 
encontrarem fora dos limites do tabuleiro Tabuleiro.
--> Usado nos predicados insereObjecto/3, inserePontosVolta/2, objectosEmCoordenadas/3,
coordObjectos/5 e encontraSequencia/4.*/
coordenadasForaAux((L, C), Tabuleiro) :-
    length(Tabuleiro, Linhas), Tabuleiro = [H|_], length(H, Colunas),
    (L < 1 ; L > Linhas ; C < 1 ; C > Colunas).

/*obtemObjetoAux/3 é um predicado auxiliar que permite obter o objecto Obj nas coordena-
das (L, C) do tabuleiro Tabuleiro.
--> Usado nos predicados insereObjecto/3, objectosEmCoordenadas/3, coordObjectos/5 e en-
contraSequencia/4 e nos auxiliares obtemVarsCoordAux/3 e obtemEstrelasCoordAux/3.*/
obtemObjetoAux(Tabuleiro, Obj, (L, C)) :- 
    nth1(L, Tabuleiro, Linha), nth1(C, Linha, Obj).