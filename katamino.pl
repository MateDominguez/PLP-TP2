:- use_module(piezas).

% 2. Ejercicios

% Ejercicio 1: Sublista
%! sublista(+Descartar, +Tomar, +L, -R)
sublista(D, T, L, R) :-
    length(Prefijo, D),         % El prefijo que descartamos tiene que tener longitud D.  
    append(Prefijo, Sufijo, L), % El prefijo descartdo + un sufijo tiene que formar L.
    length(R, T),               % El resultado R tiene que tener longitud L.
    append(R, _, Sufijo).       % R + un resto forman el sufijo.

/*
% Ejercicio 12: Reversibilidad
% sublista(-Descartm, +Tomar, +L, +R)

Casos:

* Ninguna solucion
sublista(D, 2, [a,b,c,d,e,f], [g]).
se cuelga

* Una solucion
sublista(D, 1, [a,b,c,d,e,f], [a]).
D = 0; 
se cuelga

* Multiples soluciones
sublista(D, 1, [a,a,c,d,e,f], [a]).
D = 0;
D = 1;
se cuelga
*/

% Ejercicio 2: Tablero
%! tablero(+K, -T)
tablero(K, [A,B,C,D,E]) :-
    K > 0,
    length(A, K),
    length(B, K),
    length(C, K),
    length(D, K),
    length(E, K).
    

% Ejercicio 3: Tamaño
%! tamano(+M, -F, -C)
tamano([H|T], F, C) :-
    length([H|T], F),
    length(H, C).
    

% Ejercicio 4: Coordenadas
%! coordenadas(+T, -IJ)
coordenadas(T, (I, J)) :-
    tamano(T, F, C),
    between(1, C, J),
    between(1, F, I).


% Ejercicio 5: K-Piezas
% kPiezas(+K, -PS)
kPiezas(K, XS) :-
    nombrePiezas(P), 
    kPiezasAux(K, XS, P).

% kPiezasAux(+K, -PS, +PRestantes)
kPiezasAux(0,[], _). % Caso base K == 0
kPiezasAux(K, [P|XS], [P|PS]) :- % Caso recursivo, elijo la pieza
    Km1 is K - 1,
    length(XS, Km1),
    length(PS, LPS), % Poda si no quedan sifientes piezas
    LPS >= Km1,
    kPiezasAux(Km1, XS, PS).
kPiezasAux(K, [X|XS], [_|PS]) :- % Caso recursivo, NO elijo la pieza
    length(PS, LPS), % Poda si no quedan sifientes piezas
    LPS >= K,
    kPiezasAux(K, [X|XS], PS).


% Ejercicio 6: SeccionTablero
% seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)
seccionTablero(T, ALTO, ANCHO, (I,J), ST) :-
    tamano(T, F, C),
    Im1 is I - 1,
    Jm1 is J - 1,                    % Obtenemos filas y columnas
    F >= ALTO + Im1,                 % Tiene que haber sufiecientes filas
    C >= ANCHO + Jm1,                % Tiene que haber sufiecientes columnas
    sublista(Im1, ALTO, T, X),       %  Me quedo con las ALTO filas a partir de i
    recortarFilas(Jm1, ANCHO, X, ST). % Me quedo con las ANCHO columnas a partir de j

recortarFilas(_, _, [], []).
recortarFilas(J, ANCHO, [H|T], [H1|T1]) :-
    sublista(J, ANCHO, H, H1),
    recortarFilas(J, ANCHO, T, T1).


% Ejercicio 7: Ubicar pieza
% ubicarPieza(+Tablero, +Identificador)
ubicarPieza(Tablero, ID) :-
    pieza(ID, Pieza),
    tamano(Pieza, FilasP, ColP),
    tamano(Tablero, FilasT, ColT),
    coordenadas(Tablero, (I, J)),
    I + FilasP - 1=< FilasT,
    J + ColP - 1 =< ColT,
    seccionTablero(Tablero, FilasP, ColP, (I,J), Seccion),
    puedeColocar(Pieza, Seccion, ID).

puedeColocar([], [], _).
puedeColocar([P|PS], [S|SS], ID):-
    coincideFila(P, S, ID),
    puedeColocar(PS, SS, ID).

coincideFila([], [], _).
coincideFila([P|PS], [S|SS], ID):-
    nonvar(P),
    var(S),
    S = ID,
    coincideFila(PS, SS, ID).
coincideFila([P|PS], [_|SS], ID):-
    var(P),
    coincideFila(PS, SS, ID).

% Ejercicio 8: Ubicar piezas
% ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(_, _, []).
ubicarPiezas(Tablero, Poda, [ID|IDs]):-
    ubicarPieza(Tablero, ID),
    poda(Poda, Tablero),
    ubicarPiezas(Tablero, Poda, IDs).

% poda(+Poda, +Tablero)
poda(sinPoda, _).
poda(podaMod5, T):- todosGruposLibresModulo5(T).


% Ejericio 9: Llenar Tablero
% llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(Poda, Columnas, Tablero):-
    tablero(Columnas, Tablero),
    kPiezas(Columnas, Piezas),
    ubicarPiezas(Tablero, Poda, Piezas).

% Ejercicio 10: Medicion
% cantSoluciones(+Poda, +Columnas, -N)
cantSoluciones(Poda, Columnas, N) :-
    findall(T, llenarTablero(Poda, Columnas, T), TS),
    length(TS, N).

/* 
Mediciones

K=3
?- time(cantSoluciones(sinPoda, 3, N)).
% 16,027,736 inferences, 0.902 CPU in 0.903 seconds (100% CPU, 17761550 Lips)
N = 28.

K=4
?- time(cantSoluciones(sinPoda, 4, N)).
% 764,343,781 inferences, 40.355 CPU in 40.360 seconds (100% CPU, 18940347 Lips)
N = 200. 

K=5
?- time(cantSoluciones(sinPoda, 5, N)).
% 21,229,838,177 inferences, 1129.442 CPU in 1129.582 seconds (100% CPU, 18796756 Lips)
N = 856.
*/

% Ejercicio 11: Optimizacion

% todosGruposLibresModulo5(+Tablero)
todosGruposLibresModulo5(Tablero):-
    findall(IJ, (coordenadas(Tablero, IJ), casillaLibre(IJ, Tablero)), Coordenadas),
    agrupar(Coordenadas, Grupos),
    forall(member(Grupo, Grupos), tamanoMod5(Grupo)).

% casillaLibre(+IJ, +Tablero)
casillaLibre((I, J), Tablero):-
    nth1(I, Tablero, Fila),
    nth1(J, Fila, Casilla),
    var(Casilla).

% tamanoMod5(Grupo)
tamanoMod5(Grupo):-
    length(Grupo, L),
    L mod 5 =:= 0.

/*
Mediciones

K=3
?- time(cantSoluciones(podaMod5, 3, N)).
% 9,460,374 inferences, 0.540 CPU in 0.540 seconds (100% CPU, 17508380 Lips)
N = 28.

K=4
?- time(cantSoluciones(podaMod5, 4, N)).
% 236,334,478 inferences, 13.131 CPU in 13.132 seconds (100% CPU, 17997733 Lips)
N = 200.

K=5
?- time(cantSoluciones(podaMod5, 5, N)).
% 3,846,191,964 inferences, 216.940 CPU in 216.961 seconds (100% CPU, 17729271 Lips)
N = 856.
*/