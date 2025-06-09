:- use_module(piezas).

% 2. Ejercicios

% Ejercicio 1: Sublista
%! sublista(+Descartar, +Tomar, +L, -R)
sublista(D, T, L, R) :-
    length(Prefijo, D),         % El prefijo que descartamos tiene que tener longitud D.  
    append(Prefijo, Sufijo, L), % El prefijo descartdo + un sufijo tiene que formar L.
    length(R, T),               % El resultado R tiene que tener longitud L.
    append(R, _, Sufijo).       % R + un resto forman el sufijo.

% Ejercicio 2: Tablero
%! tablero(+K, -T)
tablero(K, [A,B,C,D,E]) :-
    length(A, K),
    length(B, K),
    length(C, K),
    length(D, K),
    length(E, K).
    
% Ejercicio 3: Tamaño
% tamano(+M, -F, -C)
tamano([H|T], F, C) :-
    length([H|T], F),
    length(H, C).
    
% Ejercicio 4: Coordenadas
% coordenadas(+T, -IJ)
coordenadas(T, (I, J)) :-
    tamano(T, F, C),
    between(1, C, I),
    between(1, F, J).
    
    
% Ejercicio 5: K-Piezas
% kPiezas(+K, -PS)

kPiezas(0,[]).
kPiezas(K, [H|T]) :-
    Km1 is K - 1,
    nombrePiezas(Piezas),
    member(H,Piezas),
    kPiezas(Km1, T).


% Ejercicio 6: SeccionTablero
% seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)
seccionTablero(T, ALTO, ANCHO, (I,J), ST) :-
    tamano(T, F, C),
    Im1 is I - 1,
    Jm1 is J - 1,                    % Obtenemos filas y columnas
    F >= ALTO + Im1,                 % Tiene que haber sufiecientes filas
    C >= ANCHO + Jm1,                      
    sublista(Im1, ALTO, T, X),          % 
    recortar_filas(Jm1, ANCHO, X, ST). 

recortar_filas(_, _, [], []).
recortar_filas(J, ANCHO, [H|T], [H1|T1]) :-
    sublista(J, ANCHO, H, H1),
    recortar_filas(J, ANCHO, T, T1).