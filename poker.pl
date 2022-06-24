esCarta(a).
esCarta(8).
esCarta(9).
esCarta(10).
esCarta(j).
esCarta(q).
esCarta(k).


adyacenteDe(a, k, 8).
adyacenteDe(8, a, 9).
adyacenteDe(9, 8, 10).
adyacenteDe(10, 9, j).
adyacenteDe(j, 10, q).
adyacenteDe(q, j, k).
adyacenteDe(k, as, q).

esAdyacente(Carta, A1, A2) :- adyacenteDe(Carta, A1, A2); adyacenteDe(Carta, A2, A1).

matriz28Cartas(L):- L = [a,a,a,a, 8,8,8,8, 9,9,9,9, 10,10,10,10, j,j,j,j, q,q,q,q, k,k,k,k].
%%%%%%%%%%%%%%%
%%%% PISTAS %%%
%%%%%%%%%%%%%%%

% En al fila i hay tal palo fila(i, palo).
fila(1, poker).
fila(2, nada).
fila(3, escalera).
fila(4, nada).
fila(5, pareja).

% En al columna i hay tal palo columna(i, palo).
columna(1, escalera).
columna(2, full).
columna(3, trio).
columna(4, escalera).
columna(5, pareja).

diagonal1(doblePareja).

diagonal2(escalera).

carta(1,1,q).
carta(1,3,q).
carta(1,4,q).
carta(2,1,k).
carta(2, 4,9).
carta(3,1,a).
carta(3,3,j).
carta(3,4,8).
carta(3,5,9).
carta(5,2,k).



%%% VERIFICACION DE PALOS %%%%

palo(pareja, [A,B,C,D,E]) :- 
    A = B; A = C; A = D; A = E; 
    B = C; B =  D; B = E; 
    C = D; C = E; 
    D = E.
    

palo(doblePareja, [A,B,C,D,E]):-
    % [A A B B _] [A A B _ B] [A A _ B B]  
    % [A _ A B B] [A B A B _] [A B A _ B]
    % [A B B A _] [A B _ A B] [A _ B A B]
    % [A B B _ A] [A B _ B A] [A _ B B A]
    % [_ A A B B] [_ A B A B] [_ A B B A]
    
    (A = B, C = D); (A = B, C = E); (A = B, D = E); 
    (A = C, D = E); (A = C, B = D); (A = C, B = E);
    (A = D, B = C); (A = D, B = E); (A = D, C = E);
    (A = E, B = C); (A = E, B = D); (A = E, C = E);
    (B = C, D = E); (B = D, C = E); (B = E, C = D).


palo(trio, [A,B,C,D,E]) :- 
        (A = B, A = C); (A = B, A = D); (A = B, A = E);
        (B = C, B = D); (B = C, B = E);
        (C = D, C = E);
        (A = C, A = E);
        (A = D, A = E);
        (A = C, A = D);
        (B = D, B = E). 

palo(full, [A,B,C,D,E]):-
    % not(palo(poker, [A,B,C,D,E])), % Para que no considere un full [a,a,a,a,b] ya que segun la sgte linea cumple un par y un trio
    % palo(trio, [A,B,C,D,E]), palo(pareja, [A,B,C,D,E]).
    (A=B, A=C,D=E, A\=D); (A=B, A=D, C=E, A\=C); (A=B, A=E, C=D, A\=C); (A=C, A=D, B=E, A\=B); (A=D, A=E, B=C, A\=B);
    (B=C, B=D, A = E, B\=A); (B=D, B=E, A=C, B\=A); (B=C, B=E, A=D, A\=B); (B=A, C=D, C=E, B\=C). 


palo(poker, [A,B,C,D,E]) :-
    (A = B, A = C, A = D, A\=E); (A = B, A = C, A = E, A\=D); (A = B, A = D, A = E, A\=C); (A = C, A = D, A = E, A\=B);
    (B = C, B = D, B = E, B\=A).


    % todas las combinaciones de cartas que formen escalera ( el orden de las cartas en la lista no importa)
    % [as, 8, 9, 10, j] [ 8, 9, 10, j, q], [9,10,j,q,k]
    % [10,j,q,k,as], [j,q,k,as,8], [q,k,as,8,9], [k,as,8,9,10]
    % Entonces pregunto con member si esta tal carta en la lista.


    %L = [A|[B,C,D,E]] ( esto me tira true cuando no deberia cumplir un member)
palo(escalera, L) :- 
    ninguna_igual(L),
    (
    ( not(member(k,L)), not(member(a,L)));
    ( not(member(8,L)), not(member(a,L)));
    ( not(member(8,L)), not(member(9,L)));
    ( not(member(q,L)), not(member(k,L)))
    ).
    % ( member(as, L), member(8, L), member(9, L), member(10, L), member(j, L));
    % ( member(8, L), member(9, L), member(10, L), member(j, L), member(q, L));
    % ( member(9, L), member(10, L), member(j, L), member(q, L), member(k, L));
    % ( member(10, L), member(j, L), member(q, L), member(k, L), member(as, L));

palo(nada, L):- 
    not(palo(escalera, L)),
    not(palo(pareja, L)),
    not(palo(full, L)),
    not(palo(trio, L)),
    not(palo(doblePareja, L)).



ninguna_igual([A,B,C,D,E]) :- 
    A \= B, A \= C, A \= D, A \= E,
    B \= C, B \= D, B \= E,
    C \= D, C \= E,
    D \= E.

todasIguales([A,A,A,A,A]).
 




crearPlantilla(L) :- 
    bagof(casilla(X,Y,_),(between(1,5,X),between(1,5,Y)),L).

cargarCartasPistas(Matriz, CartasCargadas, PosicionesCargadas) :- 
    findall(pos(F,C,ValorCarta), carta(F,C,ValorCarta), CartasPistas), 
    meterCartas(Matriz, CartasPistas),
    findall(X, carta(_,_,X), CartasCargadas),
    findall(casilla(F,C,X), carta(F,C,X), PosicionesCargadas).

meterCartas(_,[]).
meterCartas(Matriz,[pos(F,C,ValorCarta)|Tail]):-
    member(casilla(F,C,ValorCarta),Matriz),
    meterCartas(Matriz,Tail).



% Carga cartas randoms en tal fila de la matriz
combinacionFila(Fila,Matriz, CartasDisponibles,MazoRestante):-
    findall(pos(Fila,C, ValorCarta), member(casilla(Fila,C,ValorCarta), Matriz), Posiciones),
    meterCartasRecursivo(Matriz, CartasDisponibles, Posiciones, MazoRestante).

    

% hace la carga de la funcion anterior en la fila correspondiente
meterCartasRecursivo(_,MazoActual, [],MazoRestante):- MazoRestante = MazoActual.
meterCartasRecursivo(Matriz, CartasDisponibles, [pos(F,C,_)|Tail],MazoRestante):-
    select(Carta, CartasDisponibles, CartasRestantes), 
    member(casilla(F,C,Carta),Matriz),
    meterCartasRecursivo(Matriz,CartasRestantes, Tail, MazoRestante).
       


verificarFila(Fila, Matriz):-
    findall(ValorCarta, member(casilla(Fila,_,ValorCarta), Matriz), L), 
    
    fila(Fila, PALO), palo(PALO, L).

verificarColumna(Columna, Matriz):-
    findall(ValorCarta, member(casilla(_,Columna,ValorCarta), Matriz), L),
   
    columna(Columna, PALO), palo(PALO, L).

    %meterCartasRandom(Matriz, Posiciones).

        

combinarFilasYVerificar(M, MazoRestante, PosicionesNOCargadas):-
    combinacionFila(1,PosicionesNOCargadas, MazoRestante, MRestante2),
    verificarFila(1, M),

    combinacionFila(2,PosicionesNOCargadas, MRestante2, MRestante3),
    verificarFila(2, M),

    combinacionFila(3,PosicionesNOCargadas, MRestante3, MRestante4),
    verificarFila(3, M),

    combinacionFila(4,PosicionesNOCargadas, MRestante4, MRestante5),
    verificarFila(4, M),
    combinacionFila(5,PosicionesNOCargadas, MRestante5, _),
    verificarFila(5, M),

    % write(MRestante2),
    % write(MRestante3),
    % write(MRestante4),
    % write(MRestante5),
    % verificarFila(2, M),
    % verificarFila(3, M),
    % verificarFila(4, M),
    % verificarFila(5,M),
   
    imprimirMatriz(M),
    verificarColumna(1,M),
    verificarColumna(2,M),
    verificarColumna(3,M),
    verificarColumna(4,M),
    verificarColumna(5,M).
    


   

    


    

imprimirMatriz(M):-
    findall(CARTA, member(casilla(1,C,CARTA), M), Fila1),
    findall(CARTA, member(casilla(2,C,CARTA), M), Fila2),
    findall(CARTA, member(casilla(3,C,CARTA), M), Fila3),
    findall(CARTA, member(casilla(4,C,CARTA), M), Fila4),
    findall(CARTA, member(casilla(5,C,CARTA), M), Fila5),

    write('La Matriz de cartas es '),nl,
    write(Fila1),nl,
    write(Fila2),nl,
    write(Fila3),nl,
    write(Fila4),nl,
    write(Fila5),nl.
    



getMazoRestante([],MazoActual,Respuesta):- Respuesta = MazoActual.

getMazoRestante([Actual| Tail] ,MazoActual, Respuesta):-
    select(Actual, MazoActual, Respuesta2), 
    getMazoRestante(Tail, Respuesta2, Respuesta).

    


poker(L):-
    matriz28Cartas(CartasDisponibles),

    crearPlantilla(L),
    cargarCartasPistas(L, CartasCargadas, PosicionesCargadas),
    subtract(L, PosicionesCargadas, PosicionesNOCargadas), % Conseguimos las posiciones tdv sin cargarse
    getMazoRestante( CartasCargadas, CartasDisponibles, MazoRestante), % Obtenemos el mazo restante
    combinarFilasYVerificar(L, MazoRestante, PosicionesNOCargadas).
    %write(L),
    %imprimirMatriz(L).
    %meterCartasRandomYComprobar(L),nl,nl.
