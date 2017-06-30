% Autor:
% Fecha: 09/03/2017

%HECHOS
personajes([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry,tiffany,natalie,roxanne,sarah,sabrina,cindy,emma]).
atributos([chico,chica,con_gafas,con_pelo_rubio,con_pelo_negro,feliz,triste,con_ropa_roja,con_ropa_verde,con_ojos_azules,con_ojos_marrones]).
chico([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry]).
chica([tiffany,natalie,roxanne,sarah,sabrina,cindy,emma]).
con_gafas([albert,michael,charles,anthony,natalie,sabrina]).
con_pelo_rubio([paul,michael,sam,will,anthony,billy,natalie,roxanne,emma]).
con_pelo_negro([albert,tom,derek,richard,louis,charles,steve,henry,tiffany,sarah,sabrina,cindy]).
feliz([albert,tom,richard,michael,charles,steve,will,anthony,tiffany,natalie,roxanne,sabrina,cindy,emma]).
triste([paul,derek,louis,sam,billy,henry,sarah]).
con_ropa_roja([albert,paul,richard,louis,sam,steve,anthony,henry,natalie,sarah,cindy]).
con_ropa_verde([tom,derek,michael,charles,will,billy,tiffany,roxanne,sabrina,emma]).
con_ojos_azules([albert,richard,louis,sam,will,billy,natalie,roxanne,sabrina]).
con_ojos_marrones([paul,tom,derek,michael,charles,steve,anthony,henry,tiffany,sarah,cindy,emma]).

%REGLAS
%-----------------------------------------------------------------------------------------------------------------
%Resta dos valores en valor absoluto
resta(A,B,Res):-A>B,Res is A-B.
resta(A,B,Res):-Res is B-A.
%------------------------------------------------------------------------------------------------------------------
%Calcula la diferencia de personajes que supondria el acertar o fallar cierta pregunta, para obtener cual es la mas equitativa
numero_filtrados(Pregunta,ListaCandidatos,NumeroFiltrados):-
                                                           filtrar_por_acierto(Pregunta,ListaCandidatos,CandidatosAcierto),
                                                           length(CandidatosAcierto,NumeroAcertando),
                                                           filtrar_por_fallo(Pregunta,ListaCandidatos,CandidatosFallo),
                                                           length(CandidatosFallo,NumeroFallando),
                                                           resta(NumeroAcertando,NumeroFallando,NumeroFiltrados),!.
%-------------------------------------------------------------------------------------------------------------------
%Elige la mejor pregunta que se puede realizar.
elegir_mejor_pregunta(ListaCandidatos,ListaPreguntas,PreguntaElegida):-
                                                           elegir_mejor_pregunta_aux(ListaCandidatos,ListaPreguntas,PreguntaElegida,PreguntaCandidata,999),!.

elegir_mejor_pregunta_aux(ListaCandidatos,[PreguntaActual|RestoPreguntas],PreguntaElegida,PreguntaCandidata,Contador):-
                                                           numero_filtrados(PreguntaActual,ListaCandidatos,Numero),
                                                           Numero<Contador,
                                                           elegir_mejor_pregunta_aux(ListaCandidatos,RestoPreguntas,PreguntaElegida,PreguntaActual,Numero).
                                                           
elegir_mejor_pregunta_aux(ListaCandidatos,[PreguntaActual|RestoPreguntas],PreguntaElegida,PreguntaCandidata,Contador):-
                                                           elegir_mejor_pregunta_aux(ListaCandidatos,RestoPreguntas,PreguntaElegida,PreguntaCandidata,Contador).
                                                           
elegir_mejor_pregunta_aux(_,_,X,X,_).
%------------------------------------------------------------------------------------------------------------------------
%Muestra la pregunta que ha elegido la maquina
mostrar_mejor_pregunta(Pregunta):-writeln('ahora te hago yo una pregunta:'),
                                  write('¿'),write(Pregunta),writeln('?').

%-------------------------------------------------------------------------------------------------------------------------
%Compueba si un personaje posee cierto atributo
posee_atributo(Personaje,Atributo):- Funcion=..[Atributo,Lista],
                      call(Funcion),
                      member(Personaje,Lista),!.

%-------------------------------------------------------------------------------------------------------------------------
%Muestra el mensaje inicial del juego
mensaje_inicial:-writeln('estrategia avanzada.').

%------------------------------------------------------------------------------------------------------------------------
%Inicia las listas necesarias para ejecutar el hilo principal
iniciar_preguntas(Lista):-atributos(Lista).
iniciar_candidatos(Lista):-personajes(Lista).

%------------------------------------------------------------------------------------------------------------------------
%Selecciona un personaje aleatorio que utilizaremos para la maquina y otro para el jugador.
seleccionar_personaje(Personaje):-personajes(Lista),random(1,22,X),nth1(X,Lista,Personaje).

%-----------------------------------------------------------------------------------------------------------------------
%Muestra el personaje que nos ha tocado y sus caracteristicas
mostrar_personaje_inicial(Personaje):-obtener_atributos(Personaje,ListaAtributos),
                              write('tu personaje es: '),
                              writeln(Personaje),
                              write('te recuerdo sus caracteristicas: '),
                              writeln(ListaAtributos).

%--------------------------------------------------------------------------------------------------------------------------
%Muestra las preguntas que estan disponibles
mostrar_preguntas(ListaPreguntas):-writeln('el jugador puede realizar las siguientes preguntas:'),
                                   writeln(ListaPreguntas).

%---------------------------------------------------------------------------------------------------------------------------
%Obtiene la pregunta del jugador
leer_pregunta(Pregunta):- read(Pregunta).

%--------------------------------------------------------------------------------------------------------------------------
%Muestra los atributos de los personajes que se indican en la lista
mostrar_personajes(ListaPersonajes):-writeln('ya sabes que soy uno de los personajes de la siguiente lista: '),
                                     mostrar_personajes_aux(ListaPersonajes).
mostrar_personajes_aux([Personaje|RestoPersonajes]):-
                                                    obtener_atributos(Personaje,ListaAtributos),
                                                    write(Personaje),write('->'),writeln(ListaAtributos),
                                                    mostrar_personajes_aux(RestoPersonajes).
mostrar_personajes_aux([]).
%--------------------------------------------------------------------------------------------------------------------------
%Devuelve todos los atributos que posee cierto personaje
obtener_atributos(Personaje,ListaAtributos):-atributos(Atributos),obtener_atributos_aux(Personaje,ListaAtributos,[],Atributos),!.

obtener_atributos_aux(Personaje,ListaAtributos,ListaRellenar,[Atributo|RestoLista]):-
                                                             posee_atributo(Personaje,Atributo),
                                                             obtener_atributos_aux(Personaje,ListaAtributos,[Atributo|ListaRellenar],RestoLista).
obtener_atributos_aux(Personaje,ListaAtributos,ListaRellenar,[Atributo|RestoLista]):-
                                                             not(posee_atributo(Personaje,Atributo)),
                                                             obtener_atributos_aux(Personaje,ListaAtributos,ListaRellenar,RestoLista).
obtener_atributos_aux(_,X,X,[]).

%---------------------------------------------------------------------------------------------------------------------------
%Borra la pregunta que se hace de la lista de preguntas que se maneja para no poder volver a hacerla
borrar_pregunta(Pregunta,ListaPreguntas,ListaRestantes):-select(Pregunta,ListaPreguntas,ListaRestantes).
%---------------------------------------------------------------------------------------------------------------------------
%Filtra los personajes que cumplen cierta condicion
filtrar_por_acierto(Atributo,ListaPersonajes,PersonajesFiltrados):-filtrar_por_acierto_aux(Atributo,ListaPersonajes,PersonajesFiltrados,[]),!.

filtrar_por_acierto_aux(Atributo,[Personaje|RestoPersonajes],PersonajesFiltrados,ListaRellenar):-
                                                           posee_atributo(Personaje,Atributo),
                                                           filtrar_por_acierto_aux(Atributo,RestoPersonajes,PersonajesFiltrados,[Personaje|ListaRellenar]).
filtrar_por_acierto_aux(Atributo,[Personaje|RestoPersonajes],PersonajesFiltrados,ListaRellenar):-
                                                           not(posee_atributo(Personaje,Atributo)),
                                                           filtrar_por_acierto_aux(Atributo,RestoPersonajes,PersonajesFiltrados,ListaRellenar).
filtrar_por_acierto_aux(_,_,X,X).

%-----------------------------------------------------------------------------------------------------------------------------
%Filtra los personajes que NO cumplen una cierta condicion
filtrar_por_fallo(Atributo,ListaPersonajes,PersonajesFiltrados):- filtrar_por_fallo_aux(Atributo,ListaPersonajes,PersonajesFiltrados,[]),!.

filtrar_por_fallo_aux(Atributo,[Personaje|RestoPersonajes],PersonajesFiltrados,ListaRellenar):-
                                                           not(posee_atributo(Personaje,Atributo)),
                                                           filtrar_por_fallo_aux(Atributo,RestoPersonajes,PersonajesFiltrados,[Personaje|ListaRellenar]).
filtrar_por_fallo_aux(Atributo,[Personaje|RestoPersonajes],PersonajesFiltrados,ListaRellenar):-
                                                           posee_atributo(Personaje,Atributo),
                                                           filtrar_por_fallo_aux(Atributo,RestoPersonajes,PersonajesFiltrados,ListaRellenar).
filtrar_por_fallo_aux(_,_,X,X).

%------------------------------------------------------------------------------------------------------------------------------
%Realiza una pregunta, de manera que borra la pregunta que ha realizado y filtra los personajes adecuados en funcion de la pregunta
realizar_pregunta(Personaje,Pregunta,ListaPreguntas,ListaCandidatos,PreguntasRestantes,CandidatosRestantes):-
                                                           posee_atributo(Personaje,Pregunta),
                                                           borrar_pregunta(Pregunta,ListaPreguntas,PreguntasRestantes),
                                                           filtrar_por_acierto(Pregunta,ListaCandidatos,CandidatosRestantes),
                                                           writeln('la respuesta a la pregunta es afirmativa').

realizar_pregunta(Personaje,Pregunta,ListaPreguntas,ListaCandidatos,PreguntasRestantes,CandidatosRestantes):-
                                                           not(posee_atributo(Personaje,Pregunta)),
                                                           borrar_pregunta(Pregunta,ListaPreguntas,PreguntasRestantes),
                                                           filtrar_por_fallo(Pregunta,ListaCandidatos,CandidatosRestantes),
                                                           writeln('la respuesta a la pregunta es negativa').

%--------------------------------------------------------------------------------------------------------------------------
%Cuenta los personajes que le quedan a la maquina como candidatos.
contar_candidatos(ListaCandidatos):-length(ListaCandidatos,NumeroCandidatos),
                                   write('dudo entre  '),
                                   write(NumeroCandidatos),
                                   writeln(' personajes').

%--------------------------------------------------------------------------------------------------------------------------
%Comprueba si ha ganado alguien y escribe por pantalla quien es el ganador o si hay empate.
comprobar_victoria(CandidatosJugador,CandidatosMaquina,PersonajeJugador,PersonajeMaquina):-
                  comprobar_victoria_aux(CandidatosJugador,CandidatosMaquina,PersonajeJugador,PersonajeMaquina).

comprobar_victoria_aux(CandidatosJugador,CandidatosMaquina,PersonajeJugador,PersonajeMaquina):-
                  length(CandidatosJugador,TamannoJugador),
                  TamannoJugador>1,
                  length(CandidatosMaquina,TamannoMaquina),
                  TamannoMaquina>1.

comprobar_victoria_aux(CandidatosJugador,CandidatosMaquina,PersonajeJugador,PersonajeMaquina):-
                  length(CandidatosMaquina,TamannoMaquina),
                  not(TamannoMaquina>1),
                  write('ha ganado la maquina, ya que ha acertado tu jugador: '),
                  writeln(PersonajeJugador),
                  fail.

comprobar_victoria_aux(CandidatosJugador,CandidatosMaquina,PersonajeJugador,PersonajeMaquina):-
                  length(CandidatosJugador,TamannoJugador),
                  not(TamannoJugador>1),
                  write('ha ganado el jugador, ya que has acertado el personaje de la maquina: '),
                  writeln(PersonajeMaquina),
                  fail.
comprobar_victoria_aux(CandidatosJugador,CandidatosMaquina,PersonajeJugador,PersonajeMaquina):-
                  length(CandidatosJugador,TamannoJugador),
                  not(TamannoJugador>1),
                  length(CandidatosMaquina,TamannoMaquina),
                  not(TamannoMaquina>1),
                  writeln('empate tecnico, ambos habeis llegado a la solucion a la vez'),
                  fail.

%--------------------------------------------------------------------------------------------------------------------------
%Bucle del juego en el que juega hasta que alguien gane
 bucle_juego(ListaPreguntasJugador,ListaPreguntasMaquina,ListaCandidatosJugador,ListaCandidatosMaquina,PersonajeJugador,PersonajeMaquina):-
       mostrar_preguntas(ListaPreguntasJugador), %Muestra todas las preguntas que el jugador puede realizar
       leer_pregunta(PreguntaJugador),           %Lee la pregunta que hace el jugador
       realizar_pregunta(PersonajeMaquina,PreguntaJugador,ListaPreguntasJugador,ListaCandidatosJugador,PreguntasRestantesJ,CandidatosRestantesJ),
       elegir_mejor_pregunta(ListaCandidatosMaquina,ListaPreguntasMaquina,PreguntaMaquina), %Elige una pregunta la pregunta menos arriesgada que puede realizar.
       mostrar_mejor_pregunta(PreguntaMaquina),
       realizar_pregunta(PersonajeJugador,PreguntaMaquina,ListaPreguntasMaquina,ListaCandidatosMaquina,PreguntasRestantesM,CandidatosRestantesM),
       contar_candidatos(CandidatosRestantesM), %Muestra el numero de personajes que le quedan a la maquina por filtrar
       mostrar_personajes(CandidatosRestantesJ), %Muestra los personajes que le quedan al jugador por filtrar
       comprobar_victoria(CandidatosRestantesJ,CandidatosRestantesM,PersonajeJugador,PersonajeMaquina),%Comprueba si alguien ha llegado a la solucion
       bucle_juego(PreguntasRestantesJ,PreguntasRestantesM,CandidatosRestantesJ,CandidatosRestantesM,PersonajeJugador,PersonajeMaquina).

%-----------------------------------------------------------------------------------------------------------------------------
%Hilo principal del juego
jugar:-mensaje_inicial,
       iniciar_preguntas(ListaPreguntasJugador),  %Inicia la lista de preguntas que puede realizar el jugador
       iniciar_preguntas(ListaPreguntasMaquina),  %Inicia la lista de preguntas que puede realizar el jugador
       iniciar_candidatos(ListaCandidatosJugador),%Inicia los candidatos que tiene que valorar el jugador para ganar
       iniciar_candidatos(ListaCandidatosMaquina),%Inicia los candidatos que tiene que valorar el jugador para ganar
       seleccionar_personaje(PersonajeJugador),   %Selecciona el personaje que le ha tocado al jugador
       seleccionar_personaje(PersonajeMaquina),   %Selecciona el personaje que le ha tocado a la maquina
       mostrar_personaje_inicial(PersonajeJugador),%Muestra el personaje que le ha tocado al jugador para saber como quien juegas
       bucle_juego(ListaPreguntasJugador,ListaPreguntasMaquina,ListaCandidatosJugador,ListaCandidatosMaquina,PersonajeJugador,PersonajeMaquina).