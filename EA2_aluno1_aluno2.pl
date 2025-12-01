:- use_module(library(aggregate)).

:- data_source(
    dbpedia_games,
    sparql("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbp: <http://dbpedia.org/property/>
SELECT DISTINCT ?nome ?genero ?desenvolvedor ?publicador ?plataforma
WHERE {
    ?game_uri a dbo:VideoGame;
              rdfs:label ?nome;
              dbo:genre ?genre_uri;
              dbo:developer ?dev_uri;
              dbo:publisher ?pub_uri;
              dbo:computingPlatform ?plat_uri.
    ?genre_uri rdfs:label ?genero.
    ?dev_uri   rdfs:label ?desenvolvedor.
    ?pub_uri   rdfs:label ?publicador.
    ?plat_uri  rdfs:label ?plataforma.
    FILTER (lang(?nome)          = 'en')
    FILTER (lang(?genero)        = 'en')
    FILTER (lang(?desenvolvedor) = 'en')
    FILTER (lang(?publicador)    = 'en')
    FILTER (lang(?plataforma)    = 'en')
}
ORDER BY ?nome
LIMIT 500",
    [ endpoint('https://dbpedia.org/sparql') ])
).

% Predicado base: mantém o MESMO padrão que já funcionava
jogo(Nome, Genero, Dev, Pub, Plat) :-
    distinct([Nome, Genero, Dev, Pub, Plat],
        dbpedia_games{
            nome:Nome,
            genero:Genero,
            desenvolvedor:Dev,
            publicador:Pub,
            plataforma:Plat
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%   REGRAS SIGNIFICATIVAS   %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Regra 1: Jogos que existem em mais de uma plataforma
% Usa: findall, listas
jogo_multiplataforma(Nome, Plataformas) :-
    jogo(Nome, _, _, _, _),
    findall(P, jogo(Nome, _, _, _, P), Lista),
    sort(Lista, Plataformas),
    length(Plataformas, N),
    N > 1.

% Regra 2: Desenvolvedora com muitos jogos (mais de 3)
% Usa: aggregate_all, corte
desenvolvedora_prolifica(Dev, Total) :-
    jogo(_, _, Dev, _, _),
    aggregate_all(
        count,
        distinct(J, jogo(J, _, Dev, _, _)),
        Total
    ),
    Total > 3,
    !.

% Regra 3: Jogo onde a publicadora é a mesma que a desenvolvedora
% ou a publicadora contém o nome da desenvolvedora
% Usa: strings, -> (se então)
autopublicado(Nome, Empresa) :-
    jogo(Nome, _, Dev, Pub, _),
    (   Dev = Pub
    ->  Empresa = Dev
    ;   sub_string(Pub, _, _, _, Dev),
        Empresa = Dev
    ).

% Regra 4: Jogos de desenvolvedoras diferentes no mesmo gênero
% Usa: negação
concorrentes_mesmo_genero(Jogo1, Jogo2, Genero) :-
    jogo(Jogo1, Genero, Dev1, _, _),
    jogo(Jogo2, Genero, Dev2, _, _),
    Jogo1 @< Jogo2,
    Dev1 \= Dev2.

% Regra 5: Lista de gêneros disponíveis em uma plataforma
% Usa: findall, listas
generos_por_plataforma(Plataforma, Generos, Total) :-
    jogo(_, _, _, _, Plataforma),
    findall(G, jogo(_, G, _, _, Plataforma), Lista),
    sort(Lista, Generos),
    length(Generos, Total).

% Regra 6: Jogo exclusivo de uma única plataforma
% Usa: findall, listas
exclusivo(Nome, Plataforma) :-
    jogo(Nome, _, _, _, Plataforma),
    findall(P, jogo(Nome, _, _, _, P), Plats),
    sort(Plats, PlatsUnicas),
    PlatsUnicas = [Plataforma].

% Regra 7: Parceria recorrente entre desenvolvedora e publicadora
% Usa: aggregate_all, -> (se então), corte
parceria_forte(Dev, Pub, NumJogos) :-
    jogo(_, _, Dev, Pub, _),
    Dev \= Pub,
    aggregate_all(
        count,
        distinct(N, jogo(N, _, Dev, Pub, _)),
        NumJogos
    ),
    (   NumJogos >= 2
    ->  true
    ;   fail
    ),
    !.

% Regra 8: Gêneros ordenados por quantidade de jogos
% Usa: findall, aggregate_all, listas
ranking_generos(ListaOrdenada) :-
    findall(Gen, jogo(_, Gen, _, _, _), Todos),
    sort(Todos, Unicos),
    findall(Qtd-Gen,
        (
            member(Gen, Unicos),
            aggregate_all(count, jogo(_, Gen, _, _, _), Qtd)
        ),
        Pares
    ),
    sort(0, @>=, Pares, ListaOrdenada).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% CONSULTAS PRE-DEFINIDAS %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/** <examples>
?- jogo(Nome, Genero, Dev, Pub, Plat).
?- jogo("Minecraft", Genero, Dev, Pub, Plat).

?- jogo_multiplataforma(Nome, Plataformas).
?- jogo_multiplataforma("Minecraft", Plataformas).

?- desenvolvedora_prolifica(Dev, Total).
?- desenvolvedora_prolifica("Ubisoft", Total).

?- autopublicado(Nome, Empresa).
?- autopublicado("Minecraft", Empresa).

?- concorrentes_mesmo_genero(J1, J2, Genero).
?- concorrentes_mesmo_genero(J1, J2, "Action game").

?- generos_por_plataforma(Plat, Generos, Total).
?- generos_por_plataforma("PlayStation 4", Generos, Total).

?- exclusivo(Nome, Plataforma).
?- exclusivo(Nome, "Nintendo Switch").

?- parceria_forte(Dev, Pub, N).
?- parceria_forte("Ubisoft", Pub, N).

?- ranking_generos(Lista).
*/
