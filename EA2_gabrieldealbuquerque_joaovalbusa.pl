:- module(ea2_games, [jogo/5, jogo_multiplataforma/2, desenvolvedora_prolifica/2, autopublicado/2, concorrentes_mesmo_genero/3, generos_por_plataforma/3, exclusivo/2, parceria_forte/3, ranking_generos/1]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(aggregate)).

/*
    Trabalho de Programação Lógica - EA2
    Tema: Video Games (DBpedia)
    Objetivo: Regras lógicas baseadas em dados da Web Semântica.
*/

:- data_source(
    dbpedia_games,
    sparql("
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
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
            
            FILTER (lang(?nome) = 'en')
            FILTER (lang(?genero) = 'en')
            FILTER (lang(?desenvolvedor) = 'en')
            FILTER (lang(?publicador) = 'en')
            FILTER (lang(?plataforma) = 'en')
        }
        ORDER BY ?nome
        LIMIT 10000",
        [ endpoint('https://dbpedia.org/sparql') ]
    )
).

% Predicado base que mapeia os dados da DBpedia para fatos Prolog
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

% -----------------------------------------------------------------------
% Regra 1: Jogos Multiplataforma
% Identifica jogos que foram lançados para mais de uma plataforma diferente.
%
% Exemplos de Consultas:
% Aberta:  ?- jogo_multiplataforma(Nome, Lista).
% Aberta:  ?- jogo_multiplataforma(X, _).
% Fechada: ?- jogo_multiplataforma("Minecraft", Lista).
% Fechada: ?- jogo_multiplataforma("Grand Theft Auto: San Andreas", _).
% -----------------------------------------------------------------------
jogo_multiplataforma(Nome, Plataformas) :-
    distinct(Nome, jogo(Nome, _, _, _, _)), % Garante processar cada jogo uma vez
    findall(P, jogo(Nome, _, _, _, P), Lista),
    sort(Lista, Plataformas),
    length(Plataformas, N),
    N > 1.

% -----------------------------------------------------------------------
% Regra 2: Desenvolvedora Prolífica
% Identifica desenvolvedoras que criaram mais de 3 jogos listados na base.
% Retirado o 'cut' para permitir listar todas as desenvolvedoras prolíficas.
%
% Exemplos de Consultas:
% Aberta:  ?- desenvolvedora_prolifica(Dev, Qtd).
% Aberta:  ?- desenvolvedora_prolifica(X, 5).
% Fechada: ?- desenvolvedora_prolifica("Rockstar North", Total).
% Fechada: ?- desenvolvedora_prolifica("Capcom", _).
% -----------------------------------------------------------------------
desenvolvedora_prolifica(Dev, Total) :-
    distinct(Dev, jogo(_, _, Dev, _, _)),
    aggregate_all(count, distinct(J, jogo(J, _, Dev, _, _)), Total),
    Total > 3.

% -----------------------------------------------------------------------
% Regra 3: Autopublicação
% Identifica casos onde a desenvolvedora é a mesma empresa que a publicadora,
% ou quando o nome da publicadora contém o nome da desenvolvedora.
%
% Exemplos de Consultas:
% Aberta:  ?- autopublicado(Jogo, Empresa).
% Aberta:  ?- autopublicado(X, "Nintendo").
% Fechada: ?- autopublicado("Super Mario Bros.", _).
% Fechada: ?- autopublicado("Minecraft", _).
% -----------------------------------------------------------------------
autopublicado(Nome, Empresa) :-
    jogo(Nome, _, Dev, Pub, _),
    (   Dev = Pub
    ->  Empresa = Dev
    ;   sub_string(Pub, _, _, _, Dev), % Verifica se Dev é substring de Pub
        Empresa = Dev
    ).

% -----------------------------------------------------------------------
% Regra 4: Concorrentes no Mesmo Gênero
% Encontra dois jogos do mesmo gênero feitos por desenvolvedoras diferentes.
% Usa @< para evitar resultados espelhados (A,B e B,A).
%
% Exemplos de Consultas:
% Aberta:  ?- concorrentes_mesmo_genero(J1, J2, "Platform game").
% Aberta:  ?- concorrentes_mesmo_genero(J1, J2, G).
% Fechada: ?- concorrentes_mesmo_genero("Sonic the Hedgehog", "Super Mario Bros.", _).
% Fechada: ?- concorrentes_mesmo_genero("Halo 3", "Call of Duty 4: Modern Warfare", "First-person shooter").
% -----------------------------------------------------------------------
concorrentes_mesmo_genero(Jogo1, Jogo2, Genero) :-
    jogo(Jogo1, Genero, Dev1, _, _),
    jogo(Jogo2, Genero, Dev2, _, _),
    Jogo1 @< Jogo2, % Evita duplicatas simétricas e compara string
    Dev1 \= Dev2.

% -----------------------------------------------------------------------
% Regra 5: Catálogo da Plataforma
% Lista todos os gêneros disponíveis para uma determinada plataforma.
%
% Exemplos de Consultas:
% Aberta:  ?- generos_por_plataforma(Plat, Generos, Total).
% Aberta:  ?- generos_por_plataforma(P, _, 10).
% Fechada: ?- generos_por_plataforma("Nintendo Switch", G, T).
% Fechada: ?- generos_por_plataforma("PlayStation 4", _, _).
% -----------------------------------------------------------------------
generos_por_plataforma(Plataforma, Generos, Total) :-
    distinct(Plataforma, jogo(_, _, _, _, Plataforma)),
    findall(G, jogo(_, G, _, _, Plataforma), Lista),
    sort(Lista, Generos),
    length(Generos, Total).

% -----------------------------------------------------------------------
% Regra 6: Exclusivo
% Identifica jogos que saíram para apenas UMA plataforma específica.
%
% Exemplos de Consultas:
% Aberta:  ?- exclusivo(Nome, Plataforma).
% Aberta:  ?- exclusivo(X, "Wii").
% Fechada: ?- exclusivo("Halo 3", P).
% Fechada: ?- exclusivo("The Last of Us", "PlayStation 3").
% -----------------------------------------------------------------------
exclusivo(Nome, Plataforma) :-
    distinct(Nome, jogo(Nome, _, _, _, _)),
    findall(P, jogo(Nome, _, _, _, P), Plats),
    sort(Plats, PlatsUnicas),
    PlatsUnicas = [Plataforma].

% -----------------------------------------------------------------------
% Regra 7: Parceria Forte
% Identifica uma relação recorrente (2 ou mais jogos) onde uma Dev 
% faz o jogo e uma Pub (diferente) publica, indicando parceria comercial.
%
% Exemplos de Consultas:
% Aberta:  ?- parceria_forte(Dev, Pub, N).
% Aberta:  ?- parceria_forte("Game Freak", Pub, N).
% Fechada: ?- parceria_forte("Bungie", "Microsoft Game Studios", _).
% Fechada: ?- parceria_forte("Rare", "Nintendo", _).
% -----------------------------------------------------------------------
parceria_forte(Dev, Pub, NumJogos) :-
    distinct([Dev, Pub], jogo(_, _, Dev, Pub, _)),
    Dev \= Pub,
    aggregate_all(count, distinct(N, jogo(N, _, Dev, Pub, _)), NumJogos),
    NumJogos >= 2.

% -----------------------------------------------------------------------
% Regra 8: Ranking de Gêneros
% Produz uma lista ordenada dos gêneros com mais jogos na base.
%
% Exemplos de Consultas:
% Aberta:  ?- ranking_generos(L).
% Aberta:  ?- ranking_generos([Top1|_]).
% Fechada: ?- ranking_generos(Lista), member(30-"Action game", Lista). % Exemplo hipotético
% Fechada: ?- ranking_generos([]). % Deve falhar se houver dados
% -----------------------------------------------------------------------
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
    % Ordena pela Quantidade (primeiro elemento do par) de forma decrescente
    sort(1, @>=, Pares, ListaOrdenada).