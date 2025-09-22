% =============================================================================
% Usei bagof pra coletar pesos, e setof pra ordenar. calcula_pontuacao/3 recebe Respostas como lista .
% Se Respostas vazia, usa o dynamic resposta/2.
% =============================================================================

:- dynamic(resposta/2).  % Pra armazenar respostas se não passadas.

% Calcula pontuação: calcula_pontuacao(Trilha, Respostas, Pontuacao)
% Respostas é lista de resposta(ID, s/n). Soma pesos onde char tem 's'.
% Se Respostas = [], usa o dynamic.
calcula_pontuacao(Trilha, Respostas, Pontuacao) :-
    (Respostas = [] ->
        bagof(Peso, 
              (perfil(Trilha, Char, Peso), 
               tem_resposta_sim(Char, Respostas)), 
              Pesos)
    ;   bagof(Peso, 
              (perfil(Trilha, Char, Peso), 
               tem_resposta_sim(Char, Respostas)), 
              Pesos)
    ),
    sum_list(Pesos, Pontuacao), !.
calcula_pontuacao(_, _, 0).  % Default 0 se nada.

% Auxiliar: Verifica se char tem resposta 's' (via ID da pergunta).
tem_resposta_sim(Char, Respostas) :-
    pergunta(ID, _, Char),
    member(resposta(ID, s), Respostas) ; 
    (Respostas = [], resposta(ID, s)).  % Fallback pro dynamic.

% Recomenda: recomenda(Respostas, ListaTrilhas)
recomenda(Respostas, ListaRecomendacoes) :-
    setof(Pontuacao-Trilha, 
          (trilha(Trilha, _), 
           calcula_pontuacao(Trilha, Respostas, Pontuacao)), 
          ListaPontos),
    ordena_decrescente(ListaPontos, ListaOrdenada),
    remove_pontos(ListaOrdenada, ListaRecomendacoes).

% Ordena decrescente (maior pontuação primeiro)
ordena_decrescente([], []).
ordena_decrescente([Pont-Trilha | Resto], [Pont-Trilha | OrdenadoResto]) :-
    forall(member(PontR-TrilhaR, Resto), Pont >= PontR),
    ordena_decrescente(Resto, OrdenadoResto).  % Estilo bubble-like pra simplicidade.

% Remove pontos
remove_pontos([], []).
remove_pontos([_ - Trilha | Cauda], [Trilha | Resto]) :-
    remove_pontos(Cauda, Resto).

