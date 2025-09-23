% =============================================================================
% iniciar/0 chama tudo. faz_perguntas/0 roda interativo. exibe_resultado/1 mostra.
% Usei read_line pra input limpo, e loop pra validar.
% =============================================================================

:- use_module(library(lists)).  % Pra member, etc.

% Inicia o sistema: Limpa, pergunta se interativo, calcula e exibe.
iniciar :-
    retractall(resposta(_, _)),
    faz_perguntas,
    coleta_respostas(AtualRespostas),
    recomenda(AtualRespostas, Lista),
    exibe_resultado(Lista).

% Coleta respostas atuais como lista pra passar pro motor.
coleta_respostas(Lista) :-
    findall(resposta(ID, Resp), resposta(ID, Resp), Lista).

% Faz perguntas: Itera por IDs não respondidos, valida input.
faz_perguntas :-
    pergunta(ID, Texto, _),
    \+ resposta(ID, _),
    format('~w (s/n): ', [Texto]),
    flush_output,
    valida_e_assert(ID),
    fail.  % Backtrack pra próxima.
faz_perguntas :-
    writeln('Obrigado pelas respostas! Calculando...').

% Valida input e asserta (loop até s/n).
valida_e_assert(ID) :-
    read_line_to_codes(user_input, Codes),
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Lower),
    (Lower = 's' ->
        assertz(resposta(ID, s))
    ; Lower = 'n' ->
        assertz(resposta(ID, n))
    ;   writeln('Erro: Digite só "s" ou "n". Tente de novo.'),
        valida_e_assert(ID)
    ).

% Exibe resultado: Ranking com scores, desc e justificativa.
exibe_resultado(Lista) :-
    writeln('=== SUAS TRILHAS RECOMENDADAS ==='),
    (Lista = [] ->
        writeln('Nenhuma trilha com match forte. Reveja interesses!')
    ;   lista_com_score(Lista, 1)
    ).

% Lista numerada com scores e just.
lista_com_score([], _).
lista_com_score([Trilha | Resto], Num) :-
    trilha(Trilha, Desc),
    coleta_respostas(Resps),
    calcula_pontuacao(Trilha, Resps, Score),
    format('~d. ~w (Score: ~d)~n  Descrição: ~w~n', [Num, Trilha, Score, Desc]),
    justifica(Trilha, Resps),
    Num1 is Num + 1,
    lista_com_score(Resto, Num1).

% Justifica: Chars com 's'.
justifica(Trilha, Resps) :-
    bagof(Char, 
          (perfil(Trilha, Char, _), 
           tem_resposta_sim(Char, Resps)), 
          Matches),
    writeln('  Por quê? Seus "sim" em:'),
    imprime_matches(Matches).

imprime_matches([]) :- nl.
imprime_matches([Char | Resto]) :-
    format('  - ~w~n', [Char]),
    imprime_matches(Resto).

