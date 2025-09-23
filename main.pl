

% =============================================================================
% base_conhecimento.pl
% =============================================================================

% Trilhas: trilha(Nome, Descricao).
trilha(inteligencia_artificial, 'Desenvolver soluções relacionadas com i.a.').
trilha(desenvolvimento_web, 'Construir sites e apps na web, com front e back-end.').
trilha(seguranca_da_informacao, 'Proteger dados e sistemas.').
trilha(ciencia_de_dados, 'Trabalhar com big data.').
trilha(redes_e_infraestrutura, 'Configurar e manter redes.').

% Perfis: perfil(Trilha, Caracteristica, Peso) 1 a 5.
perfil(inteligencia_artificial, logica, 5).
perfil(inteligencia_artificial, matematica_estatistica, 4).
perfil(inteligencia_artificial, programacao, 5).
perfil(inteligencia_artificial, criatividade, 3).

perfil(desenvolvimento_web, design_visual, 4).
perfil(desenvolvimento_web, programacao, 4).
perfil(desenvolvimento_web, criatividade, 4).

perfil(seguranca_da_informacao, redes, 5).
perfil(seguranca_da_informacao, pensamento_critico, 4).
perfil(seguranca_da_informacao, etica, 3).

perfil(ciencia_de_dados, matematica_estatistica, 5).
perfil(ciencia_de_dados, analise_dados, 5).
perfil(ciencia_de_dados, programacao, 3).

perfil(redes_e_infraestrutura, redes, 5).
perfil(redes_e_infraestrutura, hardware, 4).
perfil(redes_e_infraestrutura, resolucao_de_problemas, 4).  

% Perguntas: pergunta(ID, Texto, Caracteristica) 
pergunta(1, 'Gosta de lógica?', logica).
pergunta(2, 'Gosta de matemática e estatística?', matematica_estatistica).
pergunta(3, 'Curte programar códigos?', programacao).
pergunta(4, 'Tem interesse pra design e visuais?', design_visual).
pergunta(5, 'Sabe ou quer aprender sobre redes de computadores?', redes).
pergunta(6, 'Tem um pensamento crítico?', pensamento_critico).
pergunta(7, 'Tem interesse em mexer com dados?', analise_dados).
pergunta(8, 'Entende de hardware ou tem vontade de aprender?', hardware).
pergunta(9, 'Tem ideias criativas?', criatividade).
pergunta(10, 'Pensa em ética e seus impactos em soluções tech?', etica).
pergunta(11, 'Gosta de identificar e consertar falhas técnicas?', resolucao_de_problemas).

% =============================================================================
% motor_inferencia.pl
% =============================================================================

:- dynamic(resposta/2).  % Armazena respostas em andamento.

%  Pega via ID, default 'n'.
resposta_por_caracteristica(Caracteristica, Resposta) :-
    pergunta(ID, _, Caracteristica),
    (resposta(ID, R) -> Resposta = R ; Resposta = n).

% findall coleta todos os pesos de 's', e os soma, considera vazio como 0.
calcula_pontuacao(Trilha, Pontuacao) :-
    findall(Peso, 
            (perfil(Trilha, Char, Peso), 
             resposta_por_caracteristica(Char, s)), 
            Pesos),
    sum_list(Pesos, Pontuacao).

%  findall é usado em todas as trilhas, sort as trilhas em ordem decrescente, cut para única solução.
recomenda(ListaRecomendacoes) :-
    findall(Pontuacao-Trilha, 
            (trilha(Trilha, _), 
             calcula_pontuacao(Trilha, Pontuacao)), 
            ListaPontos),
    sort(1, @>=, ListaPontos, ListaOrdenada),  % Decrescente por pontuação.
    remove_pontuacao(ListaOrdenada, ListaRecomendacoes), !.  % Cut usado para evitar repetições.

remove_pontuacao([], []).
remove_pontuacao([_ - Trilha | Resto], [Trilha | RecomResto]) :-
    remove_pontuacao(Resto, RecomResto).

% =============================================================================
% interface.pl
% =============================================================================

% Checa se já tem respostas antes de limpar (preserva testes).
iniciar :-
    (tem_respostas ->
        % Se tem, pula limpeza e perguntas.
        true
    ;   % Senão, limpa e pergunta.
        retractall(resposta(_, _)),
        faz_perguntas
    ),
    recomenda(Lista), !,  
    exibe_resultado(Lista).

tem_respostas :- resposta(_, _).

% Recursão por ID não respondido, valida input.
faz_perguntas :-
    pergunta(ID, Texto, _),
    \+ resposta(ID, _),
    format('~w (s/n): ', [Texto]),
    flush_output,
    le_e_assert(ID),
    fail.
faz_perguntas :-
    writeln('Perguntas concluídas!').

% válida as respostas e entra em um Loop até receber 's' ou 'n' em caso de outra resposta.
le_e_assert(ID) :-
    read_line_to_codes(user_input, Codes),
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Lower),
    (Lower = 's' ->
        assertz(resposta(ID, s))
    ; Lower = 'n' ->
        assertz(resposta(ID, n))
    ;   writeln('Inválido! Use s ou n.'),
        le_e_assert(ID)
    ).

% Ranking numerado com pontuação.
exibe_resultado([]) :-
    writeln('Sem trilhas fortes. Tente mais respostas').
exibe_resultado(Lista) :-
    writeln('=== RECOMENDAÇÕES (ordem de compatibilidade) ==='),
    exibe_lista(Lista, 1).

exibe_lista([], _).
exibe_lista([Trilha | Resto], Num) :-
    trilha(Trilha, Desc),
    calcula_pontuacao(Trilha, Score),
    format('~d. ~w (Pontuação: ~d)~n   Descrição: ~w~n', [Num, Trilha, Score, Desc]),
    justifica_recomendacao(Trilha),
    Num1 is Num + 1,
    exibe_lista(Resto, Num1).

% findall pega todos os chars com 's' para a trilha específica.
justifica_recomendacao(Trilha) :-
    findall(Char, 
            (perfil(Trilha, Char, _), 
             resposta_por_caracteristica(Char, s)), 
            Matches),
    (Matches = [] ->
        writeln('   Justificativa: Poucos matches.')
    ;   writeln('   Justificativa (seus "sim"):'),
        lista_matches(Matches)
    ),
    nl.

lista_matches([]).
lista_matches([Char | Resto]) :-
    format('   - ~w~n', [Char]),
    lista_matches(Resto).

% Modo teste
testar(N) :-
    carrega_teste(N),
    recomenda(Lista), !,
    exibe_resultado(Lista).

% =============================================================================
% perfis.pl
% =============================================================================

% Teste 1: Lógico/dados
carrega_teste(1) :-
    retractall(resposta(_, _)),
    assertz(resposta(1, s)),  % logica
    assertz(resposta(2, s)),  % matematica_estatistica
    assertz(resposta(3, s)),  % programacao
    assertz(resposta(7, s)),  % analise_dados
    % Outros default n.
    writeln('-- Teste 1 carregado: Foco em lógica e dados.').
    % Para debug: Descomente abaixo e rode verifica_respostas.
  

% Teste 2: Redes/infraestrutura 
carrega_teste(2) :-
    retractall(resposta(_, _)),
    assertz(resposta(5, s)),  % redes
    assertz(resposta(6, s)),  % pensamento_critico
    assertz(resposta(8, s)),  % hardware
    assertz(resposta(10, s)), % etica
    assertz(resposta(11, s)), % resolucao_de_problemas
    writeln('-- Teste 2 carregado: Foco em redes e resolução de problemas.').

% Teste 3: Web/criativo 
carrega_teste(3) :-
    retractall(resposta(_, _)),
    assertz(resposta(3, s)),  % programacao
    assertz(resposta(4, s)),  % design_visual
    assertz(resposta(9, s)),  % criatividade
    writeln('-- Teste 3 carregado: Foco em desenvolvimento web e criatividade.').

%  Lista todas respostas assertadas.
verifica_respostas :-
    findall(ID-R, resposta(ID, R), List),
    format('Respostas assertadas: ~w~n', [List]).

%  Imprime pontuações para todas trilhas.
verifica_pontuacao :-
    findall(P-Trilha, (trilha(Trilha, _), calcula_pontuacao(Trilha, P)), Lista),
    format('Pontuações: ~w~n', [Lista]).

% =============================================================================
% Interativo:  iniciar.
% Teste:  testar(1). 
% =============================================================================
