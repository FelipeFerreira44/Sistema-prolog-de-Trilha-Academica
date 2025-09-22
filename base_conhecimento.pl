% =============================================================================
% Escolhi pesos pensando em como cada habilidade impacta a carreira.
% Adicionei uma característica extra por trilha pra deixar mais completo.
% =============================================================================

% Fatos das trilhas: trilha(Nome, Descricao)
trilha(inteligencia_artificial, 'Desenvolver soluções relacionadas com i.a.').
trilha(desenvolvimento_web, 'Construir sites e apps na web, com front e back-end.').
trilha(seguranca_da_informacao, 'Proteger dados e sistemas.').
trilha(ciencia_de_dados, 'Trabalhar com big data.').
trilha(redes_e_infraestrutura, 'Configurar e manter redes.').

% Perfis: perfil(Trilha, Caracteristica, Peso)
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
perfil(redes_e_infraestrutura, troubleshooting, 4).  

% Perguntas: pergunta(ID, Texto, Caracteristica) - IDs contínuos.
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
pergunta(11, 'Gosta de identificar e consertar falhas técnicas?', troubleshooting).
