# Ferramentas para ensino de linguagens formais

## Estrutura do projeto

* Pasta utils: utilidades utilizadas por todo o projeto.
     ** Arquivo set-extras.rkt: funções adicionais sobre conjuntos
     ** Arquivo dot.rkt: funções utilizadas para criação de imagens utilizando o graphviz.
* Pasta finite-automata: esta pasta é formada pelas seguintes subpastas
     ** Arquivo fa.rkt: definição de tipo para autômatos finitos e funções sobre este tipo.
     ** Arquivo fa-image-builder.rkt: funções para criação de imagens de autômatos.
* Pasta finite-automata/dfa: funcionalidades envolvendo autômatos finitos determinísticos
     ** Arquivo core.rkt: definição de tipo para um autômato determinístico e de uma macro
        para criar tais autômatos de maneira simples.
     ** Arquivo image-builder.rkt: funções adicionais para criação de figuras de autômatos 
        determinísticos.
     ** Arquivo table-minimization.rkt: Implementação de algoritmos de minimização.
     ** Arquivo dfa-operations.rkt: Propriedades de fechamento de autôamtos (Projeto Caio)
     ** Arquivo automaton-correction.rkt: Implementação de algoritmo de correção de 
        exercícios de autômatos finitos determinísticos (Projeto Caio)
* Pasta finite-automata/nfa: funcionalidades envolvendo autômatos não determinísticos.
     ** Arquivo core.rkt: definição de tipo para um autômato não determinístico e 
        de uma macro para criar tais autômatos de maneira simples.
     ** Arquivo image-builder.rkt: funções adicionais para criação de figuras de autômatos 
        não determinísticos.
     ** Arquivo subset-construction.rkt: conversão de autômatos não determinísticos em 
        determinísticos.
        
