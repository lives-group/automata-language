# Ferramentas para ensino de linguagens formais

## Estrutura do projeto

* **Pasta utils**: utilidades utilizadas por todo o projeto.
   - _Arquivo set-extras.rkt_
      - funções adicionais sobre conjuntos
   - _Arquivo dot.rkt_
      - funções utilizadas para criação de imagens utilizando o graphviz.
* **Pasta finite-automata**: esta pasta é formada pelas seguintes subpastas
   - _Arquivo fa.rkt_
      - definição de tipo para autômatos finitos e funções sobre este tipo.
   - _Arquivo fa-image-builder.rkt_
      - funções para criação de imagens de autômatos.
* **Pasta finite-automata/dfa**: funcionalidades envolvendo autômatos finitos determinísticos
   - _Arquivo core.rkt_
      - definição de tipo para um autômato determinístico e de uma macro para criar tais autômatos de maneira simples.
   - _Arquivo image-builder.rkt_
      - funções adicionais para criação de figuras de autômatos determinísticos.
   - _Arquivo table-minimization.rkt_
      - Implementação de algoritmos de minimização.
   - _Arquivo dfa-operations.rkt_
      - Propriedades de fechamento de autôamtos (Projeto Caio)
   - _Arquivo automaton-correction.rkt_
      - Implementação de algoritmo de correção de exercícios de autômatos finitos determinísticos (Projeto Caio)
* **Pasta finite-automata/nfa**: funcionalidades envolvendo autômatos não determinísticos.
   - _Arquivo core.rkt_
      - definição de tipo para um autômato não determinístico e de uma macro para criar tais autômatos de maneira simples.
   - _Arquivo image-builder.rkt_
      - funções adicionais para criação de figuras de autômatos não determinísticos.
   - _Arquivo subset-construction.rkt_
      - conversão de autômatos não determinísticos em determinísticos.
        
