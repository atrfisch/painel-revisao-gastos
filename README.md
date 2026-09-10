# Painel de revisão de gastos

Triagem diária das ações orçamentárias federais que se afastam de dois parâmetros:
o previsto na LOA e o executado no mesmo período do exercício anterior, em termos reais.

Fontes: SIOP, pelo pacote R [orcamentoBR](https://cran.r-project.org/package=orcamentoBR);
execução mensal consolidada da despesa, no Portal da Transparência; IPCA, na série 433 do SGS/Banco Central.

---

## Montagem, pela interface web do GitHub

1. Crie um repositório público chamado `painel-revisao-gastos`.
2. Envie todos os arquivos desta pasta preservando os caminhos
   (**Add file → Upload files**, arrastando as pastas inteiras).
3. Em **Settings → Pages**, escolha `Deploy from a branch`, branch `main`, pasta `/docs`.
4. Em **Settings → Actions → General → Workflow permissions**, marque
   `Read and write permissions`. Sem isso o robô não consegue gravar os dados atualizados.
5. Em **Actions**, rode uma vez o workflow **Backfill mensal**. Ele monta o histórico
   mensal por ação, que é o que permite a comparação com o ano anterior. Demora, e é
   normal: são vários arquivos grandes.
6. Em **Actions**, rode o workflow **Atualizar painel**. A partir daí ele roda sozinho
   todo dia às 5h20 de Brasília.

O site sobe em `https://SEU-USUARIO.github.io/painel-revisao-gastos/`.
Enquanto o passo 6 não roda, a página exibe dados de demonstração com um aviso no topo.

---

## Estrutura

```
R/pipeline.R              coleta, cálculo dos desvios, triagem e exportação (roda todo dia)
R/backfill_mensal.R       monta o histórico mensal por ação (roda uma vez, depois todo dia 15)
.github/workflows/        os dois workflows
docs/                     o site publicado
docs/dados/painel.json    metadados, régua de oportunidades e agregados
docs/dados/acoes.json     uma linha por ação, com todas as métricas
historico/                snapshots e histórico mensal
```

Todos os limiares da metodologia estão no objeto `P`, no alto de `R/pipeline.R`.
Mexer neles é a forma prevista de calibrar o painel.

---

## Os dois parâmetros

**Contra a LOA.** Comparar empenho com dotação integral no meio do ano não informa nada,
porque nenhuma ação gasta um doze avos por mês. O valor esperado para hoje é a dotação
multiplicada pela fração do gasto anual que aquela mesma ação já havia realizado até este
mês no exercício anterior. Sem par no ano anterior, usa-se o perfil da função; em último
caso, o rateio linear no tempo. A coluna `origem_perfil` registra qual dos três valeu para
cada linha.

**Contra o ano anterior, em termos reais.** Acumulado até hoje contra acumulado até o mesmo
mês do exercício anterior, ambos a preços do mês de referência, deflacionados pelo IPCA.
A consulta ao SIOP devolve valores acumulados no instante do acesso, sem dimensão de mês —
daí a necessidade do histórico mensal do Portal da Transparência.

---

## Regras de triagem

| Regra | O que dispara | Montante envolvido |
|---|---|---|
| Subexecução persistente | Empenho 20 p.p. abaixo do ritmo esperado e execução fraca também em t−1 | Dotação menos o fechamento projetado pelo ritmo observado |
| Dotação sem execução | Execução abaixo de 5% em t e de 10% em t−1, já passada a metade do perfil anual | Dotação menos empenhado |
| Dotação inflada por créditos | Dotação 30% acima da LOA | Dotação menos LOA |
| Expansão real acelerada | Crescimento real acima de 15% contra o mesmo período | Diferença em reais |
| Retração real acentuada | Queda real acima de 15% | — |
| Execução pulverizada | Mesma ação com dotação abaixo de R$ 5 mi em 10 ou mais unidades | — |
| Concentração em dezembro | Mais de 40% do empenho de t−1 feito em dezembro | — |

Todas exigem materialidade mínima de R$ 50 milhões, salvo indicação em contrário.
Ações com resultado primário obrigatório permanecem na base e nos agregados, mas ficam
fora do bloco de despesas que demandam atenção.

---

## O que este painel não é

Uma revisão de gasto, no sentido em que OCDE e FMI usam o termo, é a análise sistemática
da despesa de base para identificar economias e realocação, amarrada ao ciclo do orçamento.
Este painel cobre o primeiro passo: identificar o que demanda atenção. Não mede eficiência,
não avalia resultado e não substitui o exame feito com a unidade responsável pela política.
O valor associado a cada regra é a ordem de grandeza do montante envolvido, não uma economia
apurada nem uma redução proposta.

---

## Limites conhecidos

- Empenho não é entrega.
- Ações criadas ou reestruturadas no exercício não têm par em t−1: a variação real fica vazia.
- Mudanças na estrutura programática quebram a série; um salto pode ser recodificação.
- O IPCA é o deflator de todas as séries, por decisão de projeto e por coerência com o
  indexador de correção usado na regra fiscal. Índices setoriais dariam leitura diferente
  para pessoal e para obras.
- O arquivo mensal do Portal da Transparência pode não trazer unidade orçamentária. O
  pipeline tenta casar por UO e ação e, se falhar, por órgão e ação. A cobertura efetiva
  aparece no log do workflow.
