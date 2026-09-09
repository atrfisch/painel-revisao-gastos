# Painel de revisão de gastos

Triagem diária das ações orçamentárias federais que se afastam de três parâmetros:
o previsto na LOA, o executado no mesmo período do exercício anterior em termos reais,
e o crescimento admitido pelo Regime Fiscal Sustentável.

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

## Os três parâmetros

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

**Contra o Regime Fiscal Sustentável.** A LC 200/2023 admite crescimento real da despesa
primária entre 0,6% e 2,5% ao ano, dentro desse intervalo atrelado a 70% da variação real
da receita primária quando a meta de resultado do exercício anterior foi cumprida, e a 50%
quando não foi. O painel toma a taxa de referência do exercício, definida em `RFS_TAXA` no
workflow, e calcula quanto cada ação teria empenhado se tivesse crescido exatamente nela.
Como o indexador de correção do próprio limite é o IPCA, o deflator do painel e a régua do
regime falam a mesma língua.

Duas ressalvas pesam. O limite legal incide sobre o agregado da despesa primária de cada
Poder e órgão autônomo, nunca sobre a ação individual: uma ação crescer acima da taxa não
configura descumprimento de nada, e o conjunto pode caber no limite com muitas ações acima
e muitas abaixo. E parte da despesa está fora do limite por disposição legal, de modo que a
soma das ações do painel não reproduz a base de cálculo do regime. Por isso o painel também
mostra, no alto do bloco de gráficos, o crescimento real do agregado comparável ao lado da
banda de 0,6% a 2,5%.

---

## Regras de triagem

| Regra | O que dispara | Montante envolvido |
|---|---|---|
| Subexecução persistente | Empenho 20 p.p. abaixo do ritmo esperado e execução fraca também em t−1 | Dotação menos o fechamento projetado pelo ritmo observado |
| Dotação sem execução | Execução abaixo de 5% em t e de 10% em t−1, já passada a metade do perfil anual | Dotação menos empenhado |
| Dotação inflada por créditos | Dotação 30% acima da LOA | Dotação menos LOA |
| Expansão real acelerada | Crescimento real acima de 15% contra o mesmo período | Diferença em reais |
| Acima do crescimento do regime fiscal | Crescimento real acima da taxa de referência do RFS | Diferença contra o empenho que a taxa implicaria |
| Retração real acentuada | Queda real acima de 15% | — |
| Execução pulverizada | Mesma ação com dotação abaixo de R$ 5 mi em 10 ou mais unidades | — |
| Concentração em dezembro | Mais de 40% do empenho de t−1 feito em dezembro | — |
| Desvio atípico na função | Escore robusto acima de 3 dentro da função | — |

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
  indexador do próprio limite do regime fiscal. Índices setoriais dariam leitura diferente
  para pessoal e para obras.
- O arquivo mensal do Portal da Transparência pode não trazer unidade orçamentária. O
  pipeline tenta casar por UO e ação e, se falhar, por órgão e ação. A cobertura efetiva
  aparece no log do workflow.
