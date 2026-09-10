/* Painel de revisão de gastos — leitura dos dados e montagem da página.
   Valores dos JSONs vêm em R$ mil, inteiros. */

const estado = {
  painel: null,
  acoes: [],
  metrica: "loa",
  regra: null,
  mostrar: 200,
  ordem: { campo: "dloa", desc: true, absoluto: true }
};

const $ = (s) => document.querySelector(s);
const $$ = (s) => Array.from(document.querySelectorAll(s));

/* ---------------------------------------------------------------- formato */

// Recebe R$ mil e devolve texto curto.
function rs(mil) {
  if (mil === null || mil === undefined || isNaN(mil)) return "—";
  const v = mil * 1000;
  const a = Math.abs(v);
  const sinal = v < 0 ? "\u2212" : "";
  const n = (x, d) => x.toLocaleString("pt-BR", { maximumFractionDigits: d });
  if (a >= 1e12) return sinal + n(a / 1e12, 2) + " tri";
  if (a >= 1e9) return sinal + n(a / 1e9, 1) + " bi";
  if (a >= 1e6) return sinal + n(a / 1e6, 1) + " mi";
  if (a >= 1e3) return sinal + n(a / 1e3, 0) + " mil";
  return sinal + n(a, 0);
}

function pct(x) {
  if (x === null || x === undefined || isNaN(x)) return "—";
  const s = (x * 100).toLocaleString("pt-BR", { maximumFractionDigits: 1 });
  return (x > 0 ? "+" : "") + s + "%";
}

const classe = (x) => (x > 0 ? "pos" : x < 0 ? "neg" : "");

/* ------------------------------------------------------------------ carga */

async function carregar() {
  try {
    const r = await fetch("dados/painel.json", { cache: "no-store" });
    if (!r.ok) throw new Error(r.status);
    estado.painel = await r.json();
  } catch (e) {
    $("#subtitulo").textContent =
      "Os dados ainda não foram gerados. Rode o workflow \u201cAtualizar painel\u201d nas Actions do repositório.";
    return;
  }

  desenharTopo();
  desenharPlacar();
  desenharRegua();
  desenharGraficos();

  try {
    const r = await fetch("dados/acoes.json", { cache: "no-store" });
    estado.acoes = await r.json();
  } catch (e) {
    estado.acoes = [];
  }
  montarCabecalho();
  prepararFiltros();
  desenharTabela();
}

function desenharTopo() {
  const m = estado.painel.meta;
  if (m.demo) {
    const aviso = document.createElement("div");
    aviso.className = "aviso-demo";
    aviso.textContent =
      "Números inventados, só para conferir o layout. A primeira execução do workflow " +
      "\u201cAtualizar painel\u201d substitui estes arquivos pelos dados do SIOP.";
    document.querySelector("main").prepend(aviso);
  }
  const partes = [
    "Exercício " + m.exercicio,
    "dados de " + m.atualizado_em,
    "valores a preços de " + m.base_deflator
  ];
  if (!m.tem_historico) {
    partes.push("sem histórico mensal: a comparação com o ano anterior está desligada");
  }
  $("#subtitulo").textContent = partes.join(" · ");
}

/* ----------------------------------------------------------------- placar */

function desenharPlacar() {
  const t = estado.painel.meta.totais;
  const execucao = t.dotacao ? t.empenhado / t.dotacao : null;
  const desvio = t.esperado ? t.empenhado - t.esperado : null;
  const desvioPc = t.esperado ? t.empenhado / t.esperado - 1 : null;
  const varReal = t.ant ? (t.emp_comparavel || t.empenhado) / t.ant - 1 : null;

  const cartoes = [
    {
      rot: "Dotação atual",
      val: rs(t.dotacao),
      pe: "LOA de " + rs(t.loa)
    },
    {
      rot: "Empenhado até hoje",
      val: rs(t.empenhado),
      pe: execucao === null ? "" : pct(execucao).replace("+", "") + " da dotação"
    },
    {
      rot: "Contra o esperado para a data",
      val: rs(desvio),
      pe: desvioPc === null ? "" : pct(desvioPc) + " · esperado " + rs(t.esperado),
      cor: classe(desvio)
    },
    {
      rot: "Contra o ano anterior, em termos reais",
      val: varReal === null ? "—" : pct(varReal),
      pe: varReal === null ? "histórico mensal ainda não gerado"
                           : "mesmo período de " + (estado.painel.meta.exercicio - 1),
      cor: classe(varReal)
    }
  ];

  $("#placar").innerHTML = cartoes
    .map(
      (c) =>
        '<div class="cartao">' +
        '<span class="cartao-rot">' + c.rot + "</span>" +
        '<span class="cartao-val ' + (c.cor || "") + '">' + c.val + "</span>" +
        '<span class="cartao-pe">' + (c.pe || "") + "</span>" +
        "</div>"
    )
    .join("");
}

/* ------------------------------------------------------------------ régua */

function rotuloRegra(id) {
  const o = (estado.painel.oportunidades || []).find((x) => x.id === id);
  return o ? o.rotulo : id;
}

function desenharRegua() {
  const alvo = $("#regua");
  const ativas = estado.painel.oportunidades.filter((o) => o.n > 0);
  alvo.innerHTML = "";

  ativas.forEach((o) => {
    const b = document.createElement("button");
    b.className = "faixa";
    b.type = "button";
    b.setAttribute("aria-pressed", "false");
    b.dataset.regra = o.id;
    b.innerHTML =
      '<span class="faixa-rot">' + o.rotulo + "</span>" +
      '<span class="faixa-linha"><span class="faixa-n">' + o.n.toLocaleString("pt-BR") +
      '</span><span class="faixa-rs">' +
      (o.espaco ? rs(o.espaco) + " envolvidos" : rs(o.dotacao) + " de dotação") +
      "</span></span>" +
      '<span class="faixa-desc">' + (o.descricao || "") + "</span>";
    b.addEventListener("click", () => alternarRegra(o.id));
    alvo.appendChild(b);
  });

  // Completa a última fileira da grade com células brancas.
  const colunas = 4;
  const sobra = (colunas - (ativas.length % colunas)) % colunas;
  for (let i = 0; i < sobra; i++) {
    const d = document.createElement("div");
    d.className = "faixa-vazia";
    d.setAttribute("aria-hidden", "true");
    alvo.appendChild(d);
  }

  const total = estado.painel.meta.totais.n_sinalizadas;
  if (!ativas.length) {
    alvo.innerHTML = '<p class="vazio" style="padding:16px">Nenhuma ação atingiu os limiares de triagem hoje.</p>';
  } else if (total) {
    $("#resumo-atencao").textContent =
      total.toLocaleString("pt-BR") + " ações acionaram ao menos uma das " + ativas.length +
      " regras de triagem. Clique numa categoria para filtrar a lista de ações lá embaixo.";
  }
}

function alternarRegra(id) {
  estado.regra = estado.regra === id ? null : id;
  estado.mostrar = 200;
  $$(".faixa").forEach((b) =>
    b.setAttribute("aria-pressed", String(b.dataset.regra === estado.regra))
  );
  desenharTabela();
  if (estado.regra) {
    $("#tabela").scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

/* --------------------------------------------------------------- gráficos */

function desenharGraficos() {
  const ag = estado.painel.agregados;
  barras("#g-funcao", ag.funcao);
  barras("#g-orgao", ag.orgao);
  barras("#g-uo", ag.uo);
  barras("#g-gnd", ag.gnd);
}

function barras(sel, dados) {
  const alvo = $(sel);
  const campo = estado.metrica === "loa" ? "d_loa" : "d_ano";
  const campoP = estado.metrica === "loa" ? "p_loa" : "p_ano";

  const linhas = dados
    .filter((d) => d[campo] !== null && d[campo] !== undefined && Math.abs(d[campo]) > 0)
    .sort((a, b) => Math.abs(b[campo]) - Math.abs(a[campo]))
    .slice(0, 25);

  if (!linhas.length) {
    alvo.innerHTML = '<p class="vazio">Sem dados para este parâmetro. O histórico mensal precisa ser gerado.</p>';
    return;
  }

  const max = Math.max(...linhas.map((d) => Math.abs(d[campo])));
  alvo.innerHTML = linhas
    .map((d) => {
      const v = d[campo];
      const larg = (Math.abs(v) / max) * 50; // metade da faixa
      const cor = v > 0 ? "var(--acima)" : "var(--abaixo)";
      const barra = v > 0
        ? '<span style="left:50%;width:' + larg + '%;background:' + cor + '"></span>'
        : '<span style="right:50%;width:' + larg + '%;background:' + cor + '"></span>';
      const nome = (d.nome || d.cod || "").toString();
      return (
        '<div class="barra-linha">' +
          '<div class="barra-rot" title="' + nome.replace(/"/g, "") + '">' + nome + "</div>" +
          '<div class="barra-tri"><i class="eixo"></i>' + barra + "</div>" +
          '<div class="barra-val ' + classe(v) + '">' + rs(v) +
            "<br><small>" + pct(d[campoP]) + "</small></div>" +
        "</div>"
      );
    })
    .join("");
}

$$(".chave").forEach((b) =>
  b.addEventListener("click", () => {
    estado.metrica = b.dataset.metrica;
    $$(".chave").forEach((x) => {
      const on = x === b;
      x.classList.toggle("ativo", on);
      x.setAttribute("aria-checked", String(on));
    });
    desenharGraficos();
  })
);

/* ---------------------------------------------------------------- colunas */

const COLUNAS = [
  { campo: "acn",  rot: "Ação", tipo: "texto",
    dica: "Nome e código da ação orçamentária",
    cel: (a) => (a.acn || "") + '<br><span class="cod">' + (a.ac || "") + "</span>" },
  { campo: "uon",  rot: "Unidade orçamentária", tipo: "texto",
    dica: "Unidade que executa e órgão a que pertence",
    cel: (a) => (a.uon || "") + '<br><span class="cod">' + (a.orgn || "") + "</span>" },
  { campo: "fnn",  rot: "Função", tipo: "texto",
    dica: "Função orçamentária",
    cel: (a) => a.fnn || "" },
  { campo: "loa",  rot: "LOA", tipo: "num",
    dica: "Dotação inicial aprovada na Lei Orçamentária Anual",
    cel: (a) => rs(a.loa) },
  { campo: "dot",  rot: "Dotação atual", tipo: "num",
    dica: "LOA mais créditos adicionais abertos até hoje",
    cel: (a) => rs(a.dot) + "<br><small>" + pct(a.pdot) + " vs LOA</small>" },
  { campo: "emp",  rot: "Empenhado", tipo: "num",
    dica: "Empenhado acumulado no exercício",
    cel: (a) => rs(a.emp) + "<br><small>" + pct(a.pexec).replace("+", "") + " da dotação</small>" },
  { campo: "esp",  rot: "Esperado hoje", tipo: "num",
    dica: "Quanto a ação teria empenhado a esta altura do ano, pelo ritmo do exercício anterior",
    cel: (a) => rs(a.esp) },
  { campo: "dloa", rot: "Desvio contra a LOA", tipo: "num", abs: true,
    dica: "Empenhado menos o esperado para esta data",
    cel: (a) => rs(a.dloa) + "<br><small>" + pct(a.prit) + " de ritmo</small>",
    cor: (a) => classe(a.dloa) },
  { campo: "dano", rot: "Variação real no ano", tipo: "num", abs: true,
    dica: "Empenhado menos o acumulado até o mesmo mês do ano anterior, a preços de hoje",
    cel: (a) => rs(a.dano) + "<br><small>" + pct(a.pano) + "</small>",
    cor: (a) => classe(a.dano) },
  { campo: "sinais", rot: "Sinais", tipo: "num",
    dica: "Regras de triagem acionadas por esta ação",
    cel: (a) =>
      (a.ops || "").split(",").filter(Boolean)
        .map((s) => '<span class="marca">' + rotuloRegra(s) + "</span>").join("") }
];

function montarCabecalho() {
  $("#cabecalho-tabela").innerHTML = COLUNAS.map(
    (c, i) =>
      '<th class="' + (c.tipo === "num" ? "n " : "") + 'ordenavel" data-i="' + i +
      '" title="' + c.dica + '" tabindex="0" role="button" aria-sort="none">' +
      c.rot + '<span class="seta"></span></th>'
  ).join("");

  $$("#cabecalho-tabela th").forEach((th) => {
    const acionar = () => ordenarPor(Number(th.dataset.i));
    th.addEventListener("click", acionar);
    th.addEventListener("keydown", (e) => {
      if (e.key === "Enter" || e.key === " ") { e.preventDefault(); acionar(); }
    });
  });
  marcarOrdem();
}

function ordenarPor(i) {
  const c = COLUNAS[i];
  if (estado.ordem.campo === c.campo) {
    estado.ordem.desc = !estado.ordem.desc;
  } else {
    estado.ordem = { campo: c.campo, desc: c.tipo === "num", absoluto: false };
  }
  estado.mostrar = 200;
  marcarOrdem();
  desenharTabela();
}

function marcarOrdem() {
  $$("#cabecalho-tabela th").forEach((th) => {
    const c = COLUNAS[Number(th.dataset.i)];
    const ativo = c.campo === estado.ordem.campo;
    th.classList.toggle("ativa", ativo);
    th.setAttribute("aria-sort", ativo ? (estado.ordem.desc ? "descending" : "ascending") : "none");
    th.querySelector(".seta").textContent = ativo ? (estado.ordem.desc ? "▾" : "▴") : "";
  });
}

/* --------------------------------------------------------------- filtros */

function prepararFiltros() {
  const unicos = (campoCod, campoNome) => {
    const m = new Map();
    estado.acoes.forEach((a) => {
      if (a[campoCod] && !m.has(a[campoCod])) m.set(a[campoCod], a[campoNome] || a[campoCod]);
    });
    return Array.from(m.entries()).sort((x, y) => String(x[1]).localeCompare(String(y[1]), "pt-BR"));
  };

  const preenche = (sel, pares) => {
    const el = $(sel);
    pares.forEach(([cod, nome]) => {
      const o = document.createElement("option");
      o.value = cod;
      o.textContent = nome;
      el.appendChild(o);
    });
  };

  preenche("#f-funcao", unicos("fn", "fnn"));
  preenche("#f-gnd", unicos("gnd", "gndn"));

  ["#busca", "#f-funcao", "#f-gnd", "#f-tipo"].forEach((s) =>
    $(s).addEventListener("input", () => {
      estado.mostrar = 200;
      desenharTabela();
    })
  );

  $("#limpar").addEventListener("click", () => {
    $("#busca").value = "";
    ["#f-funcao", "#f-gnd", "#f-tipo"].forEach((s) => ($(s).value = ""));
    estado.regra = null;
    estado.ordem = { campo: "dloa", desc: true, absoluto: true };
    $$(".faixa").forEach((b) => b.setAttribute("aria-pressed", "false"));
    estado.mostrar = 200;
    marcarOrdem();
    desenharTabela();
  });

  $("#mais").addEventListener("click", () => {
    estado.mostrar += 300;
    desenharTabela();
  });

  $("#baixar").addEventListener("click", baixarCsv);
}

function filtrar() {
  const q = $("#busca").value.trim().toLowerCase();
  const fn = $("#f-funcao").value;
  const gnd = $("#f-gnd").value;
  const tipo = $("#f-tipo").value;

  const r = estado.acoes.filter((a) => {
    if (fn && a.fn !== fn) return false;
    if (gnd && a.gnd !== gnd) return false;
    if (tipo !== "" && String(a.disc) !== tipo) return false;
    if (estado.regra && !(a.ops || "").split(",").includes(estado.regra)) return false;
    if (q) {
      const alvo = [a.acn, a.uon, a.orgn, a.ac, a.uo, a.fnn].join(" ").toLowerCase();
      if (!alvo.includes(q)) return false;
    }
    return true;
  });

  const { campo, desc, absoluto } = estado.ordem;
  const col = COLUNAS.find((c) => c.campo === campo);
  const texto = col && col.tipo === "texto";

  r.sort((a, b) => {
    let x = a[campo], y = b[campo];
    if (texto) {
      return String(x || "").localeCompare(String(y || ""), "pt-BR") * (desc ? -1 : 1);
    }
    x = x === null || x === undefined || isNaN(x) ? null : Number(x);
    y = y === null || y === undefined || isNaN(y) ? null : Number(y);
    if (x === null && y === null) return 0;
    if (x === null) return 1;   // vazios sempre no fim
    if (y === null) return -1;
    if (absoluto) { x = Math.abs(x); y = Math.abs(y); }
    return (desc ? y - x : x - y);
  });
  return r;
}

function desenharTabela() {
  const linhas = filtrar();
  const corpo = $("#tabela").querySelector("tbody");
  const fatia = linhas.slice(0, estado.mostrar);

  corpo.innerHTML = fatia
    .map((a) =>
      "<tr>" +
      COLUNAS.map((c) => {
        const cor = c.cor ? " " + c.cor(a) : "";
        return '<td class="' + (c.tipo === "num" ? "n" : "") + cor + '">' + c.cel(a) + "</td>";
      }).join("") +
      "</tr>"
    )
    .join("");

  const total = linhas.length;
  const soma = (k) => linhas.reduce((s, a) => s + (a[k] || 0), 0);
  $("#contagem").textContent =
    total.toLocaleString("pt-BR") + " ações no filtro atual · dotação " + rs(soma("dot")) +
    " · empenhado " + rs(soma("emp")) +
    (estado.regra ? " · categoria: " + rotuloRegra(estado.regra) : "");
  $("#mais").style.display = fatia.length < total ? "" : "none";
}

/* ------------------------------------------------------------------- csv */

function baixarCsv() {
  const linhas = filtrar();
  const cab = [
    "orgao", "orgao_nome", "uo", "uo_nome", "funcao", "funcao_nome",
    "acao", "acao_nome", "gnd", "gnd_nome", "resultado_primario", "discricionaria",
    "loa_rs_mil", "dotacao_rs_mil", "empenhado_rs_mil", "liquidado_rs_mil", "pago_rs_mil",
    "esperado_hoje_rs_mil", "ano_anterior_periodo_rs_mil",
    "desvio_loa_rs_mil", "desvio_ano_rs_mil",
    "var_dotacao", "exec_dotacao", "desvio_ritmo", "var_real",
    "perfil_esperado", "origem_perfil", "montante_envolvido_rs_mil", "sinais"
  ];
  const esc = (v) => {
    const s = v === null || v === undefined ? "" : String(v);
    return /[";\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
  };
  const corpo = linhas.map((a) =>
    [a.org, a.orgn, a.uo, a.uon, a.fn, a.fnn, a.ac, a.acn, a.gnd, a.gndn, a.rp, a.disc,
     a.loa, a.dot, a.emp, a.liq, a.pag, a.esp, a.ant, a.dloa, a.dano,
     a.pdot, a.pexec, a.prit, a.pano, a.perf, a.oper, a.espaco, a.ops]
      .map(esc).join(";")
  );
  const csv = "\ufeff" + [cab.join(";")].concat(corpo).join("\n");
  const url = URL.createObjectURL(new Blob([csv], { type: "text/csv;charset=utf-8" }));
  const a = document.createElement("a");
  a.href = url;
  a.download = "revisao-gastos-" + new Date().toISOString().slice(0, 10) + ".csv";
  a.click();
  URL.revokeObjectURL(url);
}

carregar();
