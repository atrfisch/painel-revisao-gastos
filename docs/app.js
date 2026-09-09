/* Painel de revisão de gastos — leitura dos dados e montagem da página.
   Valores dos JSONs vêm em R$ mil, inteiros. */

const estado = {
  painel: null,
  acoes: [],
  metrica: "loa",
  regra: null,
  mostrar: 200
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
  if (a >= 1e9) return sinal + (a / 1e9).toLocaleString("pt-BR", { maximumFractionDigits: 1 }) + " bi";
  if (a >= 1e6) return sinal + (a / 1e6).toLocaleString("pt-BR", { maximumFractionDigits: 1 }) + " mi";
  if (a >= 1e3) return sinal + (a / 1e3).toLocaleString("pt-BR", { maximumFractionDigits: 0 }) + " mil";
  return sinal + a.toLocaleString("pt-BR", { maximumFractionDigits: 0 });
}

function pct(x) {
  if (x === null || x === undefined || isNaN(x)) return "—";
  const s = (x * 100).toLocaleString("pt-BR", { maximumFractionDigits: 1 });
  return (x > 0 ? "+" : "") + s + "%";
}

const classe = (x) => (x > 0 ? "pos" : x < 0 ? "neg" : "");

// Distância em pontos percentuais até a taxa de referência do regime fiscal.
function ppt(x) {
  if (x === null || x === undefined || isNaN(x)) return "—";
  const s = (x * 100).toLocaleString("pt-BR", { maximumFractionDigits: 1 });
  return (x > 0 ? "+" : "") + s + " p.p.";
}

const ROTULOS = {
  subexecucao: "Subexecução persistente",
  simbolica: "Dotação sem execução",
  credito: "Dotação inflada por créditos",
  expansao: "Expansão real acelerada",
  retracao: "Retração real acentuada",
  fragmentacao: "Execução pulverizada",
  dezembro: "Concentração em dezembro",
  outlier: "Desvio atípico na função",
  acima_rfs: "Acima do crescimento do regime fiscal"
};

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
  desenharRegua();
  desenharGraficos();

  try {
    const r = await fetch("dados/acoes.json", { cache: "no-store" });
    estado.acoes = await r.json();
  } catch (e) {
    estado.acoes = [];
  }
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
    "valores a preços de " + m.base_deflator,
    rs(m.totais.empenhado) + " empenhados de " + rs(m.totais.dotacao) + " de dotação"
  ];
  if (!m.tem_historico) {
    partes.push("sem histórico mensal: a comparação com o ano anterior está desligada");
  }
  $("#subtitulo").textContent = partes.join(" · ");
}

/* ------------------------------------------------------------------ régua */

function desenharRegua() {
  const alvo = $("#regua");
  alvo.innerHTML = "";
  estado.painel.oportunidades
    .filter((o) => o.n > 0)
    .forEach((o) => {
      const b = document.createElement("button");
      b.className = "faixa";
      b.type = "button";
      b.setAttribute("aria-pressed", "false");
      b.dataset.regra = o.id;
      b.title = o.descricao;
      b.innerHTML =
        '<span class="faixa-rot">' + o.rotulo + "</span>" +
        '<span class="faixa-n">' + o.n.toLocaleString("pt-BR") + "</span>" +
        '<span class="faixa-rs">' +
        (o.espaco ? rs(o.espaco) + " envolvidos" : rs(o.dotacao) + " de dotação") +
        "</span>";
      b.addEventListener("click", () => alternarRegra(o.id));
      alvo.appendChild(b);
    });

  if (!alvo.children.length) {
    alvo.innerHTML = '<p class="vazio" style="padding:16px">Nenhuma ação atingiu os limiares de triagem hoje.</p>';
  }
}

function alternarRegra(id) {
  estado.regra = estado.regra === id ? null : id;
  estado.mostrar = 200;
  $$(".faixa").forEach((b) =>
    b.setAttribute("aria-pressed", String(b.dataset.regra === estado.regra))
  );
  desenharTabela();
  if (estado.regra) $("#tabela").scrollIntoView({ behavior: "smooth", block: "start" });
}

/* --------------------------------------------------------------- gráficos */

function desenharGraficos() {
  const ag = estado.painel.agregados;
  notaRfs();
  barras("#g-funcao", ag.funcao);
  barras("#g-orgao", ag.orgao);
  barras("#g-uo", ag.uo);
  barras("#g-gnd", ag.gnd);
}

function notaRfs() {
  const el = $("#nota-rfs");
  const r = estado.painel.meta.rfs;
  if (estado.metrica !== "rfs" || !r) { el.hidden = true; return; }
  el.hidden = false;
  const cresc = r.crescimento_agregado;
  const dentro = cresc >= r.piso && cresc <= r.teto;
  el.innerHTML =
    "No conjunto das ações com comparação disponível, o empenho acumulado cresceu <strong>" +
    pct(cresc) + "</strong> em termos reais contra o mesmo período do ano anterior. " +
    "A banda do regime fiscal vai de " + pct(r.piso) + " a " + pct(r.teto) +
    " ao ano, e a taxa de referência usada nas barras é " + pct(r.taxa) + ". " +
    (dentro
      ? "O agregado das ações comparáveis está dentro da banda."
      : "O agregado das ações comparáveis está fora da banda — o que não equivale ao " +
        "limite legal, que incide sobre a despesa primária de cada Poder e exclui " +
        "parte das despesas.");
}

function barras(sel, dados) {
  const alvo = $(sel);
  const campo = { loa: "d_loa", ano: "d_ano", rfs: "d_rfs" }[estado.metrica];
  const campoP = { loa: "p_loa", ano: "p_ano", rfs: "p_rfs" }[estado.metrica];

  const linhas = dados
    .filter((d) => d[campo] !== null && d[campo] !== undefined && Math.abs(d[campo]) > 0)
    .sort((a, b) => Math.abs(b[campo]) - Math.abs(a[campo]))
    .slice(0, 12);

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
      const fmtP = estado.metrica === "rfs" ? ppt : pct;
      const nome = (d.nome || d.cod || "").toString();
      return (
        '<div class="barra-linha">' +
          '<div class="barra-rot" title="' + nome.replace(/"/g, "") + '">' + nome + "</div>" +
          '<div class="barra-tri"><i class="eixo"></i>' + barra + "</div>" +
          '<div class="barra-val ' + classe(v) + '">' + rs(v) +
            '<br><small>' + fmtP(d[campoP]) + "</small></div>" +
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
    $("#f-ordem").value = { loa: "dloa", ano: "dano", rfs: "drfs" }[estado.metrica];
    desenharTabela();
  })
);

/* --------------------------------------------------------------- filtros */

function prepararFiltros() {
  const unicos = (campoCod, campoNome) => {
    const m = new Map();
    estado.acoes.forEach((a) => {
      if (a[campoCod] && !m.has(a[campoCod])) m.set(a[campoCod], a[campoNome] || a[campoCod]);
    });
    return Array.from(m.entries()).sort((x, y) => x[1].localeCompare(y[1], "pt-BR"));
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

  ["#busca", "#f-funcao", "#f-gnd", "#f-tipo", "#f-ordem"].forEach((s) =>
    $(s).addEventListener("input", () => {
      estado.mostrar = 200;
      desenharTabela();
    })
  );

  $("#limpar").addEventListener("click", () => {
    $("#busca").value = "";
    ["#f-funcao", "#f-gnd", "#f-tipo"].forEach((s) => ($(s).value = ""));
    estado.regra = null;
    $$(".faixa").forEach((b) => b.setAttribute("aria-pressed", "false"));
    estado.mostrar = 200;
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
  const ordem = $("#f-ordem").value;

  let r = estado.acoes.filter((a) => {
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

  r.sort((a, b) => Math.abs(b[ordem] || 0) - Math.abs(a[ordem] || 0));
  return r;
}

function desenharTabela() {
  const linhas = filtrar();
  const corpo = $("#tabela").querySelector("tbody");
  const fatia = linhas.slice(0, estado.mostrar);

  corpo.innerHTML = fatia
    .map((a) => {
      const sinais = (a.ops || "")
        .split(",")
        .filter(Boolean)
        .map((s) => '<span class="marca">' + (ROTULOS[s] || s) + "</span>")
        .join("");
      return (
        "<tr>" +
        "<td>" + (a.acn || "") + '<br><span class="cod">' + a.ac + "</span></td>" +
        "<td>" + (a.uon || "") + '<br><span class="cod">' + (a.orgn || "") + "</span></td>" +
        "<td>" + (a.fnn || "") + "</td>" +
        '<td class="n">' + rs(a.dot) + "</td>" +
        '<td class="n">' + rs(a.emp) + "</td>" +
        '<td class="n">' + rs(a.esp) + "</td>" +
        '<td class="n ' + classe(a.dloa) + '">' + rs(a.dloa) + "<br><small>" + pct(a.prit) + "</small></td>" +
        '<td class="n ' + classe(a.dano) + '">' + rs(a.dano) + "<br><small>" + pct(a.pano) + "</small></td>" +
        '<td class="n ' + classe(a.drfs) + '">' + rs(a.drfs) + "<br><small>" + ppt(a.prfs) + "</small></td>" +
        "<td>" + sinais + "</td>" +
        "</tr>"
      );
    })
    .join("");

  const total = linhas.length;
  $("#contagem").textContent =
    total.toLocaleString("pt-BR") + " ações no filtro atual · " +
    "dotação " + rs(linhas.reduce((s, a) => s + (a.dot || 0), 0)) + " · " +
    "empenhado " + rs(linhas.reduce((s, a) => s + (a.emp || 0), 0)) +
    (estado.regra ? " · filtro: " + ROTULOS[estado.regra] : "");
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
    "desvio_loa_rs_mil", "desvio_ano_rs_mil", "desvio_regime_fiscal_rs_mil",
    "var_dotacao", "exec_dotacao", "desvio_ritmo", "var_real", "var_vs_regime_fiscal",
    "z_robusto", "perfil_esperado", "origem_perfil", "montante_envolvido_rs_mil", "sinais"
  ];
  const esc = (v) => {
    const s = v === null || v === undefined ? "" : String(v);
    return /[";\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
  };
  const corpo = linhas.map((a) =>
    [a.org, a.orgn, a.uo, a.uon, a.fn, a.fnn, a.ac, a.acn, a.gnd, a.gndn, a.rp, a.disc,
     a.loa, a.dot, a.emp, a.liq, a.pag, a.esp, a.ant, a.dloa, a.dano,
     a.drfs, a.pdot, a.pexec, a.prit, a.pano, a.prfs, a.z, a.perf, a.oper, a.espaco, a.ops]
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
