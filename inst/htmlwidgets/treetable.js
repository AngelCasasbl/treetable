/*
 * treetable: expandable/collapsible tree table with reactable-style
 * columns, driven entirely from R via
 * tree_table()/col_spec()/col_field()/treetable_theme()/column_format().
 *
 * payload (x) = {
 *   column_groups: [ { name, span }, ... ],  // one per col_spec(); span =
 *     number of col_field()s in that group (how many cells it covers)
 *   subcolumns: [ { label, format }, ... ],  // FLAT, one per col_field()
 *     across every group, in order; format = { type, decimals, symbol,
 *     thousands_sep }
 *   tree: [ { label, series: [value, ...], renders: [render?, ...]?,
 *             children?, id? }, ... ],
 *     // series/renders are parallel to `subcolumns` (flat, same length);
 *     // renders[i], if present and non-null, is either { html: "<...>" }
 *     // (raw HTML, from an htmltools tag/HTML() a col_field()'s `cell`
 *     // returned) or { text: "..." } (plain, escaped text overriding the
 *     // formatted value) - otherwise series[i] is rendered via
 *     // subcolumns[i].format
 *   theme: { header_bg, header_text, subtotal_bg, subtotal_text,
 *            leaf_hover_bg, font_family, font_size,
 *            label_column_width, value_column_width },
 *   detail_enabled: true/false,
 *   lang: "en" | "es" | null  // null/unset auto-detects from the browser
 * }
 * A group with a single field renders as one plain column under its name.
 * A group with several fields renders its name spanning a sub-header row
 * with one cell per field (that field's label) - there is no automatic
 * comparison/percentage between them.
 */
(function () {
  "use strict";

  var STYLE_ID = "treetable-style";

  var I18N = {
    en: {
      expand_all: "Expand all",
      collapse_all: "Collapse all",
      detail: "View detail",
      empty: "No data to display.",
    },
    es: {
      expand_all: "Expandir todo",
      collapse_all: "Colapsar todo",
      detail: "Ver detalle",
      empty: "No hay datos para mostrar.",
    },
  };

  // Explicit `lang` ("en"/"es") wins; otherwise auto-detect from the
  // browser, defaulting to English for anything that isn't Spanish.
  function resolveStrings(lang) {
    if (lang === "es" || lang === "en") return I18N[lang];
    var nav = ((navigator.language || navigator.userLanguage || "") + "").toLowerCase();
    return nav.indexOf("es") === 0 ? I18N.es : I18N.en;
  }

  function ensureStyle() {
    if (document.getElementById(STYLE_ID)) return;
    var css =
      ".tt-wrap{overflow-x:auto;overflow-y:auto;height:100%;box-sizing:border-box;" +
      "border:1px solid #d4d4d7;border-radius:4px;background:#fff;" +
      "font-family:var(--tt-font-family,'Inter','Segoe UI',sans-serif);" +
      "font-size:var(--tt-font-size,13px);color:#1d1f20}" +
      ".tt-table{border-collapse:collapse;width:max-content;min-width:100%}" +
      ".tt-table th,.tt-table td{white-space:nowrap;padding:0 14px;border-bottom:1px solid #e7e7ea;box-sizing:border-box}" +
      ".tt-head-col{position:sticky;top:0;z-index:3;background:var(--tt-header-bg,#1d2d3d);" +
      "color:var(--tt-header-text,#fff);text-align:center;font-weight:600;letter-spacing:.04em;" +
      "height:38px;border-left:1px solid rgba(255,255,255,.15)}" +
      ".tt-head-sub{position:sticky;top:38px;z-index:2;background:var(--tt-subtotal-bg,#eef6ff);" +
      "font-size:11px;letter-spacing:.04em;color:#416180;text-align:right;height:30px;" +
      "border-left:1px solid #d4d4d7}" +
      ".tt-label-head{position:sticky;left:0;top:0;z-index:4;background:#f2f2f3;" +
      "min-width:var(--tt-label-width,280px);text-align:left}" +
      ".tt-row-level0{background:var(--tt-header-bg,#1d2d3d);color:var(--tt-header-text,#fff);" +
      "font-weight:700;height:36px}" +
      ".tt-row-level0 .tt-label{position:sticky;left:0;background:var(--tt-header-bg,#1d2d3d);" +
      "color:var(--tt-header-text,#fff)}" +
      ".tt-row-subtotal{background:var(--tt-subtotal-bg,#eef6ff);color:var(--tt-subtotal-text,#1d1f20);" +
      "font-weight:600;height:32px}" +
      ".tt-row-subtotal .tt-label{position:sticky;left:0;background:var(--tt-subtotal-bg,#eef6ff);" +
      "color:var(--tt-subtotal-text,#1d1f20)}" +
      ".tt-row-leaf{height:30px}" +
      ".tt-row-leaf:hover{background:var(--tt-leaf-hover-bg,#f7fafc)}" +
      ".tt-row-leaf .tt-label{position:sticky;left:0;background:#fff}" +
      ".tt-row-leaf:hover .tt-label{background:var(--tt-leaf-hover-bg,#f7fafc)}" +
      ".tt-label{text-align:left;min-width:var(--tt-label-width,280px)}" +
      ".tt-toggle{cursor:pointer;display:inline-block;width:14px;transition:transform .12s ease}" +
      ".tt-toggle.tt-open{transform:rotate(90deg)}" +
      ".tt-num{text-align:right;min-width:var(--tt-value-width,88px);font-variant-numeric:tabular-nums}" +
      ".tt-detail-btn{border:none;background:transparent;cursor:pointer;padding:2px 4px;color:#597ea3;font-size:13px}" +
      ".tt-detail-btn:hover{color:#1d2d3d}" +
      ".tt-empty{padding:24px;color:#7a7a7d;text-align:center}" +
      ".tt-corner-actions{display:flex;gap:6px;align-items:center;justify-content:flex-start;height:100%;" +
      "font-size:11px;font-weight:600;letter-spacing:normal;text-transform:none}" +
      ".tt-corner-btn{cursor:pointer;border:1px solid #cdd6df;background:#fff;color:#416180;" +
      "padding:4px 9px;border-radius:999px;display:inline-flex;align-items:center;gap:4px;" +
      "line-height:1;transition:background .12s ease,border-color .12s ease,color .12s ease}" +
      ".tt-corner-btn:hover{background:var(--tt-subtotal-bg,#eef6ff);border-color:#597ea3;color:#1d2d3d}" +
      ".tt-corner-btn:active{background:#d6ebff}";
    var el = document.createElement("style");
    el.id = STYLE_ID;
    el.textContent = css;
    document.head.appendChild(el);
  }

  function escapeHtml(s) {
    return String(s === null || s === undefined ? "" : s).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
    });
  }

  function formatValue(value, format) {
    format = format || { type: "number", decimals: 0, thousands_sep: true };
    if (value === null || value === undefined || isNaN(value)) return "--";
    if (format.type === "text") return String(value);

    var decimals = format.decimals || 0;
    var num = Number(value);
    var out;
    if (format.type === "percentage") {
      out = num.toFixed(decimals) + "%";
    } else if (format.thousands_sep === false) {
      out = num.toFixed(decimals);
    } else {
      out = num.toLocaleString(undefined, {
        minimumFractionDigits: decimals,
        maximumFractionDigits: decimals,
      });
    }
    if (format.symbol && format.type !== "percentage") out = format.symbol + out;
    return out;
  }

  // `render` is a col_field()'s optional per-cell override for this node
  // (from R's render_cell()): { html: "<...>" } for raw HTML (already
  // trusted - it came from an htmltools tag/HTML() built in R), { text:
  // "..." } for plain text (still escaped here), or null/undefined to fall
  // back to the default, format-based rendering of `value`.
  function renderCell(value, render, format) {
    if (render && typeof render.html === "string") return render.html;
    if (render && typeof render.text === "string") return escapeHtml(render.text);
    return formatValue(value, format);
  }

  function applyThemeVars(el, theme) {
    theme = theme || {};
    var map = {
      header_bg: "--tt-header-bg",
      header_text: "--tt-header-text",
      subtotal_bg: "--tt-subtotal-bg",
      subtotal_text: "--tt-subtotal-text",
      leaf_hover_bg: "--tt-leaf-hover-bg",
      font_family: "--tt-font-family",
      font_size: "--tt-font-size",
      label_column_width: "--tt-label-width",
      value_column_width: "--tt-value-width",
    };
    Object.keys(map).forEach(function (key) {
      if (theme[key]) el.style.setProperty(map[key], theme[key]);
    });
  }

  function flatten(nodes, depth, collapsed, path) {
    var out = [];
    (nodes || []).forEach(function (node, i) {
      var key = path + "/" + i;
      var hasChildren = !!(node.children && node.children.length);
      out.push({ node: node, depth: depth, hasChildren: hasChildren, key: key });
      if (hasChildren && !collapsed[key]) {
        out = out.concat(flatten(node.children, depth + 1, collapsed, key));
      }
    });
    return out;
  }

  // Initial collapse state: open through the 2nd level, closed below that
  // (expandable on demand).
  function defaultCollapsed(nodes, depth, path, collapsed) {
    (nodes || []).forEach(function (node, i) {
      var key = path + "/" + i;
      var hasChildren = !!(node.children && node.children.length);
      if (hasChildren) {
        if (depth >= 1) collapsed[key] = true;
        defaultCollapsed(node.children, depth + 1, key, collapsed);
      }
    });
  }

  function setAllCollapsed(nodes, path, collapsed, value) {
    (nodes || []).forEach(function (node, i) {
      var key = path + "/" + i;
      var hasChildren = !!(node.children && node.children.length);
      if (hasChildren) {
        if (value) {
          collapsed[key] = true;
        } else {
          delete collapsed[key];
        }
        setAllCollapsed(node.children, key, collapsed, value);
      }
    });
  }

  // Whether any node in the tree is currently collapsed, used to decide
  // what the single expand/collapse toggle button should do (and say) next.
  function anyCollapsed(nodes, path, collapsed) {
    return (nodes || []).some(function (node, i) {
      var key = path + "/" + i;
      var hasChildren = !!(node.children && node.children.length);
      if (!hasChildren) return false;
      return collapsed[key] || anyCollapsed(node.children, key, collapsed);
    });
  }

  // render() redraws the whole widget on every toggle; collapse state lives
  // on the DOM element itself so it survives re-renders triggered by Shiny
  // (new data resets the state).
  function render(el, x, onDetail) {
    ensureStyle();
    x = x || {};
    applyThemeVars(el, x.theme);

    var collapsed = el.__ttCollapsed;
    if (!collapsed) {
      collapsed = {};
      defaultCollapsed(x.tree || [], 0, "r", collapsed);
    }
    el.__ttCollapsed = collapsed;

    var t = resolveStrings(x.lang);
    var columnGroups = x.column_groups || [];
    var subcolumns = x.subcolumns || [];
    var tree = x.tree || [];
    var detailEnabled = !!x.detail_enabled;

    if (!tree.length) {
      el.innerHTML = '<div class="tt-empty">' + escapeHtml(t.empty) + "</div>";
      return;
    }

    var rows = flatten(tree, 0, collapsed, "r");
    // A group's own sub-header row only earns its keep when at least one
    // group has more than one field to tell apart under its header (e.g.
    // "Budget"/"Actual"); with every group a single field, it would just
    // repeat the group name for no reason, so that row is skipped entirely.
    var anyMultiField = columnGroups.some(function (g) {
      return g.span > 1;
    });
    var headerRows = anyMultiField ? 2 : 1;
    // Single toggle button: its label always names the action a click will
    // perform next (expand everything while something is collapsed,
    // otherwise collapse everything).
    var somethingCollapsed = anyCollapsed(tree, "r", collapsed);
    var toggleLabel = somethingCollapsed ? t.expand_all : t.collapse_all;
    var toggleIcon = somethingCollapsed ? "&#8645;" : "&#8646;";
    var html = [
      '<div class="tt-wrap">',
      '<table class="tt-table"><thead>',
      '<tr><th class="tt-label-head" rowspan="' + headerRows + '"><div class="tt-corner-actions">' +
        '<button type="button" class="tt-corner-btn" data-toggle-all>' +
        toggleIcon +
        " " +
        escapeHtml(toggleLabel) +
        "</button>" +
        "</div></th>",
    ];
    columnGroups.forEach(function (g) {
      // A single-field group has no sub-header cell reserved for it, so its
      // own header spans both rows when the table has two; a multi-field
      // group's header stays on row 1, leaving room for its field labels
      // on row 2.
      var rowspan = anyMultiField && g.span === 1 ? headerRows : 1;
      html.push(
        '<th class="tt-head-col" colspan="' + g.span + '" rowspan="' + rowspan + '">' +
          escapeHtml(g.name) +
          "</th>"
      );
    });
    html.push("</tr>");
    if (anyMultiField) {
      html.push("<tr>");
      var idx = 0;
      columnGroups.forEach(function (g) {
        if (g.span > 1) {
          for (var k = 0; k < g.span; k++) {
            var sub = subcolumns[idx] || {};
            html.push('<th class="tt-head-sub">' + escapeHtml(String(sub.label || "").toUpperCase()) + "</th>");
            idx++;
          }
        } else {
          idx += g.span;
        }
      });
      html.push("</tr>");
    }
    html.push("</thead><tbody>");

    rows.forEach(function (r) {
      var node = r.node;
      var rowClass = r.depth === 0 ? "tt-row-level0" : r.hasChildren ? "tt-row-subtotal" : "tt-row-leaf";
      html.push('<tr class="' + rowClass + '">');
      html.push('<td class="tt-label" style="padding-left:' + (14 + r.depth * 18) + 'px">');
      if (r.hasChildren) {
        var open = !collapsed[r.key];
        html.push(
          '<span class="tt-toggle' + (open ? " tt-open" : "") + '" data-toggle="' + r.key + '">&#9656;</span> '
        );
      } else if (detailEnabled && node.id) {
        html.push(
          '<button type="button" class="tt-detail-btn" data-detail="' +
            escapeHtml(node.id) +
            '" title="' +
            escapeHtml(t.detail) +
            '">&#128269;</button> '
        );
      }
      html.push(escapeHtml(node.label) + "</td>");
      (node.series || []).forEach(function (value, i) {
        var fmt = (subcolumns[i] && subcolumns[i].format) || { type: "number", decimals: 0, thousands_sep: true };
        var renderOverride = node.renders ? node.renders[i] : null;
        html.push('<td class="tt-num">' + renderCell(value, renderOverride, fmt) + "</td>");
      });
      html.push("</tr>");
    });
    html.push("</tbody></table></div>");
    el.innerHTML = html.join("");

    Array.prototype.forEach.call(el.querySelectorAll("[data-toggle]"), function (toggleEl) {
      toggleEl.addEventListener("click", function () {
        var key = toggleEl.getAttribute("data-toggle");
        collapsed[key] = !collapsed[key];
        render(el, x, onDetail);
      });
    });
    Array.prototype.forEach.call(el.querySelectorAll("[data-detail]"), function (b) {
      b.addEventListener("click", function () {
        if (onDetail) onDetail(b.getAttribute("data-detail"));
      });
    });

    var toggleAllBtn = el.querySelector("[data-toggle-all]");
    if (toggleAllBtn) {
      toggleAllBtn.addEventListener("click", function () {
        // Mirrors the label: expand everything if anything is collapsed,
        // otherwise collapse everything.
        setAllCollapsed(tree, "r", collapsed, !somethingCollapsed);
        render(el, x, onDetail);
      });
    }
  }

  HTMLWidgets.widget({
    name: "treetable",
    type: "output",

    factory: function (el, width, height) {
      return {
        renderValue: function (x) {
          render(el, x, function (id) {
            if (window.HTMLWidgets && HTMLWidgets.shinyMode) {
              Shiny.setInputValue(el.id + "_detail", { id: id, nonce: Math.random() }, { priority: "event" });
            }
          });
        },

        resize: function () {},
      };
    },
  });
})();
