/*
 * treetable: expandable/collapsible tree table (grouped columns with one or
 * two metrics and automatic percent variance when there are two), driven
 * entirely from R via tree_table()/treetable_theme()/column_format().
 *
 * payload (x) = {
 *   columns: ["Q1", "Q2", ...],
 *   metrics: ["Budget", "Actual"],  // 1 or 2 elements
 *   tree: [ { label, series: [{a, b?}, ...], children?, id? }, ... ],
 *   theme: { header_bg, header_text, subtotal_bg, subtotal_text,
 *            leaf_hover_bg, positive_var_color, negative_var_color,
 *            neutral_var_color, font_family, font_size,
 *            label_column_width, value_column_width },
 *   format_a: { type, decimals, symbol, thousands_sep },
 *   format_b: same shape as format_a, or null,
 *   detail_enabled: true/false
 * }
 * If `metrics` has a single element, each column shows one number
 * (series.a) and no percent variance is computed.
 */
(function () {
  "use strict";

  var STYLE_ID = "treetable-style";

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
      ".tt-var{font-weight:600}" +
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

  function varColor(pct, theme) {
    theme = theme || {};
    if (pct === null || pct === undefined || isNaN(pct)) return theme.neutral_var_color || "#7a7a7d";
    if (pct > 5) return theme.positive_var_color || "#3f7a52";
    if (pct < -5) return theme.negative_var_color || "#b1483f";
    return theme.neutral_var_color || "#7a7a7d";
  }

  function applyThemeVars(el, theme) {
    theme = theme || {};
    var map = {
      header_bg: "--tt-header-bg",
      header_text: "--tt-header-text",
      subtotal_bg: "--tt-subtotal-bg",
      subtotal_text: "--tt-subtotal-text",
      leaf_hover_bg: "--tt-leaf-hover-bg",
      positive_var_color: "--tt-positive-var",
      negative_var_color: "--tt-negative-var",
      neutral_var_color: "--tt-neutral-var",
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

    var columns = x.columns || [];
    var metrics = x.metrics || ["Value"];
    var twoMetrics = metrics.length >= 2;
    var tree = x.tree || [];
    var formatA = x.format_a || { type: "number", decimals: 0, thousands_sep: true };
    var formatB = x.format_b || formatA;
    var detailEnabled = !!x.detail_enabled;

    if (!tree.length) {
      el.innerHTML = '<div class="tt-empty">No data to display.</div>';
      return;
    }

    var rows = flatten(tree, 0, collapsed, "r");
    var colspanGroup = twoMetrics ? 3 : 1;
    var html = [
      '<div class="tt-wrap">',
      '<table class="tt-table"><thead>',
      '<tr><th class="tt-label-head" rowspan="2"><div class="tt-corner-actions">' +
        '<button type="button" class="tt-corner-btn" data-expand-all>&#8645; Expand all</button>' +
        '<button type="button" class="tt-corner-btn" data-collapse-all>&#8646; Collapse all</button>' +
        "</div></th>",
    ];
    columns.forEach(function (c) {
      html.push('<th class="tt-head-col" colspan="' + colspanGroup + '">' + escapeHtml(c) + "</th>");
    });
    html.push("</tr><tr>");
    columns.forEach(function () {
      if (twoMetrics) {
        html.push(
          '<th class="tt-head-sub">' +
            escapeHtml(String(metrics[0]).toUpperCase()) +
            '</th><th class="tt-head-sub">' +
            escapeHtml(String(metrics[1]).toUpperCase()) +
            '</th><th class="tt-head-sub">VAR %</th>'
        );
      } else {
        html.push('<th class="tt-head-sub">' + escapeHtml(String(metrics[0]).toUpperCase()) + "</th>");
      }
    });
    html.push("</tr></thead><tbody>");

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
            '" title="View detail">&#128269;</button> '
        );
      }
      html.push(escapeHtml(node.label) + "</td>");
      (node.series || []).forEach(function (s) {
        var a = s ? s.a : null;
        html.push('<td class="tt-num">' + formatValue(a, formatA) + "</td>");
        if (twoMetrics) {
          var b = s ? s.b : null;
          var pct = !a ? null : ((b - a) / Math.abs(a)) * 100;
          html.push('<td class="tt-num">' + formatValue(b, formatB) + "</td>");
          html.push(
            '<td class="tt-num tt-var" style="color:' +
              varColor(pct, x.theme) +
              '">' +
              (pct === null || isNaN(pct) ? "--" : pct.toFixed(2) + "%") +
              "</td>"
          );
        }
      });
      html.push("</tr>");
    });
    html.push("</tbody></table></div>");
    el.innerHTML = html.join("");

    Array.prototype.forEach.call(el.querySelectorAll("[data-toggle]"), function (t) {
      t.addEventListener("click", function () {
        var key = t.getAttribute("data-toggle");
        collapsed[key] = !collapsed[key];
        render(el, x, onDetail);
      });
    });
    Array.prototype.forEach.call(el.querySelectorAll("[data-detail]"), function (b) {
      b.addEventListener("click", function () {
        if (onDetail) onDetail(b.getAttribute("data-detail"));
      });
    });

    var expandAllBtn = el.querySelector("[data-expand-all]");
    if (expandAllBtn) {
      expandAllBtn.addEventListener("click", function () {
        setAllCollapsed(tree, "r", collapsed, false);
        render(el, x, onDetail);
      });
    }
    var collapseAllBtn = el.querySelector("[data-collapse-all]");
    if (collapseAllBtn) {
      collapseAllBtn.addEventListener("click", function () {
        setAllCollapsed(tree, "r", collapsed, true);
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
