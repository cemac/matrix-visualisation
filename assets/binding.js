var matVisBinding = new Shiny.OutputBinding();

let matvis_vars = $.getJSON( "matvis-1.0.0/matvis_vars.json");

let addLegend = function(node, name, cell_class, levels) {

    let title = document.createElement("h4");
    title.innerText = name;
    node.appendChild(title);

    // create legend
    let table = document.createElement("table");
    table.className = "matvis-table matvis-table-legend";

    for (let key of Object.keys(levels)) {
      let row = table.insertRow(-1);
      let cell = row.insertCell(-1);
      let class_names = [];
      class_names.push('matvis-td');
      class_names.push('matvis-legend-key');
      class_names.push(cell_class, cell_class + '-' + levels[key]);
      cell.className = class_names.join(' ');

      let label_cell = row.insertCell(-1);
      label_cell.className = 'matvis-legend-label';
      label_cell.innerText = key;
    }
    node.appendChild(table);
};

$.extend(matVisBinding, {
  find: function(scope) {
    return $(scope).find(".matvis");
  },
  renderValue: function(el, data) {

    // insert the title
    let title_id = el.id + '-matvis-title';
    document.getElementById(title_id).innerText = data.title;

    // create layout for co-benefits visualisation
    let table_id = el.id + '-matvis-table';

    let mydata = data.data;
    let nrow = mydata[Object.keys(mydata)[0]].length;
    if (nrow === 0) {
      document.getElementById(table_id).innerText = "No data to be displayed";
      return;
    }

    let mvars = matvis_vars.responseJSON;

    let table = document.createElement("table");
    table.className = "matvis-table";
    let groups = [];
    for (let i = 0; i < nrow + 1; i++) {
      let row = table.insertRow(-1);
      let level = mydata["Level"][row.rowIndex - 1];
      if (row.rowIndex !== 0) {
        let key = mydata[Object.keys(mydata)[0]][row.rowIndex - 1];
        row.className = 'matvis-table-group-' + mvars.transition[key];
        if (groups.indexOf(row.className) === -1) {
          groups.push(row.className);
        }
        row.className += ' matvis-table-level-' + level;
      }
      for (let col of Object.keys(mydata)) {
        if (col === 'Level') continue;
        let cell = row.insertCell(-1);
        let class_names = [];
        class_names.push(row.rowIndex === 0 ? 'matvis-th' : 'matvis-td');
        if (row.rowIndex === 0 &&
              (mvars.co_benefits.indexOf(col) !== -1 || col === "Context sensitivity")) {
          // Co-benefits header
          class_names.push('matvis-cb');
          class_names.push('matvis-cb-' + col.toLowerCase().replace(' ', '_'));
        }
        var entry = row.rowIndex === 0 ? col : mydata[col][row.rowIndex - 1];
        if (entry === null) {
          console.log(col);
        }
        if (typeof entry === 'string' || typeof entry === 'number') {
          const e = document.createElement("span");
          if (col === "Intervention" && row.rowIndex > 0) {
            class_names.push('matvis-intervention', 'matvis-level-' + level);
            let s = entry.split(";");
            let am = s[0].split("/").join('_');
            class_names.push('matvis-am-' + am.toLowerCase());
            entry = s[1];
          } else if (col === "Context sensitivity" && row.rowIndex !== 0) {
            class_names.push('matvis-cs', 'matvis-cs-' + entry);
          }
          e.innerText = entry;
          cell.appendChild(e);
        } else {
          // Use the object to construct elements for display
          if (col === "Context sensitivity" && row.rowIndex !== 0) {
            class_names.push('matvis-cs');
          } else {
            class_names.push('matvis-tl');
          }
          if (entry !== null) {
            // Traffic light level
            let tll_label = "Traffic light level";
            if (Object.keys(entry).indexOf(tll_label) == -1) {
              tll_label = "Traffic light co-impact";
            }
            let tll = entry[tll_label][0];
            if (tll !== null) {
              class_names.push('matvis-tll-' + mvars.traffic_light_co_impact[tll]);
            }

            // Traffic light confidence
            let tlc = entry["Traffic light confidence"][0];
            if (tlc !== null) {
              class_names.push('matvis-tlc-'+ mvars.traffic_light_confidence[tlc]);
            }

            // Context sensitivity
            let cs_label = "Context sensitivity";
            if (Object.keys(entry).indexOf(cs_label) > -1) {
              let cs = entry[cs_label][0];
              if (cs !== null) {
                class_names.push('matvis-cs matvis-cs-' + mvars.context_sensitivity[cs]);
              }
            }

            // Info to display on hover
            let vars = [
              "Traffic light co-impact",
              "Traffic light confidence",
              "Confidence justification",
              "Co-impact narrative",
              "Context sensitivity"
            ];
            let info = document.createElement("div");
            info.className = 'matvis-info';
            for (let i = 0; i < vars.length; i++) {
              if (Object.keys(entry).indexOf(vars[i]) != -1 && entry[vars[i]][0] !== null) {
                let span = document.createElement("span");
                span.className = 'matvis-info-' + i;
                span.innerText = vars[i] + ': ' + entry[vars[i]][0];
                info.appendChild(span);
              }
            }
            cell.appendChild(info);
          }
        }
        cell.className = class_names.join(' ');
      }
    }
    document.getElementById(table_id).replaceChildren(table);

    // Merge transition cells
    const parent = document.getElementById(table_id);
    for (let group of groups) {
      let group_rows = parent.getElementsByClassName(group);
      if (group_rows.length > 1) {
        // merge cells across rows in group
        for (let i = 0; i < group_rows.length; i++) {
          first_cell = group_rows.item(i).childNodes[0];
          if (i === 0) {
            first_cell.setAttribute("rowspan", group_rows.length);
          } else {
            first_cell.remove();
          }
        }
      }
    }

    // insert the legend
    let legend_id = el.id + '-matvis-legend';
    let legend = document.getElementById(legend_id);

    addLegend(legend, "Potential for trade-offs or co-benefits", "matvis-tll", mvars.traffic_light_co_impact);
    addLegend(legend, "Confidence", "matvis-tlc", mvars.traffic_light_confidence);

    // Select context sensitivity legend based on tab panel title
    let active_panel = document.getElementsByClassName("tab-pane active")[0];
    let panel_title = active_panel.getAttribute("data-value");
    addLegend(legend, "Context sensitivity", "matvis-cs", mvars.context_sensitivity);
  }
});

// register
Shiny.outputBindings.register(matVisBinding, "lbf2.matVis");