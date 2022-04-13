var matVisBinding = new Shiny.OutputBinding();

let matvis_vars = $.getJSON( "matvis-1.0.0/matvis_vars.json");

$.extend(matVisBinding, {
  find: function(scope) {
    return $(scope).find(".matvis");
  },
  renderValue: function(el, data) {

    // insert the title
    let title_id = el.id + '-matvis-title';
    document.getElementById(title_id).innerText = data.title;

    let mydata = data.data;
    let nrow = mydata[Object.keys(mydata)[0]].length;

    let mvars = matvis_vars.responseJSON;

    let table_id = el.id + '-matvis-table';
    let table = document.createElement("table");
    table.className = "matvis-table";
    let groups = [];
    for (let i = 0; i < nrow + 1; i++) {
      let row = table.insertRow(-1);
      if (row.rowIndex !== 0) {
        let key = mydata[Object.keys(mydata)[0]][row.rowIndex - 1];
        row.className = 'matvis-table-group-' + mvars.transition[key];
        if (groups.indexOf(row.className) === -1) {
          groups.push(row.className);
        }
      }
      for (let col of Object.keys(mydata)) {
        let cell = row.insertCell(-1);
        let class_names = [];
        class_names.push(row.rowIndex === 0 ? 'matvis-th' : 'matvis-td');
        if (row.rowIndex === 0 && mvars.co_benefits.indexOf(col) !== -1) {
          class_names.push('matvis-cb');
        }
        var entry = row.rowIndex === 0 ? col : mydata[col][row.rowIndex - 1];
        if (typeof entry === 'string') {
          const e = document.createElement("span");
          if (col === "Intervention" && row.rowIndex > 0) {
            let s = entry.split(";")
            class_names.push('matvis-am-' + s[0].toLowerCase());
            entry = s[1];
          }
          e.innerText = entry;
          cell.appendChild(e);
        } else {
          // Use the object to construct elements for display
          class_names.push('matvis-tl');

          // Traffic light level
          let tl = entry["Traffic light level"][0];
          class_names.push('matvis-tll-' + mvars.traffic_light_level[tl]);

          // Traffic light confidence
          let tlc = entry["Traffic light confidence"][0];
          class_names.push('matvis-tlc-'+ mvars.traffic_light_confidence[tlc]);

          // Info to display on hover
          let vars = [
            "Traffic light level",
            "Traffic light confidence",
            "Confidence justification/ comments",
            "Co-benefit narrative (broad findings)"
          ];
          let info = document.createElement("div");
          info.className = 'matvis-info';
          for (let i = 0; i < vars.length; i++) {
            let span = document.createElement("span");
            span.className = 'matvis-info-' + i;
            span.innerText = vars[i] + ': ' + entry[vars[i]][0];
            info.appendChild(span);
          }
          cell.appendChild(info);
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
  }
});

// register
Shiny.outputBindings.register(matVisBinding, "lbf2.matVis");