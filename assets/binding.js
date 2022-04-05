var matVisBinding = new Shiny.OutputBinding();

$.extend(matVisBinding, {
  find: function(scope) {
    return $(scope).find(".matvis");
  },
  renderValue: function(el, data) {

    // insert the title
    let title_id = el.id + '-matvis-title';
    document.getElementById(title_id).innerText = data.title;

    let mydata = data.data;

    let value_id = el.id + '-matvis-value';
    document.getElementById(value_id).innerText = mydata.data.length;
    
    let table_id = el.id + '-matvis-table';
    console.log(table_id);
    let table = document.getElementById(table_id);
    console.log(table);
    for (let step = 0; step < mydata.data.length; step++) {
      var row = table.insertRow(-1);
      for (let col = 0; col < Object.keys(mydata).length; col++) {
        cell = row.insertCell(-1);
        //text = 'Row ' + row.rowIndex + ' Cell ' + col;
        text = mydata[Object.keys(mydata)[col]][row.rowIndex];
        cell.appendChild(document.createTextNode(text));
      }
    }
    
    //for (var x in Object.keys(data.data)) {
      //document.getElementById(level_id).innerText += x + ":" + data.data[x];
    //}
    //console.log(data.data)

    // background color 
    //el.style.backgroundColor = data.color;
  }
});

// register
Shiny.outputBindings.register(matVisBinding, "lbf2.matVis");