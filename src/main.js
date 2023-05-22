import * as fjs from './function.js'
import * as gramjs from './gruposram.js'
//#region Get the problem name element.
var problem_name = document.getElementById("problem_name");
//#endregion

//#region Get the os element selection box.
var os_td = document.getElementById("os_td");
var os_box = document.createElement("select");
    os_box.className = "os_box";
var os_option = document.createElement("option");
    os_option.value = "linux_ubuntu";
    os_option.text = "Linux_Ubuntu";

os_box.appendChild(os_option);
var os_option = document.createElement("option");
    os_option.value = "windows";
    os_option.text = "Windows";

os_box.appendChild(os_option);
os_td.appendChild(os_box);

//#endregion

//#region Get the model element selection box.
var model_td = document.getElementById("model_td");
var model_box = document.createElement("select");
    model_box.className = "model_box";
var model_option = document.createElement("option");
    model_option.value = "0";
    model_option.text = "UNIFAC";
    model_box.appendChild(model_option);
var model_option = document.createElement("option");
    model_option.value = "1";
    model_option.text = "A-UNIFAC";

model_box.appendChild(model_option);
model_td.appendChild(model_box);
//#endregion

//#region Get the "partable" element selection box
var partable_td = document.getElementById("partable_td");
var partable_box = document.createElement("select");
    partable_box.className = "partable_box";
var partable_option = document.createElement("option");
    partable_option.value = "2";
    partable_option.text = "Vapor-Liquid";
    partable_box.appendChild(partable_option);
var partable_option = document.createElement("option");
    partable_option.value = "1";
    partable_option.text = "Liquid-Liquid";
    partable_box.appendChild(partable_option);
var partable_option = document.createElement("option");
    partable_option.value = "3";
    partable_option.text = "Infinity dil.";

partable_box.appendChild(partable_option);
partable_td.appendChild(partable_box);
//#endregion

//#region Calculation config
/*
    '!   icalc:  0-' **** FLASH CALCULATION ****'
    '!           1-' **** BINODAL CURVE CALCULATION ****'
    '!           2-' **** CALCULATION OF UNIQUAC PARAMETERS FROM UNIFAC **** '
    '!   model:  0-' MODEL USED FOR LIQUID PHASE NON-IDEALITY: UNIFAC'
    '!           1-' MODEL USED FOR LIQUID PHASE NON-IDEALITY: UNIQUAC'
    '!   ipr:    1-' ** COMPARISON OF ACTIVITIES CALCULATED BY UNIFAC AND 
                    UNIQUAC, RESPECTIVELY **'
    '!   iout:   1-'open 'lleasoccuzada.OUT''
    '!   novap:  0-'VAPOR PHASE INCLUDED IN FLASH-CALCULATIONS'
    '!   ig:     0-'write compositions'
    '!           1-'write compositions and activities'
    '!   ipareq: 1-'liquid-liquid parameters table (UNIFAC)'
    '!           2-'liquid-vapor parameters table (UNIFAC)'
    '!           3-'infinite dilution parameters table (UNIFAC)'
    '!           4-'GC-EOS parameters'
*/
var icalc = 0
var ipr   = 0
var iout  = 1
var novap = 0
var ig    = 1
//#endregion

//#region Create a button to print the data input
var saveconfig_div = document.getElementById("saveconfig_div");
var saveconfig_button = document.createElement("input");
    saveconfig_button.id = "saveconfig_button";
    saveconfig_button.type = "button";
    saveconfig_button.value = "Save";
    saveconfig_button.onclick = () => {
        fjs.compremover();
        fjs.savealert(
            os_box,model_box,partable_box,problem_name);
        
    }
    saveconfig_div.appendChild(saveconfig_button);
    
//#endregion

//#region Create Download Button
//function makedownload(){
    var div_download = document.getElementById("div_download");
    var downloadButton = document.createElement("input");
        downloadButton.id = "downloadButton";
        downloadButton.type = "button";
        downloadButton.value = "Download";
    
    // Add an onclick event listener to downloadButton
    downloadButton.onclick = () => {download()};
    div_download.appendChild(downloadButton);
//}
//#endregion

//#region Download Subroutine
function download(){
    var values = [];
    
    var model_sel = model_box.options[model_box.selectedIndex].value;
    var partable_sel = partable_box.options[partable_box.selectedIndex].value;

    values.push(`"${problem_name.value}"`); //Print name of file
    values.push(
        `${icalc},${model_sel},${ipr},${iout},${novap},${ig},${partable_sel}`);  
    
    values.push(`${ncomp_input.value}`);
    var lines = document.getElementsByClassName("line");
    for (var i = 0; i < (lines.length-1); (i++)) {
        var lineValues = [];
        var boxes = lines[i].getElementsByClassName("box");
        var inputs = lines[i+1].getElementsByClassName("input");
        for (var j = 0; j < boxes.length; j++) {
            lineValues.push(boxes[j].value);
            lineValues.push(inputs[j].value);
        }
        if (i % 2 == 0)
        values.push(lineValues.join(","));
    }
    
    var z_values = [];
    var TP_values = [];
    var z_inputs = document.getElementsByClassName("z_input");
    var TP_inputs = document.getElementsByClassName("tp_input");
    var jj = 0
    var kk = 0
    for (var j = 0; j < (nflash_input.value); (j++)) {
        z_values=[]
        TP_values=[]
        for (var k = 0; k < ncomp_input.value; k++){
            if (z_inputs[jj].value == 0){
                z_values.push(".00000001")
            } else if ((z_inputs[jj].value).includes(".")){
                z_values.push(z_inputs[jj].value);
            } else z_values.push(z_inputs[jj].value+".");
            jj++;
        }
        if ((TP_inputs[kk].value).includes(".")){
                TP_values.push(TP_inputs[kk].value);
            } else TP_values.push(TP_inputs[kk].value+".");
        kk++;
        if ((TP_inputs[kk].value).includes(".")){
                TP_values.push(TP_inputs[kk].value);
            } else TP_values.push(TP_inputs[kk].value+".");
        kk++;
        values.push(TP_values.join(","));
        values.push(z_values.join(","));
    };

    values.push("0,0") //Print end of file

    // Create a text file from the values
    var textFileAsBlob = new Blob([values.join("\n")], {type: 'text/plain'});
  
    // Create a link element to download the file
    var downloadLink = document.createElement("a");
    downloadLink.download = problem_name.value+".dat";
  
    // Use createObjectURL to create a URL for the file
    downloadLink.href = window.URL.createObjectURL(textFileAsBlob);
  
    // Click the link to download the file
    downloadLink.click();
}
//#endregion