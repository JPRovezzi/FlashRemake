import * as fjs from './function.js'
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
        fjs.savealert(os_box,model_box,partable_box,problem_name)
    }
    saveconfig_div.appendChild(saveconfig_button);
    
//#endregion

//#region Set the Group list from the "partable" information.

var grouplist1 = ["None",
    "(CH3)",
    "(CH2)",
    "(CH)",
    "(C)",
    "(CH2=CH)",
    "(CH=CH)",
    "(CH=C)",
    "(CH2=C)",
    "(ACH)",
    "(AC)",
    "(ACCH3)",
    "(ACCH2)",
    "(ACCH)",
    "(OH)",
    "(P1)",
    "(P2)",
    "(H2O)",
    "(ACOH)",
    "(CH3CO)",
    "(CH2CO)",
    "(CHO)",
    "(FURF)",
    "(COOH)",
    "(HCOOH)",
    "(CH3COO)",
    "(CH2COO)",
    "(CH3O)",
    "(CH2O)",
    "(CH-O)",
    "(FCH2O)",
    "(CH2CL)",
    "(CHCL)",
    "(CCL)",
    "(CH2CL2)",
    "(CHCL2)",
    "(CCL2)",
    "(CHCL3)",
    "(CCL3)",
    "(CCL4)",
    "(ACCL)",
    "(CH3CN)",
    "(CH2CN)",
    "(ACNH2)",
    "(CH3NO2)",
    "(CH2NO2)",
    "(CHNO2)",
    "(ACNO2)",
    "(CH2OH)2",
    "(DEOH)",
    "(C5H5N)",
    "(C5H4N)",
    "(C5H3N)",
    "(TCLE)",
    "(MFA)",
    "(DMFA)",
    "(TMS)",
    "(ME2SO)"
];
var grouplist2 = ["None",
        "(CH3)",
        "(CH2)",
        "(CH)",
        "(C)",
        "(CH2=CH)",
        "(CH=CH)",
        "(CH2=C)",
        "(CH=C)",
        "(C=C)",
        "(ACH)",
        "(AC)",
        "(ACCH3)",
        "(ACCH2)",
        "(ACCH)",
        "(OH)",
        "(CH3OH)",
        "(H2O)",
        "(ACOH)",
        "(CH3CO)",
        "(CH2CO)",
        "(CHO)",
        "(CH3COO)",
        "(CH2COO)",
        "(HCOO)",
        "(CH3O)",
        "(CH2O)",
        "(CH-O)",
        "(FCH2O)",
        "(CH3NH2)",
        "(CH2NH2)",
        "(CHNH2)",
        "(CH3NH)",
        "(CH2NH)",
        "(CHNH)",
        "(CH3N)",
        "(CH2N)",
        "(ACNH2)",
        "(C5H5N)",
        "(C5H4N)",
        "(C5H3N)",
        "(CH3CN)",
        "(CH2CN)",
        "(COOH)",
        "(HCOOH)",
        "(CH2CL)",
        "(CHCL)",
        "(CCL)",
        "(CH2CL2)",
        "(CHCL2)",
        "(CCL2)",
        "(CHCL3)",
        "(CCL3)",
        "(CCL4)",
        "(ACCL)",
        "(CH3NO2)",
        "(CH2NO2)",
        "(CHNO2)",
        "(ACNO2)",
        "(CS2)",
        "(CH3SH)",
        "(CH2SH)",
        "(FURF)",
        "(CH2OH)2",
        "(I)",
        "(BR)",
        "(CH=-C)",
        "(C=-C)",
        "(ME2SO)",
        "(ACRY)",
        "CL-C=C)",
        "(ACF)",
        "(DMFA)",
        "(DMF-2)",
        "(CF3)",
        "(CF2)",
        "(CF)",
        "(COO)",
        "(SIH3)",
        "(SIH2)",
        "(SIH)",
        "(SI)",
        "(SIH2O)",
        "(SIHO)",
        "(SIO)"
    ]
var grouplist3 = ["None",
"(CH3)",
"(CH2)",
"(CH)",
"(C)",
"(CH2=CH)",
"(CH=CH)",
"(CH2=C)",
"(CH=C)",
"(C=C)",
"(ACH)",
"(AC)",
"(ACCH3)",
"(ACCH2)",
"(ACCH)",
"(OH)",
"(CH3OH)",
"(H2O)",
"(ACOH)",
"(CH3CO)",
"(CH2CO)",
"(CHO)",
"(CH3COO)",
"(CH2COO)",
"(HCOO)",
"(CH3O)",
"(CH2O)",
"(CH-O)",
"(FCH2O)",
"(CH3NH2)",
"(CH2NH2)",
"(CHNH2)",
"(CH3N)",
"(CH2N)",
"(ACNH2)",
"(C5H5N)",
"(C5H4N)",
"(C5H3N)",
"(C5H2N)",
"(CH3CN)",
"(CH2CN)",
"(CH2CL)",
"(CHCL)",
"(CCL)",
"(CH2CL2)",
"(CHCL2)",
"(CCL2)",
"(CHCL3)",
"(CCL3)",
"(CCL4)",
"(ACCL)",
"(CH3NO2)",
"(CH2NO2)",
"(CHNO2)",
"(CNO2)",
"(ACNO2)",
"(FURF)",
"(CH2OH)2",
"(I)",
"(BR)",
"(ME2SO)",
"CL-(C=C)",
"(DMFA)",
"(NMP)",
"(SULFOL)",
"(ACCOO)",
"(CH3S)",
"(CH2S)",
"(DMA)",
"(DEOH)",
"(NFM)",
"(TEG)"
] 
;
//#endregion


//#region Get the wrapper_table element for the component's input boxes
var nflash = 1;
var wrapper_table = document.getElementById("wrapper_table");
var ncomp_input = document.getElementById("ncomp_input");
var createtable_button = document.getElementById("createtable_button");
var tpz_table = document.getElementById("tpz_table");


// Create an input box for n_components
var nInput = document.createElement("input");
nInput.id = "nInput";
nInput.type = "number";
nInput.min = "2";
nInput.max = "10";
nInput.value = "2";
ncomp_input.appendChild(nInput);

// Create a set button
var setButton = document.createElement("input");
setButton.id = "setButton";
setButton.type = "button";
setButton.value = "Create table";
createtable_button.appendChild(setButton);

// Add an onclick event listener to setButton
setButton.onclick = () => {
    var n_components = nInput.value; // Number of components
    var n_maxgroups = 10; // Max number of functional groups per component
    var partable_sel = partable_box.options[partable_box.selectedIndex].value
    var grouplist = ["ERROR"]
    if (partable_sel == 1){
        grouplist = grouplist1
    } 
    else if(partable_sel == 2){
        grouplist = grouplist2
    } 
    else grouplist = grouplist3
    // Remove any existing selection boxes or lines for each n_components line
    var boxes = document.getElementsByClassName("box");
    while (boxes.length > 0) {
        boxes[0].remove();
    }
    var line = document.getElementsByClassName("line");
    while (line.length > 0) {
        line[0].remove();
    }
    var line = document.getElementsByClassName("tpz_line");
    while (line.length > 0) {
        line[0].remove();
    }
    var line = document.getElementsByClassName("tpz_table_button");
    while (line.length > 0) {
        line[0].remove();
    }
    var line = document.getElementById("tpz_table");
    while (line.length > 0) {
        line[0].remove();
    }
    
    
  
    // Create n_maxgroups selection boxes for each n_components line
    for (var i = 0; i < n_components; i++) {
        // Create a container element for each n_components line
        var line = document.createElement("tr");
        line.className = "line";
        wrapper_table.appendChild(line);
        
        // Create a label for each Component line
        //var label = document.createElement("td");
        //label.textContent = `Component ${(i + 1)}:`;
        var label = document.createElement("td");
            label.textContent = `Component ${(i + 1)}:`;
        var grouplabel =  document.createElement("td");
            grouplabel.textContent = "Group:"
        
        line.appendChild(label);
        line.appendChild(grouplabel);
    
        // Create n_maxgroups selection boxes for each line
        for (var j = 0; j < n_maxgroups; j++) {
            // Create a selection box
            var box_cell = document.createElement("td");
            var box = document.createElement("select");
            box.className = "box";
            // Add the grouplist options for each box
            for (var k = 0; k < grouplist.length; k++) { 
                var option = document.createElement("option");
                option.value = k;
                option.text = grouplist[k];
                box.appendChild(option);
            }
      
            // Append the box to the line
            box_cell.appendChild(box);
            line.appendChild(box_cell);
        }
        // Create a container element for each line
        var line = document.createElement("tr");
            line.className = "line";
        wrapper_table.appendChild(line);
    
        // Create a label for each Component line
        var label = document.createElement("td");
        var label_input = document.createElement("input");
        label_input.id = "grouplabel_input";
        label_input.className = "label_input"
        label_input.type = "string";
        label_input.value = "";
        label_input.setAttribute("size", 10); //Adjust Firefox box size 
        var numberlabel = document.createElement("td");
            numberlabel.textContent = "Number of groups:";
        
        label.appendChild(label_input);
        line.appendChild(label);      
        line.appendChild(numberlabel);

        for (var j = 0; j < n_maxgroups; j++) {
            // Create a selection box
            var ngroup_cell = document.createElement("td")
            var ngroup_Input = document.createElement("input");
            ngroup_Input.id = "ngroup_Input";
            ngroup_Input.className = "input"
            ngroup_Input.type = "number";
            ngroup_Input.min = "0";
            ngroup_Input.max = "100000"; //Also adjust Chromium box size
            ngroup_Input.value = "0";
            ngroup_Input.setAttribute("size", 5); //Adjust Firefox box size 
            
          
            // Append the box to the line
            ngroup_cell.appendChild(ngroup_Input);
            line.appendChild(ngroup_cell);
        }
    }
    var option_size = document.querySelectorAll("option"); //Adjust box size
    option_size.forEach(function(option) {
        if (option.textContent.length > 13) {
            option.textContent = option.textContent.substring(0, 13) + "...";
        };
    });
    
    //Create TPZ Table
    
    var line = document.createElement("tr");
        line.className = "tpz_table_button"
    var nflash_label_td = document.createElement("td");
        nflash_label_td.textContent = "# of flash calculations:"
    var nflash_input_td = document.createElement("td");
    var nflash_input = document.createElement("input");
        nflash_input.id = "nflash_input";
        nflash_input.className = "input"
        nflash_input.type = "number";
        nflash_input.min = "1";
        nflash_input.max = "10"; //Also adjust Chromium box size
        nflash_input.value = "1";
        nflash_input.setAttribute("size", 5); //Adjust Firefox box size     
    var tpz_table_td = document.createElement("td");
        
    var tpz_table_button = document.createElement("input");
        tpz_table_button.id = "tpz_table_button";
        tpz_table_button.type = "button";
        tpz_table_button.value = "Create table";
        tpz_table_button.onclick = () => {
            var tpz_table_delete = document.getElementById("tpz_table");
            while (tpz_table_delete.length > 0) {
            tpz_table_delete.remove();
            console.log("H")
            };
            nflash = nflash_input.value;
            var tpz_head = document.createElement("tr");
            tpz_head.className = "tpz_line";
    
            var tpz_th = document.createElement("th");
            tpz_th.textContent = "Conditions";
            tpz_th.className = "tpz_line"

            var tpz_head_N = document.createElement("td");
            tpz_head_N.textContent = "";
            var tpz_head_T = document.createElement("td");
            tpz_head_T.textContent = "T (K)";
            var tpz_head_P = document.createElement("td");
            tpz_head_P.textContent = "P (atm)";
    
            tpz_head.appendChild(tpz_head_N);
            tpz_head.appendChild(tpz_head_T);
            tpz_head.appendChild(tpz_head_P);
    
            for (var i = 0; i < nInput.value; i++) {
                var tpz_head_z = document.createElement("td");
                tpz_head_z.textContent = `z${i+1}`
    
                tpz_head.appendChild(tpz_head_z);
    
            }
            tpz_table.appendChild(tpz_th);
            tpz_table.appendChild(tpz_head);
        
            tpz_table.appendChild(tpz_head);

            for (var i = 0; i < nflash; i++) {
                var tpz_line = document.createElement("tr");
                tpz_line.className = "tpz_line";
        
                var tpz_comp = document.createElement("td");
                tpz_comp.textContent=`Flash ${(i+1)}`;
        
                tpz_line.appendChild(tpz_comp);
        
        
                // Create a label for each Component line
                var tpz_T = document.createElement("td");
                var tpz_P  =  document.createElement("td");
        
                var tpz_T_input = document.createElement("input");
                tpz_T_input.id = "tpz_T_input";
                tpz_T_input.className = "tp_input"
                tpz_T_input.type = "number";
                tpz_T_input.min = "0";
                tpz_T_input.max = "10000"; //Also adjust Chromium box size
                tpz_T_input.value = "273";
        
                var tpz_P_input = document.createElement("input");
                tpz_P_input.id = "tpz_P_input";
                tpz_P_input.className = "tp_input"
                tpz_P_input.type = "number";
                tpz_P_input.min = "1";
                tpz_P_input.max = "10000"; //Also adjust Chromium box size
                tpz_P_input.value = "1";
      
                tpz_T.appendChild(tpz_T_input);
                tpz_P.appendChild(tpz_P_input);
        
                tpz_line.appendChild(tpz_T);
                tpz_line.appendChild(tpz_P);

                for (var p = 0; p < nInput.value; p++) {
                    var tpz_z = document.createElement("td");
                    var tpz_z_input = document.createElement("input");
                    tpz_z_input.id = "tpz_z_input";
                    tpz_z_input.className = "z_input"
                    tpz_z_input.type = "number";
                    tpz_z_input.min = "1";
                    tpz_z_input.max = "10000"; //Also adjust Chromium box size
                    tpz_z_input.value = "0";
                    tpz_z.appendChild(tpz_z_input);
                    tpz_line.appendChild(tpz_z);
                }
        
                
                tpz_table.appendChild(tpz_line);
            }    
        };
    
        tpz_table_td.appendChild(tpz_table_button);
        nflash_input_td.appendChild(nflash_input)
        line.appendChild(nflash_label_td);
        line.appendChild(nflash_input_td);
        line.appendChild(tpz_table_td);
        tpz_table.appendChild(line);
};
//#endregion

//#region Create a download button
var div_download = document.getElementById("div_download");
var downloadButton = document.createElement("input");
downloadButton.id = "downloadButton";
downloadButton.type = "button";
downloadButton.value = "Download";
div_download.appendChild(downloadButton);
// Add an onclick event listener to downloadButton
downloadButton.onclick = () => {
    var values = [];
    
    var model_sel = model_box.options[model_box.selectedIndex].value
    var partable_sel = partable_box.options[partable_box.selectedIndex].value

    values.push(`"${problem_name.value}"`) //Print name of file
    values.push(
        `${icalc},${model_sel},${ipr},${iout},${novap},${ig},${partable_sel}`)
    //#region Get all the input and selection values
    
    
    values.push(`${nInput.value}`);
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
    jj = 0
    kk = 0
    for (var j = 0; j < (nflash); (j++)) {
        z_values=[]
        TP_values=[]
        for (var k = 0; k < nInput.value; k++){
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

        
    
  //#endregion

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
};
//#endregion