import * as gramjs from './gruposram.js'

//#region remover function

export function remover(){
    // Remove any existing selection boxes or lines for each ncomp_value line
    complabel_td.textContent = " "
    var buttons = document.getElementsByClassName("button");
    while (buttons.length > 0) {
        buttons[0].remove();
    }
    
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
    var tpzline = document.getElementsByClassName("tpz_table_button");
    while (tpzline.length > 0) {
        tpzline[0].remove();
    }
    var line = document.getElementById("tpz_table");
    while (line.length > 0) {
        line[0].remove();
    }
}
//#endregion

//#region savealert function
export function savealert(
    os_box,model_box,partable_box,problem_name
    ) {

    var os_sel = os_box.options[os_box.selectedIndex].text
    var model_sel = model_box.options[model_box.selectedIndex].text
    var partable_sel = partable_box.options[
        partable_box.selectedIndex].text
    alert(
        "Saved problem data and configuration:\n\n"+
        "#-PROBLEM-NAME:".padEnd(23,"_")        + problem_name.value
        + "\n"  +
        "#-OPERATING-SYSTEM:".padEnd(24,"_")    + os_sel
        + "\n"  +
        "#-MODEL:".padEnd(25,"_")               + model_sel 
        + "\n"  +
        "#-PARAMETERS-TABLE:".padEnd(24,"_")    + partable_sel  
        + "\n\n"        +
        "-> FLASH-CALCULATION;".padEnd(0,"_")
        + "\n"  +
        `-> COMPARISON OF ACTIVITIES CALCULATED BY ${model_sel};`   
        + "\n"  +
        "-> OPEN 'lleasoccuzada.OUT';"
        + "\n"  +
        "-> VAPOR PHASE INCLUDED IN FLASH-CALCULATIONS;"
        + "\n"   +
        "-> WRITE COMPOSITIONS AND ACTIVITIES."
    );
    remover();
    compinput(partable_box);

}
//#endregion

//#region create input box for component table 

export function compinput(partable_box){
    
    //var nflash = 1;
    var wrapper_table = document.getElementById("wrapper_table");
    var ncomp_td = document.getElementById("ncomp_td");
    var complabel_td = document.getElementById("complabel_td");
    var comptable_td = document.getElementById("comptable_td");
    var tpz_table = document.getElementById("tpz_table");

    // Create a label for the n_components input box
        complabel_td.textContent = "Number of components:"
        //complabel_td.className = "button"

    // Create an input box for n_components
    var ncomp_input = document.createElement("input");
        ncomp_input.id = "ncomp_input";
        ncomp_input.className="button"
        ncomp_input.type = "number";
        ncomp_input.min = "2";
        ncomp_input.max = "10";
        ncomp_input.value = "2";
        ncomp_td.appendChild(ncomp_input);

    // Create a set button
    var setButton = document.createElement("input");
        setButton.className="button"
        setButton.id = "setButton";
        setButton.type = "button";
        setButton.value = "Create table";
        comptable_td.appendChild(setButton);

    // Add an onclick event listener to setButton
    setButton.onclick = () => {comptable(ncomp_input,partable_box)}    
    };

//#endregion

//#region Create component table subroutine
export function comptable(ncomp_input,partable_box){
    
    remover();   
    var ncomp_value = ncomp_input.value; // Number of components
    var maxngroups = 10; // Max number of functional groups per component
    var partable_sel = partable_box.options[partable_box.selectedIndex].value
    
    //Set the Group list from the "partable" information.
    var grouplist = ["ERROR"]
    if (partable_sel == 1){
        grouplist = gramjs.grouplist1;
    } 
    else if(partable_sel == 2){
        grouplist = gramjs.grouplist2;
    } 
    else grouplist = gramjs.grouplist3;
 
    // Create maxngroups selection boxes for each ncomp_value line
    for (var i = 0; i < ncomp_value; i++) {
        // Create a container element for each ncomp_value line
        var line = document.createElement("tr");
            line.className = "line";
        wrapper_table.appendChild(line);
        
        // Create a label for each Component line
        var label = document.createElement("td");
            label.textContent = `Component ${(i + 1)}:`;
        var grouplabel =  document.createElement("td");
            grouplabel.textContent = "Group:"
        
        line.appendChild(label);
        line.appendChild(grouplabel);
    
        // Create maxngroups selection boxes for each line
        for (var j = 0; j < maxngroups; j++) {
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

        for (var j = 0; j < maxngroups; j++) {
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
        tpz_table_button.className="button"
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
    
            for (var i = 0; i < ncomp_input.value; i++) {
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

                for (var p = 0; p < ncomp_input.value; p++) {
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
}
//#endregion