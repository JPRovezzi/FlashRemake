import * as gramjs from './gruposram.js'

//#region remover function

export function remover(){
    // Remove any existing selection boxes or lines for each ncomp_value line
    //complabel_td.textContent = " ";
    flashlabel_td.textContent  = " ";
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
    var grouprows = document.getElementsByClassName("group_tr");
    while (grouprows.length > 0) {
        grouprows[0].remove();
    }
    var gnumrows = document.getElementsByClassName("gnum_tr");
    while (gnumrows.length > 0) {
        gnumrows[0].remove();
    }
}
//#endregion

//#region compremover function
export function compremover(){
    complabel_td.textContent = " ";
    var remove_element = document.getElementsByClassName("comp_button");
    while (remove_element.length > 0) {
        remove_element[0].remove();
    };
    flash_remover();
}


//#endregion

//#region tpzinput remover function
export function flash_remover(){
    flashlabel_td.textContent = " ";
    var remove_element = document.getElementsByClassName("tpz_button");
    while (remove_element.length > 0) {
        remove_element[0].remove();
    };
}

//#endregion

//#region tpzremover function

export function tpzremover(){
    // Remove any existing selection boxes or lines for each ncomp_value line
    //complabel_td.textContent = " ";
    //flashlabel_td.textContent  = " ";
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

//-----------------------------------------------------------------------------

//#region Range function
export function frange(start,end,step){
    var frange_array = [];
    for (var i = start; i < end; i++){
        //console.log(i);
        frange_array.push(i);
    }
    //console.log(frange_array.length);
    return frange_array;
}
//#endregion

//-----------------------------------------------------------------------------

//#region Savealert function
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

//#region Create input box for component table 

export function compinput(partable_box){
    
    //var nflash_value = 1;
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
        ncomp_input.className="comp_button"
        ncomp_input.type = "number";
        ncomp_input.min = "2";
        ncomp_input.max = "10";
        ncomp_input.value = "2";
        ncomp_td.appendChild(ncomp_input);

    // Create a set button
    var setButton = document.createElement("input");
        setButton.className="comp_button"
        setButton.id = "setButton";
        setButton.type = "button";
        setButton.value = "Create table";
        comptable_td.appendChild(setButton);

    // Add an onclick event listener to setButton
    setButton.onclick = () => {
        comptable(ncomp_input,partable_box);
        flash_remover();
    }    
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
    var grouplist_index = []

    if (partable_sel == 1){
        grouplist = gramjs.grouplist1;
        grouplist_index = frange(0,grouplist.length,1);
    } 
    else if(partable_sel == 2){
        grouplist = gramjs.grouplist2;
        //console.log(frange(0,grouplist.length,1));
        grouplist_index = frange(0,grouplist.length,1);
    } 
    else {
        grouplist = gramjs.grouplist3;
        grouplist_index = frange(0,grouplist.length,1)
    } 
 
    // Create maxngroups selection boxes for each ncomp_value group row
    for (var i = 0; i < ncomp_value; i++) {
        // Create a row for group input for each component.
        var group_tr = document.createElement("tr");
            group_tr.className = "group_tr";
            group_tr.id=`${(i+1)}group_tr`;
        wrapper_table.appendChild(group_tr);
        
        // Create a label for each Component group row
        var complabel_td = document.createElement("td");
            complabel_td.textContent = `Component ${(i + 1)}:`;
        // Create text before group selection
        var grouplabel_td =  document.createElement("td");
            grouplabel_td.textContent = "Group:"
        
        group_tr.appendChild(complabel_td);
        group_tr.appendChild(grouplabel_td);
    
        // Create maxngroups number of group selection boxes for each row
        for (var j = 0; j < maxngroups; j++) {
            // Create a selection box
            var groupbox_td = document.createElement("td");
            var groupbox = document.createElement("select");
                groupbox.className = "groupbox";
            // Add the grouplist options for each box
            for (var k = 0; k < grouplist.length; k++) { 
                var groupoption = document.createElement("option");
                    groupoption.value = k;
                    groupoption.text = `[${grouplist_index[k]}]`+" "+grouplist[k];
                groupbox.appendChild(groupoption);
            }
      
            // Append the boxes to the row
            groupbox_td.appendChild(groupbox);
            group_tr.appendChild(groupbox_td);
        }
        // Create a container element for each line
        var gnum_tr = document.createElement("tr");
            gnum_tr.className = "gnum_tr";
            gnum_tr.id=`${(i+1)}gnum_tr`;
        wrapper_table.appendChild(gnum_tr);
    
        // Create a label for each Component line
        var compname_td = document.createElement("td");
        var compname_input = document.createElement("input");
            compname_input.id = "compname_input";
            compname_input.className = "compname_input";
            compname_input.type = "string";
            compname_input.value = "";
            compname_input.setAttribute("size", 10); //Adjust Firefox box size 
        // Create text before the number input
            var numlabel_td = document.createElement("td");
            numlabel_td.textContent = "Number of groups:";
        
        compname_td.appendChild(compname_input);
        gnum_tr.appendChild(compname_td);      
        gnum_tr.appendChild(numlabel_td);

        for (var j = 0; j < maxngroups; j++) {
            // Create a selection box
            var gnumbox_td = document.createElement("td")
            var gnumbox = document.createElement("input");
                gnumbox.id = "gnumbox";
                gnumbox.className = "gnumbox"
                gnumbox.type = "number";
                gnumbox.min = "0";
                gnumbox.max = "100000000"; //Also adjust Chromium box size
                gnumbox.value = "0";
                gnumbox.setAttribute("size", 14); //Adjust Firefox box size 
            
          
            // Append the box to the line
            gnumbox_td.appendChild(gnumbox);
            gnum_tr.appendChild(gnumbox_td);
        }
    }
    var option_size = document.querySelectorAll("option"); //Adjust box size
    option_size.forEach(function(option) {
        if (option.textContent.length > 13) {
            option.textContent = option.textContent.substring(0, 13) + "...";
        };
    });
    
    //Create TPZ Table
    savecomp(ncomp_input);

}
//#endregion

//#region Create Save button for component table

export function savecomp(ncomp_input){
    var savecomp_div = document.getElementById("savecomp_div");
    var savecomp_button = document.createElement("input");
        savecomp_button.className = "button"
        savecomp_button.id = "saveconfig_button";
        savecomp_button.type = "button";
        savecomp_button.value = "Save";
        savecomp_button.onclick = () => {
            flash_remover();
            tpzremover();
            tpz(ncomp_input);
            console.log(countgroup());
        }
        savecomp_div.appendChild(savecomp_button);
        
}


//#endregion

//#region Count Number and kind of group
export function countgroup(){
    var final_array = [[],[]];
    var temp_array = [[],[]];

    var groups = document.getElementsByClassName("groupbox");
    var gnums = document.getElementsByClassName("gnumbox");
    //}
    for (var j = 0; j < groups.length; j++) {
        temp_array[0].push(groups[j].value);
        temp_array[1].push(gnums[j].value);
    }
        //console.log(temp_array[0]);
        //console.log(temp_array[1]);
    for (var k = 0; k < temp_array[0].length; k++) {
        if (final_array[0].includes(temp_array[0][k])){
            var index = final_array[0].indexOf(temp_array[0][k]);
            final_array[1][index] = final_array[1][index] + parseInt(temp_array[1][k],10); 
        } else {
            final_array[0].push(temp_array[0][k]);
            final_array[1].push(parseInt(temp_array[1][k],10));
        }
    }
    final_array[1].splice(final_array[0].indexOf("0"),1);
    final_array[0].splice(final_array[0].indexOf("0"),1);
    return final_array;

}
//#endregion

//#region create a button to TPZ table
export function tpz(ncomp_input){
    var flashlabel_td = document.getElementById("flashlabel_td");
        //flashlabel_td.className = "tpz_line"
        flashlabel_td.textContent = "# of flash calculations:";

    var nflash_input = document.createElement("input");
        nflash_input.id = "nflash_input";
        nflash_input.className = "tpz_button";
        nflash_input.type = "number";
        nflash_input.min = "1";
        nflash_input.max = "10"; //Also adjust Chromium box size
        nflash_input.value = "1";
        nflash_input.setAttribute("size", 5); //Adjust Firefox box size     
        
    var tpz_table_button = document.createElement("input");
        tpz_table_button.id = "tpz_table_button";
        tpz_table_button.className="tpz_button";
        tpz_table_button.type = "button";
        tpz_table_button.value = "Create table";
        tpz_table_button.onclick = () => {
            tpzremover();
            tpztable(nflash_input,ncomp_input);
        }
        nflash_td.appendChild(nflash_input);
        flashtable_td.appendChild(tpz_table_button);
}

//#endregion

//#region Create TPZ Table

export function tpztable(nflash_input, ncomp_input){
    var nflash_value = nflash_input.value;
    
    var tpz_head = document.createElement("tr");
        tpz_head.className = "tpz_line";
    
    var tpz_th = document.createElement("th");
        tpz_th.textContent = "Conditions:";
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
            tpz_head_z.textContent = `z${i+1}`;
    
            tpz_head.appendChild(tpz_head_z);
    
        }
    
    tpz_table.appendChild(tpz_th);
            tpz_table.appendChild(tpz_head);
        
            tpz_table.appendChild(tpz_head);

            for (var i = 0; i < nflash_value; i++) {
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
                tpz_T_input.className = "tp_input";
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
                    tpz_z_input.min = "0";
                    tpz_z_input.max = "10000"; //Also adjust Chromium box size
                    tpz_z_input.value = "0";
                    tpz_z_input.step = "0.05";
                    tpz_z.appendChild(tpz_z_input);
                    tpz_line.appendChild(tpz_z);
                }
        
                
                tpz_table.appendChild(tpz_line);
            }    
        };

//#endregion








































