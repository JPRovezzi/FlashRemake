export function savealert(os_box,model_box,partable_box,problem_name) {
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
};
