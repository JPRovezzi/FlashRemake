import os

###############################################################################

def write_input(folder_path: str, Name:str, Ncomp: int, Ngrup: int):
    """
    This subroutine writes the input file
    """
    #Ncomp = int(input())
    #Ngrup = None #REVISAR
    
    icalc = 0
    ipr = 0
    iout = 1
    novap = 0
    ig = 1
    
    Name  = str(input())
    name1 = Name + ".dat"
    file  = open(folder_path + "name.dat","w")
    file.write(f"0 \n\"{name1}\"\n")
    file.write(f"{Name}\n")
    file.close()

def lee_modelo(modelo: str):
    if modelo == "UNIFAC":
        model1 = 0
    elif modelo == "A-UNIFAC":
        model1 = 2
    else:
        model1 = -1
    return model1

###############################################################################

folder_path = str(os.getcwd())

exe_name    = "Flash.exe"
exe_path    = folder_path+f"/{exe_name}"

#Revisar:
Ncomp   = None  #Numero de componentes
Ngrup   = None  #Numero de grupos
ipareq  = None  #Tabla de par�metros

write_input(folder_path,2,2)

###############################################################################


