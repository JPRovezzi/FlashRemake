import os

###############################################################################

def write_input():
    """
    This subroutine writes the input file
    '!   icalc:  0-' **** FLASH CALCULATION ****'
    '!           1-' **** BINODAL CURVE CALCULATION ****'
    '!           2-' **** CALCULATION OF UNIQUAC PARAMETERS FROM UNIFAC **** '
    '!   model:  0-' MODEL USED FOR LIQUID PHASE NON-IDEALITY: UNIFAC'
    '!           1-' MODEL USED FOR LIQUID PHASE NON-IDEALITY: UNIQUAC'
    '!   ipr:    1-' ** COMPARISON OF ACTIVITIES CALCULATED BY UNIFAC AND UNIQUAC, RESPECTIVELY **'
    '!   iout:   1-'open 'lleasoccuzada.OUT''
    '!   novap:  0-'VAPOR PHASE INCLUDED IN FLASH-CALCULATIONS'
    '!   ig:     0-'write compositions'
    '!           1-'write compositions and activities'
    '!   ipareq: 1-'liquid-liquid parameters table (UNIFAC)'
    '!           2-'liquid-vapor parameters table (UNIFAC)'
    '!           3-'infinite dilution parameters table (UNIFAC)'
    '!           4-'GC-EOS parameters'
    """
    #Hola
    global Name,name1,Ncomp,Ngrup,folder_path,icalc,model1,ipr,iout,novap,ig,ipareq
    global T,P,Z
    file0  = open(folder_path + "name.dat","w")
    file0.write(f"0 \n\"{name1}\"\n")
    file0.close()
    file1  = open(f"{folder_path}/{name1}","w")
    file1.write(f"\"{Name}\"\n")
    file1.write(f"{icalc},{model1},{ipr},{iout},{novap},{ig},{ipareq}\n")
    file1.write(f"{Ncomp}\n")
    file1.write(f"{Ngrup},{(Ngrup*Ngrup)}\n")
    #
    file1.write(f"{T},{(P)}\n")
    for i in Z:
        file1.write(f"{T},{(P)}\n")
    file1.write("\n")
    file1.write(f"{T},{(P)}\n")
    file1.write(f"0,0")
    return True

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
Name    = "llecalas2"
name1   = Name + ".dat"

#Variables para modificar
Ncomp   = 2  #Numero de componentes
Ngrup   = 2  #Numero de grupos
ipareq  = 2  #Tabla de par�metros
modelo  = "UNIFAC"  #Modelo
T       = 300 #Temperatura (K)
P       = 1 #Presion (Bar)
Z       = [0.5,0.5] #Composicion (len = Ncomp)

icalc   = 0
ipr     = 0
iout    = 1
novap   = 0
ig      = 1

model1  = lee_modelo(modelo)
write_input(folder_path,name1)

###############################################################################


