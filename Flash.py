import os

###############################################################################

def write_input(folder_path: str, Ncomp, Ngrup):
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
    file.write(f"0 \n\"{name1}\"")
    file.close()

###############################################################################

folder_path = str(os.getcwd())

exe_name    = "Flash.exe"
exe_path    = folder_path+f"/{exe_name}"

write_input(folder_path,2,2)

###############################################################################


