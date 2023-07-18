VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Hoja1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = True
Option Explicit
    
'Funciones del api
'''''''''''''''''''''''''''''''''''''''''''''''''''
Private Declare PtrSafe Function OpenProcess Lib "kernel32" _
      (ByVal dwDesiredAccess As Long, _
       ByVal bInheritHandle As Long, _
       ByVal dwProcessId As Long) As Long

Private Declare PtrSafe Function GetExitCodeProcess Lib "kernel32" _
      (ByVal hProcess As Long, lpExitCode As Long) As Long
      
Private Declare PtrSafe Function CloseHandle Lib "kernel32" _
      (ByVal hObject As Long) As Long
      
'Constantes
'''''''''''''''''''''''''''''''''''''''''''''''''''
Private Const PROCESS_QUERY_INFORMATION = &H400
    Private Const STATUS_PENDING = &H103&


Sub Main()

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'                 SUBRUTINA PRINCIPAL
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    Dim program As String
    Dim path As String
    Dim invar As Boolean
    On Error Resume Next
    

    
'Lee la ruta en donde se encuentran el programa y en donde se va a generar el input
    path = Range("B2")
    
'Escritura del input
    Call write_input(path, invar)
    If Not invar Then
        Exit Sub
    End If
'Llamado a la subrutina que ejecuta el programa
    'MsgBox (CurDir()) 'Esta línea emite un cartel informando el directorio en el que está ubicado el programa
    ChDir path
    program = "flash.exe"
    Call Command1_Click(program)

'Lectura del output de Fortran y escritura de los datos en el libro excel
    Call Write_Output(path)
       

End Sub
Sub reset()
    Rows("4:150").Select 'borra filas
    Selection.Delete Shift:=xlUp
    Cells(4, 1).Select
End Sub

Sub Charge_problem()
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This subroutine writes the input file
''''''''''''''''''''''' '''''''''''''''''''''''''''''''''''''
 '   Dim name, name1 As String
 '   Dim icalc, model1, ipr, iout, novap, ig, ipareq As Integer
 '   Dim Ncomp, i, j, ng, row_id, ncond As Integer
 '   Dim T, P, z() As Double
    Dim row_id, Ncomp, Ngrup, i, j, k, TableNumber As Integer
    Dim path, line As String
   
    UserForm1.Show
   
'lee número de componentes
    row_id = 5
    TableNumber = Cells(row_id, 2)
    row_id = row_id + 1
    Ncomp = Cells(row_id, 2)
    
    
'Genera espacio para cargar componentes
    
    Rows("7:150").Select 'borra filas
    Selection.Delete Shift:=xlUp
    row_id = row_id + 1
    
    For j = 1 To Ncomp
        row_id = row_id + 1
        Cells(row_id, 1) = "Component " + CStr(j)
        Call combinar_componentes(row_id)
        Cells(row_id, 2) = "Group"
        For i = 3 To 12
            Cells(row_id, i) = "Choose"
            Cells(row_id + 1, i) = "-"
            
            Cells(row_id, i).Select
            With Selection.Validation
            
            If TableNumber = 1 Then
        .Add Type:=xlValidateList, AlertStyle:=xlValidAlertStop, Operator:= _
        xlBetween, Formula1:="=gruposram!$A$1:$A$101"
            ElseIf TableNumber = 2 Then
        .Add Type:=xlValidateList, AlertStyle:=xlValidAlertStop, Operator:= _
        xlBetween, Formula1:="=gruposram!$B$1:$B$101"
            ElseIf TableNumber = 3 Then
        .Add Type:=xlValidateList, AlertStyle:=xlValidAlertStop, Operator:= _
        xlBetween, Formula1:="=gruposram!$C$1:$C$101"
            End If
            
                .IgnoreBlank = True
                .InCellDropdown = True
                .InputTitle = ""
                .ErrorTitle = ""
                .InputMessage = ""
                .ErrorMessage = ""
                .ShowInput = True
                .ShowError = True
            End With

        Next i
            row_id = row_id + 1
            Cells(row_id, 2) = "Number of groups"
    
    Next j
    row_id = row_id + 3


    

 
End Sub

Sub LeerBases1()

    Dim Name, name1, path, Name_group, Name_comp(10), ngt As String
    Dim row_id, col_id, sup, model1, ipareq, Ncomp, Ngrup, Ngrupa, i, j, k As Integer
    Dim gruposram As Worksheet
   ' Dim npunt(150), Ngrup(150) As Integer
    Dim program As String
    Dim linea As String
    
    Close 1#
'SENTENCES
    Worksheets("Input").Select
    Name = Range("B1")
    path = Range("B2")
    
    name1 = Name + ".dat"
    
'Genera archivo "name" en donde escribe el nombre del archivo de entrada
    Open path + "name.dat" For Output As #1
    Write #1, 1
    Write #1, name1
    Close #1
    
   'Abre archivo de entrada
    Open path + name1 For Output As #1
    
    
'Lee modelo
    row_id = 4
    If Cells(row_id, 2) = "UNIFAC" Then
        model1 = 1
    ElseIf Cells(row_id, 2) = "A-UNIFAC" Then
        model1 = 2
    End If
    
'Lee tabla de parámetros
    row_id = row_id + 1
    ipareq = Cells(row_id, 2)
    
'Lee número de componentes
    row_id = row_id + 1
    Ncomp = Cells(row_id, 2)
    
    Write #1, model1, ipareq, Ncomp
    
'Lee componentes
   
    Set gruposram = ThisWorkbook.Sheets("gruposram")
   
    row_id = row_id + 2

    For i = 1 To Ncomp
        Name_comp(i) = Cells(row_id, 1)
        j = 1
        Name_group = Cells(row_id, 2 + j).Value
        Do While Name_group <> "Choose"
            k = 1
            ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
            Do While ngt <> "fin"
                If ngt = Name_group Then Exit Do
                k = k + 1
                ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
            Loop
            Write #1, (k - 2), Cells(row_id + 1, 2 + j).Value,

            j = j + 1
            Name_group = Cells(row_id, 2 + j).Value
        Loop
       Write #1,
        row_id = row_id + 2
    Next i
    Close 1#
    
'Escritura de parámetros en hoja
    ChDir path
    program = "flash.exe"
    Call Command1_Click(program)
    
    Open path + "parameters.dat" For Input As #1
                
    row_id = row_id + 2
    sup = row_id - 1
    Range(Cells(sup, 1), Cells(200, 200)).Select 'borra filas
    Selection.Delete Shift:=xlUp
    col_id = 2
    Cells(row_id, 1) = "r"
    Call azul(row_id, 1)
    row_id = row_id + 1
    Cells(row_id, 1) = "q"
    Call azul(row_id, 1)
    row_id = row_id + 1

'Escritura de grupos
    Ngrup = 0
    linea = leer_linea(1)
    Do While linea <> "end"
        Ngrup = Ngrup + 1
        Cells(row_id, 1) = gruposram.Cells(CInt(linea) + 2, ipareq)
        Call azul(row_id, 1)
        Cells(sup, col_id) = gruposram.Cells(CInt(linea) + 2, ipareq)
        Call azul(sup, col_id)
        linea = leer_linea(1)
        row_id = row_id + 1
        col_id = col_id + 1
    Loop
    Cells(row_id - Ngrup - 3, 1) = Ngrup
    'pone en blanco el contenido de la celda
    Cells(row_id - Ngrup - 3, 1).Select
    With Selection.Font
        .ThemeColor = xlThemeColorDark1
        .TintAndShade = 0
    End With
'Escritura de parámetros grupales
    row_id = row_id - Ngrup - 2
    linea = leer_linea(1)
    col_id = 2
    Do While linea <> "end"
        Cells(row_id, col_id) = linea
        Call blanco(row_id, col_id)
        linea = leer_linea(1)
        Cells(row_id + 1, col_id) = linea
        Call blanco(row_id + 1, col_id)
        linea = leer_linea(1)
        col_id = col_id + 1
    Loop

'lectura parámetros interacción
    row_id = row_id + 2

    linea = leer_linea(1)
    Do While linea <> "endint"
        col_id = 2
        Do While linea <> "end"
            Cells(row_id, col_id) = linea
            Call blanco(row_id, col_id)
            linea = leer_linea(1)

            col_id = col_id + 1
        Loop
        row_id = row_id + 1
        linea = leer_linea(1)
    Loop
'lectura composición grupal
    For i = 1 To Ncomp
        col_id = 2
        Cells(row_id, 1) = Name_comp(i)
        Call azul(row_id, 1)
        linea = leer_linea(1)
        Do While linea <> "end"
            Cells(row_id, col_id) = linea
            Call blanco(row_id, col_id)
            linea = leer_linea(1)
            col_id = col_id + 1
        Loop
        row_id = row_id + 1
    Next i
    
'Escritura de grupos asociativos
    If model1 = 2 Then
    Ngrupa = 0
    linea = leer_linea(1)
    row_id = row_id - Ngrup - Ncomp - 3
    col_id = 3 + Ngrup
    Cells(row_id, col_id) = "Associative groups"
    Call azul(row_id, col_id)
    Call azul(row_id, col_id + 1)
    row_id = row_id + 1
    Cells(row_id, col_id) = "Group"
    Call blanco(row_id, col_id)
    Cells(row_id, col_id + 1) = "N. of sites"
    Call blanco(row_id, col_id + 1)
    row_id = row_id + 1
    Do While linea <> "end"
        Ngrupa = Ngrupa + 1
        Cells(row_id, col_id) = CStr(Ngrupa) + "- " + CStr(gruposram.Cells(CInt(linea) + 2, ipareq))
        Call blanco(row_id, col_id)
        linea = leer_linea(1)
        Cells(row_id, col_id + 1) = linea
        Call blanco(row_id, col_id + 1)
        linea = leer_linea(1)
        row_id = row_id + 1

    Loop
    
'Escritura matriz energía asociación
    row_id = row_id - Ngrupa - 2
    col_id = col_id + 3
    Cells(row_id, col_id) = "Energy parameters"
    Call azul(row_id, col_id)
    Call azul(row_id, col_id + 1)
    row_id = row_id + 1
    linea = leer_linea(1)
    Do While linea <> "enden"
        k = 1
        Do While linea <> "end"
            Cells(row_id, col_id) = linea
            Call blanco(row_id, col_id)
            linea = leer_linea(1)
            col_id = col_id + 1
            k = k + 1
        Loop
        linea = leer_linea(1)
        col_id = col_id - k + 1
        
        row_id = row_id + 1
    Loop
    
'Escritura matriz volumen asociación
    row_id = row_id - Ngrupa * 2 - 1
    
    col_id = col_id + Ngrupa * 2 + 1
    Cells(row_id, col_id) = "Volume parameters"
    Call azul(row_id, col_id)
    Call azul(row_id, col_id + 1)
    row_id = row_id + 1
    linea = leer_linea(1)
    Do While linea <> "enden"
        k = 1
        Do While linea <> "end"
            Cells(row_id, col_id) = linea
            Call blanco(row_id, col_id)
            linea = leer_linea(1)
            col_id = col_id + 1
            k = k + 1
        Loop
        linea = leer_linea(1)
        col_id = col_id - k + 1
        
        row_id = row_id + 1
    Loop
    End If
'*****************************************************
'Escritura datos de flash
    row_id = 14 + Ncomp * 3 + Ngrup
    Cells(row_id, 1) = "Flash"
    Call azul(row_id, 1)
    Call combinar(row_id)
    Cells(row_id - 1, 2) = "Conditions"
    Call azul(row_id - 1, 2)
    Cells(row_id, 2) = "T[K]"
    Call azul(row_id, 2)
    Call blanco(row_id + 1, 2)
    Cells(row_id, 3) = "P[atm]"
    Call azul(row_id, 3)
    Call blanco(row_id + 1, 3)
    For i = 1 To Ncomp
        Cells(row_id, 3 + i) = "z" + CStr(i)
        Call azul(row_id, 3 + i)
        Call blanco(row_id + 1, 3 + i)
    Next i
    
   
    Close 1#
End Sub

Sub Leer_comp(row_id As Integer, Ncomp As Integer)




End Sub

Sub write_input(path As String, invar As Boolean)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This subroutine writes the input file
''''''''''''''''''''''' '''''''''''''''''''''''''''''''''''''
    Dim Name, name1, Name_group, ngt As String
    Dim icalc, model1, ipr, iout, novap, Ngrup, ig, ipareq As Integer
    Dim Ncomp, i, j, k, NG, row_id, cont, ncond As Integer
    Dim T, P, z() As Double
    Dim gruposram As Worksheet
    
   Close #1
   
    invar = True


    Ncomp = Cells(6, 2)
    Ngrup = Cells(9 + Ncomp * 2, 1)
    row_id = 15 + Ncomp * 3 + Ngrup
    If IsEmpty(Cells(row_id, 2)) Then 'Pregunta si T está cargada
        MsgBox "Load Temperature"
        invar = False
        Exit Sub
    End If
    
    If IsEmpty(Cells(row_id, 3)) Then 'Pregunta si P está cargada
        MsgBox "Load Pressure"
        invar = False
        Exit Sub
    End If
    
    For i = 1 To Ncomp
        
        If IsEmpty(Cells(row_id, 3 + i)) Then 'Pregunta si z está cargada
            MsgBox "Load Composition"
            invar = False
            Exit Sub
        End If
        
    Next i
    
'SENTENCES
    Worksheets("Input").Select

    Name = Range("B1")
   
    name1 = Name + ".dat"
    
'Genera archivo "name" en donde escribe el nombre del archivo de entrada
    Open path + "name.dat" For Output As #1
    Write #1, 0
    Write #1, name1
    Close #1
    
   'Abre archivo de entrada
    Open path + name1 For Output As #1


   'Escribe archivo de entrada
   
   icalc = 0
   ipr = 0
   iout = 1
   novap = 0
   ig = 1
   
'Lee modelo
   row_id = 4
    If Cells(row_id, 2) = "UNIFAC" Then
        model1 = 0
    ElseIf Cells(row_id, 2) = "A-UNIFAC" Then
        model1 = 2
    End If
        
'Lee tabla de parámetros
   row_id = row_id + 1
   ipareq = Cells(row_id, 2).Value
   
'Escribe nombre de archivo en name.dat
   Write #1, Name
   
   Write #1, icalc; model1; ipr; iout; novap; ig; ipareq
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

'lee número de componentes
    Set gruposram = ThisWorkbook.Sheets("gruposram")
    row_id = row_id + 1
    Ncomp = Cells(row_id, 2)
    Write #1, Ncomp
   Ngrup = Cells(9 + Ncomp * 2, 1)
   Write #1, Ngrup, Ngrup * Ngrup
   
'Escribe parámetros r y q
    cont = 12 + Ncomp * 2
    For i = 1 To Ngrup
        Name_group = Cells(cont, 1)
        k = 1
        ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
        Do While ngt <> "fin"
            If ngt = Name_group Then Exit Do
            k = k + 1
            ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
        Loop
        Write #1, (k - 2), CDbl(Cells(cont - 2 - (i - 1), i + 1)), CDbl(Cells(cont - 1 - (i - 1), i + 1)) 'grupo,r,q
        cont = cont + 1
    Next i
 
 'Escribe parámetros de interacción
    cont = 9 + Ncomp * 2
    For i = 1 To Ngrup
        For j = 1 To Ngrup
            Write #1, Number_of_group(Cells(cont + 2 + i, 1), ipareq), Number_of_group(Cells(cont, 1 + j), ipareq), CDbl(Cells(cont + 2 + i, 1 + j))
        Next j
    Next i
    
   
    row_id = row_id + 2
    For i = 1 To Ncomp

        j = 1
        Name_group = Cells(row_id, 2 + j).Value
        Do While Name_group <> "Choose"
     '       k = 1
     '       ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
     '       Do While ngt <> "fin"
     '           If ngt = Name_group Then Exit Do
     '           k = k + 1
     '           ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
     '       Loop
            Write #1, Number_of_group(Cells(row_id, 2 + j), ipareq), Cells(row_id + 1, 2 + j).Value,
            j = j + 1
            Name_group = Cells(row_id, 2 + j).Value
        Loop
       Write #1,
        row_id = row_id + 2
    Next i
    
'Condiciones flash
    row_id = 15 + Ncomp * 3 + Ngrup
    T = Cells(row_id, 2)
    P = Cells(row_id, 3)
    
    ReDim z(Ncomp - 1)
    For i = 0 To Ncomp - 1
        z(i) = Cells(row_id, 4 + i)
    Next i
    Write #1, T, P
    For i = 0 To Ncomp - 1
        Write #1, z(i),
    Next i
    Write #1,

   Write #1, 0, 0 'Flash.exe lee T y P. Si T==0, el programa termina
   
   
   
 
    Close #1

End Sub
Function Number_of_group(Name As String, ipareq As Integer) As Integer
    Dim k As Integer
    Dim ngt As String
    Dim gruposram As Worksheet
    
    
    Set gruposram = ThisWorkbook.Sheets("gruposram")
            k = 1
            ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
            Do While ngt <> "fin"
                If ngt = Name Then Exit Do
                k = k + 1
                ngt = gruposram.Cells(k, ipareq) 'lee nombre de grupo en gruposram
            Loop
            Number_of_group = k - 2

End Function



Sub Write_Output(path As String)
    
    Dim Name, line As String
    Dim Ncomp, nphases, i, k, col_id, row_id, Ngrup As Integer
    Dim datos() As String

    Close #1
'Abre archivo de salida generado en Fortran
    Name = "output.out"
    'MsgBox (CurDir())
    Open path + Name For Input As #1

'Lee numero de componentes
    Ncomp = Range("B6")
    nphases = CInt(leer_linea(1)) 'lee la primer línea del archivo (número de fases) y la convierte en entero
    Ngrup = Cells(9 + Ncomp * 2, 1)
    row_id = 15 + Ncomp * 3 + Ngrup
    col_id = 4 + Ncomp
    Range(Cells(row_id, col_id), Cells(row_id + 50, col_id + 50)).Select
    Selection.ClearContents
  '  If nphases = 1 Then
  '      Cells(row_id, col_id) = "System is stable"
  '      Exit Sub
  '  End If
    
'Escribe tabla Flash
    row_id = row_id - 2
    Range(Cells(row_id, col_id), Cells(row_id + 50, col_id + 50)).Select
    Selection.Delete Shift:=xlUp
    
    col_id = col_id + 2
    Cells(row_id + 1, col_id - 1) = "Comp."
    Call azul(row_id + 1, col_id - 1)
    Cells(row_id + 2, col_id - 1) = "x(i)"
    Call azul(row_id + 2, col_id - 1)
    Cells(row_id + 3, col_id - 1) = "Ln Gamma"
    Call azul(row_id + 3, col_id - 1)
    Cells(row_id + 4, col_id - 1) = "Gamma"
    Call azul(row_id + 4, col_id - 1)
    
    For i = 1 To nphases
    
        Cells(row_id, col_id) = "Phase " + CStr(i)
        Call azul(row_id, col_id)
        row_id = row_id + 1
        For k = 1 To Ncomp
            Cells(row_id, col_id + k - 1) = k
            Call azul(row_id, col_id + k - 1)
        Next k
            
        row_id = row_id + 1
            
        line = leer_linea(1) 'Composición
        datos() = Split(line, ";", , vbTextCompare)
        For k = 0 To Ncomp - 1
            Cells(row_id, col_id) = datos(k)
            Call blanco(row_id, col_id)
            col_id = col_id + 1
        Next k
            
        col_id = col_id - Ncomp
        row_id = row_id + 1
            
        line = leer_linea(1) 'Ln Gamma
        datos() = Split(line, ";", , vbTextCompare)
        For k = 0 To Ncomp - 1
            Cells(row_id, col_id) = datos(k)
            Call blanco(row_id, col_id)
            col_id = col_id + 1
        Next k
            
        col_id = col_id - Ncomp
        row_id = row_id + 1
            
        For k = 0 To Ncomp - 1
            Cells(row_id, col_id) = Exp(Cells(row_id - 1, col_id))
            Call blanco(row_id, col_id)
            col_id = col_id + 1
        Next k
            
        row_id = row_id - 4
        
    Next i
    
    
 '   Do While (Not EOF(1)) ' leer linea a linea hasta EOF y copiar cada linea a variable 'line'
 '       line = leer_linea(1)
 '       datos() = Split(line, ";", , vbTextCompare)
 '       For i = 0 To Ncomp - 1
 '           Cells(row_id, col_id) = datos(i)
 '           col_id = col_id + 1
 '       Next i

'    Loop
    
    
'    j = 1
'    Cells(row_id - 1, 3 + i) = "Phase 1"
'    Call azul(row_id - 1, 3 + i)
'    Do While j <= Ncomp 'composición fase 1
'        Cells(row_id, 3 + i) = "x" + CStr(j)
'        Call azul(row_id, 3 + i)
'        Call blanco(row_id + 1, 3 + i)
'        i = i + 1
'        j = j + 1
'    Loop

'    j = 1
'    Cells(row_id - 1, 3 + i) = "Phase 2"
'    Call azul(row_id - 1, 3 + i)
'    Do While j <= Ncomp 'composición fase 2
'        Cells(row_id, 3 + i) = "x" + CStr(j)
'        Call azul(row_id, 3 + i)
'        Call blanco(row_id + 1, 3 + i)
'        i = i + 1
'        j = j + 1
'    Loop

 '   j = 1
 '   Cells(row_id - 1, 3 + i) = "Phase 1"
 '   Call azul(row_id - 1, 3 + i)
 '   Do While j <= Ncomp 'composición fase 1
 '       Cells(row_id, 3 + i) = "G" + CStr(j)
 '       Call azul(row_id, 3 + i)
 '       Call blanco(row_id + 1, 3 + i)
 '       i = i + 1
 '       j = j + 1
 '   Loop

 '   j = 1
 '   Cells(row_id - 1, 3 + i) = "Phase 2"
 '   Call azul(row_id - 1, 3 + i)
 '   Do While j <= Ncomp 'composición fase 2
 '       Cells(row_id, 3 + i) = "G" + CStr(j)
 '       Call azul(row_id, 3 + i)
 '       Call blanco(row_id + 1, 3 + i)
 '       i = i + 1
 '       j = j + 1
 '   Loop
    


    Close #1

End Sub

Public Function leer_linea(file As Integer)
Dim line As String
' leer linea a linea hasta EOF y copiar cada linea a variable 'line'
    Line Input #file, line
    ' reemplzamos los espacios " " por nada ""
    line = LTrim(line)
    line = RTrim(line)
    line = Replace(line, ",", "")
    Do Until InStr(1, line, "  ") = 0
        line = Replace(line, "  ", " ")
        DoEvents
    Loop
    line = Replace(line, " ", ";")
    leer_linea = line
End Function

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Las siguientes subrutinas están dedicadas a ejecutar
' y esperar que finailce la ejecución del ejecutable
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub Command1_Click(Programa As String)
      
       Call Ejecutar_shell(Programa)
      
End Sub
      
      
    ' Recibe el argumento de la línea de comandos para pasarle al Shell
    Private Sub Ejecutar_shell(Programa As String)
      
        Dim handle_Process As Long
        Dim id_process As Long
        Dim lp_ExitCode As Long
          
        ' Abre el proceso con el shell
        id_process = Shell(Programa, 1)
          
        ' handle del proceso
        handle_Process = OpenProcess(PROCESS_QUERY_INFORMATION, False, id_process)
          
        ' Mientras lp_ExitCode = STATUS_PENDING, se ejecuta el do
        Do
      
            Call GetExitCodeProcess(handle_Process, lp_ExitCode)
              
            DoEvents
         
        Loop While lp_ExitCode = STATUS_PENDING
          
        ' fin
        ' Cierra
        Call CloseHandle(handle_Process)
      
        'MsgBox "Se cerró el " & Programa, vbInformation
      
    End Sub
      
      
    Private Sub Form_Load()
        Command1.Caption = " Ejecutar programa y esperar a que cierre "
    End Sub


