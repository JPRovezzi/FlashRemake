C  *******************************************************************  00000010
C  *                                                                 *  00000020
C  *           PROGRAM   L L E C A L A S  (asociacion incorporada    *  00000030
C  *                                      para el calculo de flash y *  00000000
C  *                                      curva binodal (icalc 0 y 1)*
C  *                                                                 *
C  *                          ASOCIACIÓN CRUZADA					   *		  
C  *                       VERSIÓN GENERALIZADA                      *        
C  *              FEBRERO 2006 MODIFICADA POR                        *
C  *                   ALFONSINA  ESTER ANDREATTA                    *        
C  *        BASADA EN LAS SIMPLLIFICACIONES DE LOS PAPERS:           *
c  *       Revisada en Octubre del 2007 en el chequeo de estabilidad *
C  *																   *   	
c  * Michelsen, et al. (Fluid Phase Equilibria, 180(2001)165-174 )   *		
C  * Tan, et al.  (Ind. Eng. Chem. Res, 2004,43,203-208).			   *	   	
C  *																   *	   	
C  *        Esto permitió  que todos los casos particulares          *         
c  *       de asociación se puedan simplificar a un único cálculo. 
c
c   Válido para un máximo número grupo asociativo de 12
c   Con la implementación en el cálculo de la fracción no asociada en el componente puro 
c   por  el metodo iterativo aquí implementado se permite que una molécula
c   tenga más de un grupo asociativo 14/07/06
C  El cálculo se limita a que el número máximo de sitios sea 2(por razones matemáticas)
c                                                       
C  *******************************************************************  00000050
C  *                                           DATE: 24/3 - 1982 /TJ *  00000060
      IMPLICIT REAL*8(A-H,O-Z)                                          00000070
      EXTERNAL STABIL,GMIX,FUNC                                         00000080
      COMMON/CVAP/NOVAP,NDUM,IDUM(4),PRAT(10)                           00000090
      COMMON/CGIBBS/NF,MAXZ,GNUL,Z(10),A(10),XVL(10,4),SFAS(4),GAM(10,1000000100
     *),AL(10),DA(10,10),XM(10,4)                                       00000110
      COMMON/CUFAC/N,NG,P(10,10),T                                      00000120
      COMMON/CY/Y13,Y21,STEP                                            00000130
      COMMON/CA/XC(5),GE(5,2),GC(5,2)                                   00000140
      COMMON/CIPR/IPR                                                   00000150
      COMMON/CQT/QT(10,10),Q(10),R(10)                                  00000160
      COMMON/CACT/Y1(10),Y2(10),ACT1(10),ACT2(10),DACT1(10,10),DACT2(10,00000170
     *10),PACT(2,2)                                                     00000180
      COMMON/CMODEL/MODEL                                               00000190
      COMMON/COUT/IOUT                                                  00000200
      common/nga/nga,mass(12)
      common/ig/ig
      DIMENSION DLX(10),YVAL(30),Y(10),GRAD(30),XMAT(30,30),WORK(30,5)  00000210
      DIMENSION NTEXT(36),X(2),ANT(10,3)                                00000220

c-----
      dimension xmj(10),actgam(10),agam(10,4),de(10,10),pe(2,2)
c-----
c      character*6 name
c      name='llecal'
c      call entrada(name)
      OPEN (UNIT=2,FILE='llecalas.DAT',status='OLD',FORM='FORMATTED')
      READ(2,501) NTEXT                                                 00000230
C     READ(2,503) ICALC,MODEL,IPR,IOUT,NOVAP                            00000240
      READ(2,*) ICALC,MODEL,IPR,IOUT,NOVAP,nga,ig
	IF (IOUT.EQ.1) OPEN (UNIT=1,FILE='lleasoccuzada.OUT',
     *                     FORM='FORMATTED')
      WRITE(6,608)                                                      00000250
      WRITE(6,628) IOUT                                                 00000260
      WRITE(6,610)                                                      00000270
      IF(IOUT.EQ.0) IOUT=6                                              00000280
      IF(ICALC.EQ.0) WRITE(6,620)                                       00000290
      IF(ICALC.EQ.1) WRITE(6,621)                                       00000300
      IF(ICALC.EQ.2) WRITE(6,622)                                       00000310
      IF(NOVAP.NE.0) WRITE(6,629)                                       00000320
      IF(MODEL.EQ.0) WRITE(6,624)                                       00000330
      IF(MODEL.EQ.1) WRITE(6,625)                                       00000340
      WRITE(6,623) NTEXT                                                00000350
      IF(IOUT.EQ.6) GOTO 5                                              00000360
      WRITE(IOUT,608)                                                   00000370
      WRITE(IOUT,610)                                                   00000380
      IF(ICALC.EQ.0) WRITE(IOUT,620)                                    00000390
      IF(ICALC.EQ.1) WRITE(IOUT,621)                                    00000400
      IF(ICALC.EQ.2) WRITE(IOUT,622)                                    00000410
      IF(NOVAP.NE.0) WRITE(IOUT,629)                                    00000420
      IF(MODEL.EQ.0) WRITE(IOUT,624)                                    00000430
      IF(MODEL.EQ.1) WRITE(IOUT,625)                                    00000440
      WRITE(IOUT,623) NTEXT                                             00000450
    5 CONTINUE                                                          00000460
      CALL PARIN2                                                       00000470
      IF(NOVAP.EQ.0) GOTO 9                                             00000480
      DO 6 J=1,N                                                        00000490
C   6 READ(2,502) (ANT(K,J),K=1,3)                                      00000500
    6 READ(2,*) (ANT(K,J),K=1,3)                                        00000500
      DO 7 J=1,N                                                        00000510
      ANT(1,J)=2.302585*(ANT(1,J)-2.880814)                             00000520
    7 ANT(2,J)=2.302585*ANT(2,J)                                        00000530
    9 CONTINUE                                                          00000540
      T1=0.                                                             00000550
      NN=0                                                              00000560
C  10 READ(2,502) T,PP                                                  00000570
   10 IF (ICALC.EQ.0) READ(2,*) T,PP                                   
      IF (ICALC.GE.1) READ(2,*) T
      IF(T.EQ.0.) GOTO 10000                                            00000580
      IF(PP.EQ.0..OR.NOVAP.EQ.0) GOTO 4                                 00000590
      DO 1 I=1,N                                                        00000600
    1 PRAT(I)=DLOG(PP)-ANT(1,I)+ANT(2,I)/(T-273.15+ANT(3,I))            00000610
C   4 READ(2,502) (Z(I),I=1,N)                                          00000620
    4 READ(2,*) (Z(I),I=1,N)                                            00000620
      ZSUM=0.                                                           00000630
      ZMAX=0.                                                           00000640
      DO 15 I=1,N                                                       00000650
      ZSUM=ZSUM+Z(I)                                                    00000660
      IF(Z(I).LT.ZMAX) GOTO 15                                          00000670
      ZMAX=Z(I)                                                         00000680
      MAXZ=I                                                            00000690
   15 CONTINUE                                                          00000700
      IF(T.EQ.T1) GOTO 30                                               00000710
      CALL PARAM2                                                       00000720
      IF(ICALC.NE.1) GOTO 16                                            00000730
      IF(N.NE.2.AND.N.NE.3) WRITE(6,616)                                00000740
      IF(IOUT.NE.6.AND.N.NE.2.AND.N.NE.3) WRITE(IOUT,616)               00000750
      Y13=Z(1)                                                          00000760
      Y21=Z(2)                                                          00000770
      WRITE(6,633) T                                                    00000780
      IF(IOUT.NE.6) WRITE(IOUT,633) T                                   00000790
      IF(N.EQ.3) GOTO 12                                                00000800
      CALL SOLBIN                                                       00000810
      GOTO 10000                                                        00000820
   12 STEP=Z(3)/100.D0                                                  00000830
      IF(STEP.EQ.0.) STEP=.02D0                                         00000840
      CALL BINOD                                                        00000850
      GOTO 10000                                                        00000860
   16 CONTINUE                                                          00000870
      IF(ICALC.NE.2) GOTO 19                                            00000880
      IF(N.NE.2) WRITE(6,616)                                           00000890
      IF(IOUT.NE.6.AND.N.NE.2) WRITE(IOUT,616)                          00000900
      XC(1)=0.                                                          00000910
      XC(2)=.2D0                                                        00000920
      XC(3)=.5D0                                                        00000930
      XC(4)=.8D0                                                        00000940
      XC(5)=1.D0                                                        00000950
      DO 17 K=1,5                                                       00000960
      Y(1)=XC(K)                                                        00000970
      Y(2)=1.D0-XC(K)                                                   00000980
      CALL UNIFA2(1,Y,ACT1,DACT1,PACT)                                  00000990
      GE(K,1)=ACT1(1)                                                   00001000
   17 GE(K,2)=ACT1(2)                                                   00001010
      READ(2,*) R(1),Q(1)                                               00001020
      READ(2,*) R(2),Q(2)                                               00001030
C     READ(2,502) R(1),Q(1)                                             00001020
C     READ(2,502) R(2),Q(2)                                             00001030
      WRITE(6,627)                                                      00001040
      DO 14 I=1,2                                                       00001050
   14 WRITE(6,626) I,R(I),Q(I)                                          00001060
      IF(IOUT.EQ.6) GOTO 13                                             00001070
      WRITE(IOUT,627)                                                   00001080
      DO 11 I=1,2                                                       00001090
   11 WRITE(IOUT,626) I,R(I),Q(I)                                       00001100
   13 CONTINUE                                                          00001110
      X(1)=Z(1)/300.D0                                                  00001120
      X(2)=Z(2)/300.D0                                                  00001130
      DO 18 I=1,2                                                       00001140
      DO 18 J=1,2                                                       00001150
      QT(I,J)=0.                                                        00001160
   18 P(I,J)=0.                                                         00001170
      QT(1,1)=Q(1)                                                      00001180
      QT(2,2)=Q(2)                                                      00001190
      NK=2                                                              00001200
      NG=2                                                              00001210
      XLAMB=1.                                                          00001220
      CALL MARQ(FUNC,2,10,X,XLAMB,3.D0,1.D-7,99)                        00001230
      WRITE(6,633) T                                                    00001240
      IF(IOUT.NE.6) WRITE(IOUT,633) T                                   00001250
      WRITE(6,617) P(1,2),P(2,1)                                        00001260
      IF(IPR.EQ.1) WRITE(6,618)                                         00001270
      DO 21 L=1,5                                                       00001280
      DO 21 I=1,2                                                       00001290
      GE(L,I)=DEXP(GE(L,I))                                             00001300
   21 GC(L,I)=DEXP(GC(L,I))                                             00001310
      IF(IPR.EQ.1) WRITE(6,619) ((GE(L,I),L=1,5),I=1,2)                 00001320
      IF(IPR.EQ.1) WRITE(6,619) ((GC(L,I),L=1,5),I=1,2)                 00001330
      IF(IOUT.EQ.6) GOTO 22                                             00001340
      WRITE(IOUT,617) P(1,2),P(2,1)                                     00001350
      IF(IPR.EQ.1) WRITE(IOUT,618)                                      00001360
      IF(IPR.EQ.1) WRITE(IOUT,619) ((GE(L,I),L=1,5),I=1,2)              00001370
      IF(IPR.EQ.1) WRITE(IOUT,619) ((GC(L,I),L=1,5),I=1,2)              00001380
   22 CONTINUE                                                          00001390
      GOTO 10000                                                        00001400
   19 CONTINUE                                                          00001410
      DO 20 I=1,N                                                       00001420
      DO 20 J=1,N                                                       00001430
      GAM(I,J)=0.D0                                                     00001440
      IF(J.EQ.I) GOTO 20                                                00001450
      CALL GAMINF(I,J,G)                                                00001460
      GAM(I,J)=G                                                        00001470
   20 CONTINUE                                                          00001480
   30 T1=T                                                              00001490
      NN=NN+1                                                           00001500
      WRITE(6,602) NN                                                   00001510
      DO 35 I=1,N                                                       00001520
   35 Z(I)=Z(I)/ZSUM                                                    00001530
      WRITE(6,605) T,PP,ZSUM,(Z(I),I=1,N)                               00001540
      IF(IOUT.NE.6) WRITE(IOUT,602) NN                                  00001550
      IF(IOUT.NE.6) WRITE(IOUT,605) T,PP,ZSUM,(Z(I),I=1,N)              00001560
      CALL UNIFA2(1,Z,AL,DA,PACT)                                       00001570
      SFAS(1)=1.                                                        00001580
      GNUL=0.                                                           00001590
      DO 40 I=1,N                                                       00001600
      XVL(I,1)=1.                                                       00001610
      Z(I)=Z(I)+1.D-20                                                  00001620
      DLX(I)=DLOG(Z(I))                                                 00001630
      A(I)=AL(I)+DLX(I)                                                 00001640
   40 GNUL=GNUL+Z(I)*AL(I)                                              00001650
      NF=1                                                              00001660
   50 CALL STIG(Y,S)                                                    00001670
      IF(S.GT.-1.D-7) GOTO 70                                           00001680
      WRITE(6,603)                                                      00001690
      IF(IOUT.NE.6) WRITE(IOUT,603)                                     00001700
      DO 60 I=1,N                                                       00001710
      YVAL(I)=1.D-5*Y(I)/Z(I)                                           00001720
   60 CONTINUE                                                          00001730
      GOTO 100                                                          00001740
   70 DO 75 I=1,N                                                       00001750
   75 YVAL(I)=DLOG(Y(I))                                                00001760
      XLAM=1.                                                           00001770
      IF(NF.EQ.1.AND.IPR.GT.0) WRITE(6,606)                             00001780
      IF(NF.GT.1.AND.IPR.GT.0) WRITE(6,609) NF                          00001790
      IF(IOUT.NE.6.AND.NF.EQ.1.AND.IPR.GT.0) WRITE(IOUT,606)            00001800
      IF(IOUT.NE.6.AND.NF.GT.1.AND.IPR.GT.0) WRITE(IOUT,609) NF         00001810
      CALL TMSSJ(30,N,IPR,15,XLAM,1.D-12,FUN,YVAL,GRAD,XMAT,WORK,1)     00001820
      IF(FUN.LT.-1.D-7) GOTO 80                                         00001830
      WRITE(6,604)                                                      00001840

	 write(7,46) T,  (xM(l,1),l=1,N)    !Alfonsina
	 write(7,46) T,  (xM(l,2),l=1,N)!Alfonsina
	 write(7,*)                    !Alfonsina



      IF(IOUT.NE.6) WRITE(IOUT,604)                                     00001850
      GOTO 10                                                           00001860
   80 WRITE(6,603)                                                      00001870
      IF(IOUT.NE.6) WRITE(IOUT,603)                                     00001880
      DO 90 I=1,N                                                       00001890
   90 YVAL(I)=1.D-5*DEXP(YVAL(I))/Z(I)                                  00001900
  100 NF=NF+1                                                           00001910
  104 DO 105 I=1,N                                                      00001920
      IF(YVAL(I).GT.1.D0) GOTO 106                                      00001930
  105 CONTINUE                                                          00001940
      GOTO 109                                                          00001950
  106 DO 107 I=1,N                                                      00001960
  107 YVAL(I)=YVAL(I)/10.                                               00001970
      GOTO 104                                                          00001980
  109 CONTINUE                                                          00001990
      SFAS(NF)=1.                                                       00002000
      XLAM=.2                                                           00002010
      IF(NF.EQ.2) XLAM=.5                                               00002020
      M=(NF-1)*N                                                        00002030
      IF(IPR.GT.0) WRITE(6,607) NF                                      00002040
      IF(IOUT.NE.6.AND.IPR.GT.0) WRITE(IOUT,607) NF                     00002050
      CALL TMSSJ(30,M,IPR,60,XLAM,1.D-16,FUN,YVAL,GRAD,XMAT,WORK,2)     00002060
      NT=NF*N                                                           00002070
      NB=NT-N                                                           00002080
      DO 110 I=1,NB                                                     00002090
  110 YVAL(NT+1-I)=YVAL(NB+1-I)                                         00002100
      WRITE(6,614) NF                                                   00002110
      NVAP=0                                                            00002120
      DO 111 J=1,NF                                                     00002130
      IF(IDUM(J).EQ.1) NVAP=J                                           00002140
  111 CONTINUE                                                          00002150
      IF(NVAP.EQ.0) WRITE(6,630)                                        00002160
      IF(NVAP.NE.0) WRITE(6,631) NVAP                                   00002170
      IF(IOUT.NE.6.AND.NVAP.EQ.0) WRITE(IOUT,630)                       00002180
      IF(IOUT.NE.6.AND.NVAP.NE.0) WRITE(IOUT,631) NVAP                  00002190
      WRITE(6,611) (J,SFAS(J),J=1,NF)                                   00002200
      WRITE(6,612) (J,J=1,NF)                                           00002210
      IF(IOUT.NE.6) WRITE(IOUT,614) NF                                  00002220
      IF(IOUT.NE.6) WRITE(IOUT,611)(J,SFAS(J),J=1,NF)                   00002230
      IF(IOUT.NE.6) WRITE(IOUT,612) (J,J=1,NF)                          00002240
      SUM=0.                                                            00002250
      DO 115 I=1,N                                                      00002260
      DLX(I)=XVL(I,NF)*Z(I)/SFAS(NF)                                    00002270
  115 SUM=SUM+DLX(I)                                                    00002280
      SUM=DLOG(SUM)                                                     00002290
      CALL UNIFA2(1,DLX,A,DA,PACT)                                      00002300
      DO 120 I=1,N                                                      00002310
      DLX(I)=DLOG(DLX(I))                                               00002320
  120 A(I)=A(I)+DLX(I)-SUM                                              00002330
c-----
      do 1130 j=1,nf
      do 1131 i=1,n
 1131 xmj(i)=xm(i,j)
      call unifa2(1,xmj,actgam,de,pe)
      do 1132 i=1,n
 1132 agam(i,j)=actgam(i)
 1130 continue
      DO 130 I=1,N                                                      00002340
      WRITE(6,613) I,(XM(I,J),J=1,NF)                                   00002350
  130 write(6,1613) i,(agam(i,j),j=1,nf)
      IF(IOUT.EQ.6) GOTO 132                                            00002360
      DO 131 I=1,N                                                      00002370
      WRITE(IOUT,613) I,(XM(I,J),J=1,NF)    
  131 write(iout,1613) i,(agam(i,j),j=1,nf)
   46	FORMAT (2X,F12.2, 8X,F12.6, 8X,F12.6 , 8X,F12.6, 8X,F12.6,  !Alfonsina                                                 
     @8X,F12.6,8X,F12.6,8X,F12.6,8X,F12.6,8X,F12.6,8X,F12.6,8X,F12.6)
c-----
  132 CONTINUE                                                          00002390
      GOTO 50                                                           00002400


  501 FORMAT(36A2)                                                      00002410
  502 FORMAT(8F10.2)                                                    00002420
  503 FORMAT(20I3)                                                      00002430
  602 FORMAT(///,' * * * FLASH NUMBER',I3,' * * *',//)                  00002440
  603 FORMAT(/,' SYSTEM IS UNSTABLE, PHASE SPLIT PERFORMED')            00002450
  604 FORMAT(/,' * SYSTEM IS STABLE *',/)                               00002460
  605 FORMAT(' TEMPERATURE =',F10.4,' K, PRESSURE =',F7.3,' ATM, FEED ='00002470
     *,F10.2,' MOLES',/,' FEED COMPOSITION (MOLE PERCENT):',/,1X,15(2PF700002480
     *.3))                                                              00002490
  606 FORMAT(//,' DIFFERENTIAL STABILITY TEST FOR FEED MIXTURE:')       00002500
  607 FORMAT(/,' PHASE SPLIT CALCULATION,',I2,' PHASES:')               00002510
  608 FORMAT(1H1)                                                       00002520
  609 FORMAT(//,' DIFFERENTIAL STABILITY TEST FOR',I2,'-PHASE SYSTEM')  00002530
  610 FORMAT(///)                                                       00002540
  611 FORMAT(/,'  PHASE FRACTIONS (PERCENT):',4(5X,I3,2PF7.3,5X))       00002550
  612 FORMAT(/,'  COMPOSITION  ',10X,4(8X,I3,9X))                       00002560
  613 FORMAT('   X(',I2,')            ',5(8X,F12.8))                    00002570
c-----
 1613 format('  ln(G',i2,')            ',5(8x,f12.8))

c-----
  614 FORMAT(//,' RESULT OF',I2,'-PHASE CALCULATION:')                  00002580
  616 FORMAT(//,' * WRONG INPUT SPECIFICATION *',//)                    00002590
  617 FORMAT(///,' ** UNIQUAC PARAMETERS FROM UNIFAC **',//,5X,'A12/R = 00002600
     * ',F12.3,' K ,  A21/R = ',F12.3,' K',///)                         00002610
  618 FORMAT(//,' ** COMPARISON OF ACTIVITIES CALCULATED BY UNIFAC AND U00002620
     *NIQUAC, RESPECTIVELY **'//)                                       00002630
  619 FORMAT(10F12.5)                                                   00002640
  620 FORMAT(' **** FLASH CALCULATION ****')                            00002650
  621 FORMAT(' **** BINODAL CURVE CALCULATION ****',//)                 00002660
  622 FORMAT(' **** CALCULATION OF UNIQUAC PARAMETERS FROM UNIFAC **** '00002670
     *,//)                                                              00002680
  623 FORMAT(1X,'COMPONENTS : ',40A2,//)                                00002690
  624 FORMAT(' MODEL USED FOR LIQUID PHASE NON-IDEALITY: UNIFAC'//)     00002700
  625 FORMAT(' MODEL USED FOR LIQUID PHASE NON-IDEALITY: UNIQUAC'//)    00002710
  626 FORMAT(I5,2F15.4)                                                 00002720
  627 FORMAT(//,' SPECIFIED UNIQUAC R AND Q',/)                         00002730
  628 FORMAT(/,' IOUT = ',I2,/' IF IOUT = 0: OUTPUT ONLY ON UNIT 6',/,  00002740
     *' IF IOUT = 1: OUTPUT ON BOTH UNIT 6 AND 1')                      00002750
  629 FORMAT(/,' VAPOR PHASE INCLUDED IN FLASH-CALCULATIONS',//)        00002760
  630 FORMAT(' NO VAPOR PHASE')                                         00002770
  631 FORMAT(' PHASE',I2,' IS A VAPOR PHASE')                           00002780
  633 FORMAT(///,'   TEMPERATURE =',F10.2,' DEG K')                     00002790
10000 CLOSE (UNIT=2)
      IF (IOUT.EQ.1) CLOSE (UNIT=1)
c      call salida(name)
      STOP                                                              00002800
      END                                                               00002810
      SUBROUTINE UNIFA2(NDIF,X,ACT,DACT,PACT)                           00002820
      IMPLICIT REAL*8(A-H,O-Z)                                          00002830
c------
      common/asoc/nktt,igamt(20,12),nytt(20,12)   
      common/nga/nga,mass(12)
      common/grupas1/rkass(6,12,6,12),enass(6,12,6,12),deloh(6,12,6,12)!Alfonsin
c------
      COMMON/CVAP/NOVAP,NDUM,IDUM(4),PRAT(10)                           00002840
      COMMON/CUFAC/NK,NG,P(10,10),T                                     00002850
      COMMON/CPAR/TAU(10,10),S(10,10),F(10)                             00002860
      COMMON/CQT/QT(10,10),Q(10),R(10)                                  00002870
      DIMENSION X(10),GAM(10),ACT(10),DACT(10,10),THETA(10),PHI(10)     00002880
     *,RI(10),QI(10),ETA(10),QIL(10),RIL(10),QID(10),ETAL(10),TETAR(10) 00002890
      DIMENSION U(10,10),V(10,10),PACT(2,2),DTAU(2,2,2)                 00002900
c------
      dimension goh(10),xgamk(20),dxohdx(10),dxxdx(10,10),
     @dasdx1(10,10),dasdx2(10,10),dasdx(10,10)
	common/ioh2/rngoh(12,12)

      dimension dif(12,12), dif1(10,12,12) !Alfonsina
      common/zzzas/xoh(6,12),xohi0(12,6,12),xoh_old(6,12),xohi(6,12),
     @xohi_old(6,12), xohi0_old(12,6,12)  !Alfonsina
	dimension m_lambda(nga*2,nga*2),m_lambda1(nga*2,nga*2) !Alfonsina
	dimension psin(12) !Alfonsina
	dimension indx(20)
      double precision  m_lambda,m_lambda1,xoh,xohi0,xoh_old,xohi0_old  !Alfon
	double precision del, del1, dif, dif1, d1,psin, xgam, xnoh1 !Alfonsina
	integer order !Alfonsina
      double precision sum1, sum2, sum3, sum4, SUMA1J, sumaj !Alfonsina
      dimension xnohi0(12,12),tgt(12),dnohdx(12,12),actas(12) !Alfonsina
	dimension xnoh1(12), xnoh(12),das1(3),
     *das3(3),dxkdni(12,6,12), dxkdnic(12,6,12), dgasdx(12)  !Alfonsina
      dimension dgasdxij (12,12), drhodx(12), drhodni(12,6,12)
	

c------
      dk=1.381e-23
      deloh=0.0
      xnoh=0.0
      xnoh1=0.0
	xoh=0.0
      xgam=0.0
      do 7777 i=1,10
	xohi0=0
      xnohi0=0.0
      tgt(i)=0.0
 7777 continue

      THETS=0.                                                          00002910
      PHS=0.                                                            00002920
      DO 10 I=1,NK                                                      00002930
      THETA(I)=X(I)*Q(I)                                                00002940
      PHI(I)=R(I)*X(I)                                                  00002950
      THETS=THETS+THETA(I)                                              00002960
   10 PHS=PHS+PHI(I)                                                    00002970
      DO 20 I=1,NK                                                      00002980
      RI(I)=R(I)/PHS                                                    00002990
      RIL(I)=DLOG(RI(I))                                                00003000
      QI(I)=Q(I)/THETS                                                  00003010
   20 QIL(I)=DLOG(QI(I))                                                00003020
c------
      do 33 i=1,nk
      goh(i)=0.
      tgt(i)=0.0
      xnohi0=0.0
      xgam=0.0
     
CCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      do j=1,nk
		tgt(j)=0.0
		xgam=0.0
	end do
   33 continue
      do i=1,nk
	if(nga.gt.0) then
	
		do k=1,nktt
			tgt(i)=tgt(i)+nytt(k,i)
		end do

		do j=1,nga  
			xnohi0(i,j)=rngoh(i,j)/R(i)  
		end do
		xgam=xgam+R(i)*x(i)
		end if  
	end do
  
      xnoh1=0d0
	do ja=1,nga
	    do i=1,nk
		xnoh1(ja)=xnoh1(ja)+rngoh(i,ja)*x(i)
	  end do
      end do
	
      do ja=1,nga
		xnoh(ja)=xnoh1(ja)/xgam
	end do
CCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c------
      DO 40 I=1,NG                                                      00003030
      ETA(I)=0.                                                         00003040
      DO 45 J=1,NK                                                      00003050
   45 ETA(I)=ETA(I)+S(I,J)*X(J)                                         00003060
   40 ETAL(I)=DLOG(ETA(I))                                              00003070
      DO 55 I=1,NG                                                      00003080
      TETAR(I)=0.                                                       00003090
      DO 55 J=1,NK                                                      00003100
   55 TETAR(I)=TETAR(I)+QT(I,J)*X(J)                                    00003110
      DO 60 I=1,NK                                                      00003120
      QID(I)=1.-RI(I)/QI(I)                                             00003130
      XX=F(I)+Q(I)*(1.-QIL(I))-RI(I)+RIL(I)                             00003140
      XX=XX-5.*Q(I)*(QID(I)+RIL(I)-QIL(I))                              00003150
      ACT(I)=XX                                                         00003160
      DO 661 J=1,NG                                                     00003170
      U(J,I)=S(J,I)/ETA(J)                                              00003180
      V(J,I)=U(J,I)*TETAR(J)                                            00003190
  661 ACT(I)=ACT(I)-V(J,I)-QT(J,I)*ETAL(J)                              00003200
c------
  
  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	!**************************calculo de las fuerzas de asociacion*******************
      if(nga.ne.0) then
	  DO J=1,NGA 
              IF(MASS(J).EQ.0) GO TO 201
                DO m=1,NGA
                  IF(MASS(m).EQ.0) GO TO 101
                        DO L=1,MASS(J)
                                DO K=1,MASS(m)
                                  IF(ENASS(K,m,L,J).EQ.0) THEN
                                         CONTINUE
                                    ELSE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      deloh(k,m,l,j)=(DEXP(ENASS(K,m,L,J)/T) - 1 )*RKASS(K,m,L,J)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                  END IF
                                END DO
                        END DO

 101            CONTINUE
                END DO
 201          CONTINUE
        END DO
  

	end if  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	!***********************************calculo de xoh**************************
c Cálculo de la  fracción no asociada Paper:Ind. Eng. Chem. Res, 2004,43,203-208   
c !Inicializació
      if(nga.ne.0) then
      xoh=1.0d0 
      del=1.d0
c Iteraciones con tolerancia de 10-9
      do while (del>1.0d-10)
      xoh_old=xoh
	do m=1, nga
		do j=1,2
      sum1=0.D0
	do k=1, nga
	sum2=0.D0
	do l=1,2

	sum2=sum2+xoh_old(l,k)*deloh(l,k,j,m)
	end do
	sum1=sum1+sum2*xnoh1(k)
      end do
      xoh(j,m)=1.D0/(1.D0+sum1/xgam)          
	dif(j,m)=dabs((xoh(j,m)-xoh_old(j,m))/xoh(j,m))
	end do
	end do
	del=maxval(dif)
      end do
      end if

			write(4,*)"T=", t           
	write(4,*)"xoh (1,1)=", xoh (1,1)  
	write(4,*)"xoh (1,2)=", xoh(2,1) 
 
	 

cc Fin del Cálculo de la  fracción no asociada 
c	!*****************************calculo de xohi0**************************************
C	xohi0=1d0
c	do i=1, nc
C		do j=1, nga
C	       do l=1,mass(j)
C	do k=1,nga
C	 do m=1,mass(k)
C			If (rngoh(i,j).eq.0d0) then
C					xohi0(i,l,j)=1.d0
C			elseif (deloh(l,j,m,k).gt.0d0.and.rngoh(i,j).ne.0d0.and.
C     @		mass(j).eq.2)then
C		xohi0(i,l,j)=(-1d0+dsqrt(1d0+4d0*xnohi0(i,j)*deloh(l,j,m,k)))/
C     @			(2d0*xnohi0(i,j)*deloh(l,j,m,k))
C	
C			end if
C		 end do
C	end do
C	end do
C		end do
c	end do
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c****************************calculo de xohi0(esta implementación permite que una molécula
c tenga más de un grupo asociativo 14/07/06**************************************
c !Inicializació
     
      if(nga.ne.0) then
      xohi0=1.D0
      del1=1.D0
c Iteraciones con tolerancia de 10-12
      do while (del1>1.0d-10)
      xohi0_old=xohi0
	do m=1, nga
	if	(rngoh(i,m).gt.0d0) then
		do j=1,2
      sum3=0.D0
	do k=1, nga
	sum4=0.D0
	do l=1,2 
	sum4=sum4+ xohi0_old(i,l,k)*deloh(l,k,j,m)*xnohi0(i,k)
	end do
	sum3=sum3+sum4
      end do
      xohi0(i,j,m)=1.D0/(1.D0+sum3)    
 	dif1(i,j,m)=dabs((xohi0(i,j,m)-xohi0_old(i,j,m))/xohi0(i,j,m))
	end do
	else
	end if
	end do
	del1=maxval(dif1)
      end do
      end if

c*****************************fin del calculo de xohi0**************************************
Cálculo del gama de asociación ALFONSINA CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C actas(M) = LOGARITMO NATURAL DEL GAMA DE ASOCIACIÓN DEL COMPONENTE I  

       if(nga.ne.0) then	

      SUMAJ = 0.D0
      DO J=1,NGA 
      IF(MASS(J).NE.0) THEN      
      DO K=1,MASS(J)
	If(XOH(K,J).gt.1d-13)then
      SUMAJ = SUMAJ + RNGOH(i,j)*(dlog(XOH(K,J)/XOHI0(I,K,J))+
     @0.5D0*(XOHi0(i,K,J)-1))+0.5D0*R(i)*xnoh(j)*(1-xoh(k,j))
	end if
      END DO
      ELSE
      CONTINUE
      END IF
      END DO
      actas(I) = SUMAJ
	end if
      act(i)=act(i)+actas(i)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   60 continue
      NDUM=0                                                            00003210
      IF(NOVAP.EQ.0) GOTO 69                                            00003220
      SS=0                                                              00003230
      DO 61 I=1,NK                                                      00003240
   61 SS=SS+X(I)*(PRAT(I)-ACT(I))                                       00003250
      IF(SS.GT.0.) GOTO 69                                              00003260
      NDUM=1                                                            00003270
      DO 62 I=1,NK                                                      00003280
      ACT(I)=PRAT(I)                                                    00003290
      DO 62 J=1,NK                                                      00003300
   62 DACT(I,J)=0.                                                      00003310
      GOTO 100                                                          00003320
   69 CONTINUE                                                          00003330
      IF(NDIF.EQ.4) GOTO 90                                             00003340
      IF(NDIF.LT.2) GOTO 100                                            00003350
      DO 70 I=1,NK                                                      00003360
      DO 70 J=I,NK                                                      00003370
      XX=Q(I)*QI(J)*(1.-5.*QID(I)*QID(J))+(1.-RI(I))*(1.-RI(J))         00003380
      DO 75 K=1,NG                                                      00003390
   75 XX=XX+U(K,I)*(V(K,J)-QT(K,J))-U(K,J)*QT(K,I)                      00003400

!********************************calculo de dxkdni Alfonsina**************************************

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINA CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cCalcula los elementos de la matriz deltapq para el cálculo de la derivada de la fracción 
c no asociada respecto a la fracción molar del componente
      psin=0.0d0
	if(nga.ne.0) then
	m_lambda1=0.0d0
	m_lambda=0.0d0
      z=0; y=0
	do n=1,2
	do m=1,nga
     	z=z+1
	do l=1, 2
	do k=1, nga
		y=y+1
      m_lambda(z,y)=xnoh(k)*deloh(l,k,n,m)*xoh(n,m)**2 
	   if (z.eq.y)  then
      m_lambda(z,y)=m_lambda(z,y)+ 1.0d0
      end if
	end do
	end do
	y=0
	end do
	end do
      order=nga*2
	end if 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINA CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

c Calculo de  los elementos de la matriz [Yp] para el cálculo de la derivada de la fracción 
c no asociada respecto a la fracción molar del componente
      if(nga.ne.0) then
      do k=1,nga

	do ll=1,2
	do m=1,nga
	drhodni(j,ll,m)=(((rngoh(j,m)*xgam-xnoh1(m)*R(j)))/xgam**2)	  
	end do
	end do

      z=0     
	do ll=1,2
	do m=1,nga
	sum3=0.0d0
      do l=1,nga
	sum4=0.0d0


	do kk=1,2
	sum4=sum4+ (xoh(kk,l)*deloh(kk,l,ll,m))*drhodni(j,kk,l)
	end do
	sum3=sum3+sum4
	end do
	z=z+1
	psin(z)=-(xoh(ll,m)**2)*sum3
	end do
	end do


      N=order
	NP=order
      m_lambda1=m_lambda
      call  ludcmp(m_lambda1,N,NP,indx,d1)
       call lubksb(m_lambda1,N,NP,indx,psin)
c colectando las derivadas en su correspondiente subíndice
      z=0
	do m=1,2
	do l=1, nga
	z= z+1
	dxkdni(k,m,l)=psin(z)
	end do
	end do 
	end do


	do l=1,nga
	do m=1,2
	do kk=1,nga
	if (rngoh(i,kk).ne.0) then
      dxkdnic(j,m,l)=dxkdni(kk,m,l)   
	end if
	end do
      end do
	end do

c fin del cálculo de la derivada de la fracción no asociada respecto a la 
c fracción molar del componente
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C dgasdx(M) = derivada LOGARITMO NATURAL DEL GAMA DE ASOCIACIÓN DEL COMPONENTE I
      if(nga.ne.0) then
   
      DO l=1,NGA 
	   SUMA1J = 0.D0

      IF(MASS(l).NE.0) THEN      
      DO K=1,MASS(l)
	If(XOH(K,l).gt.1d-13)then

      SUMA1J = SUMA1J + RNGOH(i,l)*1.D0/XOH(K,l)*dxkdnic(j,k,l)+0.5D0*
     @r(i)*(drhodni(j,k,l)-xnoh(l)*dxkdnic(j,k,l)-drhodni(j,k,l)*
     @XOH(K,l))


	end if
      END DO
      ELSE
      CONTINUE
      END IF
	xx=xx+ SUMA1J
 	end do   
	end if
		end if
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCALFONSINACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DACT(I,J)=XX                                                      00003410
   70 DACT(J,I)=XX    
       IF(NDIF.LT.3) GOTO 100                                           00003430
      DO 80 I=1,NK                                                      00003440
      GAM(I)=DEXP(ACT(I))                                               00003450
   80 ACT(I)=GAM(I)*X(I)                                                00003460
      DO 85 I=1,NK                                                      00003470
      DO 85 J=1,NK                                                      00003480
      DACT(I,J)=ACT(I)*(DACT(I,J)-1.D0)                                 00003490
      IF(J.EQ.I)DACT(I,J)=DACT(I,J)+GAM(I)                              00003500
   85 CONTINUE                                                          00003510
      GOTO 100                                                          00003520
   90 CONTINUE                                                          00003530
      DO 91 I=1,2                                                       00003540
      DO 91 K=1,2                                                       00003550
      DTAU(I,K,K)=0.                                                    00003560
      DO 91 L=1,2                                                       00003570
      IF(L.EQ.K) GOTO 91                                                00003580
      H1=TETAR(L)-QT(L,I)*ETA(L)/S(L,I)                                 00003590
      H2=QT(K,I)-S(L,I)*TETAR(K)/ETA(L)                                 00003600
      DTAU(I,K,L)=-H1*H2/ETA(L)                                         00003610
   91 CONTINUE                                                          00003620
      DO 92 I=1,NK                                                      00003630
      PACT(I,1)=-DTAU(I,1,2)*TAU(1,2)/T*300.D0                          00003640
   92 PACT(I,2)=-DTAU(I,2,1)*TAU(2,1)/T*300.D0                          00003650
  100 RETURN                                                            00003660
      END  
	
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   00003670
      SUBROUTINE PARIN2                                                 00003680
      IMPLICIT REAL*8(A-H,O-Z)                                          00003690
c------
      common/asoc/nktt,igamt(20,12),nytt(20,12)  !,p4,p5
      common/grupas1/rkass(6,12,6,12),enass(6,12,6,12), deloh(6,12,6,12)!Alfon
      common/nga/nga,mass(12) 
		common/ioh2/rngoh(12,12)

	common/ioh2sis/rngoht(3,2)
c------
      COMMON/CUFAC/NK,NG,P(10,10),T                                     00003700
      COMMON/CQT/QT(10,10),Q(10),R(10)                                  00003710
      COMMON/CMODEL/MODEL                                               00003720
      COMMON/COUT/IOUT                                                  00003730
      DIMENSION RT(10,10),A(32,32),NGM(10),MAINSG(57)                   00003740
      DIMENSION MS(10,10,2),NY(10,20),JH(57),IH(20)                     00003750
      REAL*4 RR(57),QQ(57)                                              00003760
      REAL*4    A1(32),A2(32),A3(32),A4(32),A5(32),A6(32),A7(32),A8(32),00003770
     *A9(32),A10(32),A11(32),A12(32),A13(32),A14(32),A15(32),A16(32),A1700003780
     *(32),A18(32),A19(32),A20(32),A21(32),A22(32),A23(32),A24(32),A25(300003790
     *2),A26(32),A27(32),A28(32),A29(32),A30(32),A31(32),A32(32)        00003800
      DATA MAINSG/4*1,4*2,2*3,3*4,5,6,7,8,9,2*10,11,12,2*13,2*14,4*15,3*00003810
     *16,3*17,2*18,19,20,2*21,22,3*23,24,25,26,3*27,28,29,30,31,32/     00003820
      DATA RR/.9011,.6744,.4469,.2195,1.3454,1.1167,.8886,1.1173,.5313,.00003830
     *3652,1.2663,1.0396,.8121,1.,3.2499,3.2491,.92,.8952,1.6724,1.4457,00003840
     *.998,3.168,1.3013,1.528,1.9031,1.6764,1.145,.9183,.6908,.9183,1.4600003850
     *54,1.238,1.006,2.2564,2.0606,1.8016,2.87,2.6401,3.39,1.1562,1.870100003860
     *,1.6434,1.06,2.0086,1.7818,1.5544,1.4199,2.4088,4.0013,2.9993,2.8300003870
     *32,2.667,3.3092,2.4317,3.0856,4.0358,2.8266/                      00003880
      DATA QQ/.848,.54,.228,0.,1.176,.867,.676,.988,.4,.12,.968,.66,.34800003890
     *,1.2,3.128,3.124,1.4,.68,1.488,1.18,.948,2.484,1.224,1.532,1.728,100003900
     *.42,1.088,.78,.468,1.1,1.264,.952,.724,1.988,1.684,1.448,2.41,2.1800003910
     *4,2.91,.844,1.724,1.416,.816,1.868,1.56,1.248,1.104,2.248,3.568,2.00003920
     *113,1.833,1.553,2.86,2.192,2.736,3.2,2.472/                       00003930
      DATA A1/0.,292.3,156.5,104.4,328.2,-136.7,-131.9,342.4,-159.8,66.500003940
     *6,146.1,14.78,1744.,-320.1,1571.,73.8,27.9,21.23,89.97,-59.06,29.000003950
     *8,175.8,94.34,193.6,108.5,81.49,-128.8,147.3,-11.91,14.91,67.84,3600003960
     *.42/                                                              00003970
      DATA A2/74.54,0.,-94.78,-269.7,470.7,-135.7,-135.7,220.6,1.,306.1,00003980
     *517.,1.,-48.52,485.6,76.44,-24.36,-52.71,-185.1,-293.7,1.,34.78,1.00003990
     *,375.4,5*1.,176.7,132.1,42.73,60.82/                              00004000
      DATA A3/-114.8,340.7,0.,-146.8,-9.21,-223.,-252.,372.8,-473.2,-78.00004010
     *31,-75.3,-10.44,75.49,114.8,52.13,4.68,1.,288.5,-4.7,777.8,56.41,-00004020
     *218.9,113.6,7.18,247.3,-50.71,-255.3,1.,-80.48,-17.78,59.16,29.77/00004030
      DATA A4/-115.7,4102.,167.,0.,1.27,-162.6,-273.6,203.7,-470.4,-73.800004040
     *7,223.2,-184.9,147.3,-170.,65.69,122.9,1.,33.61,134.7,-47.13,-53.200004050
     *9,-15.41,-97.05,-127.1,453.4,-30.28,-124.6,3*1.,26.59,55.97      /00004060
      DATA A5/644.6,724.4,703.9,4000.,0.,-281.1,-268.8,-122.4,-63.15,21600004070
     *.,-431.3,444.7,118.4,180.6,137.1,455.1,669.2,418.4,713.5,1989.,20100004080
     *1.,529.,483.8,332.6,-289.3,-99.56,-319.2,837.9,4*1.              /00004090
      DATA A6/329.6,1731.,511.5,136.6,937.3,2*0.,247.,-547.,401.7,643.4,00004100
     *-94.64,728.7,-76.64,-218.1,351.5,-186.1,-465.7,-260.3,3*1.,264.7,900004110
     **1./                                                              00004120
      DATA A7/310.7,1731.,577.3,906.8,991.3,2*0.,104.9,-547.2,-127.6,23100004130
     *.4,732.3,349.1,-152.8,-218.1,351.5,-401.6,-465.7,512.2,3*1.,264.7,00004140
     *9*1./                                                             00004150
      DATA A8/1300.,896.,859.4,5695.,28.73,-61.29,5.89,0.,-595.9,634.8,600004160
     *23.7,211.6,652.3,385.9,212.8,770.,740.4,793.2,1205.,390.7,63.48,-200004170
     *39.8,13.32,439.9,-424.3,1.,203.,1153.,-311.,-262.6,1.11,1.       /00004180
      DATA A9/2255.,1.,1649.,292.6,-195.5,-153.2,-153.2,344.5,0.,-568.,300004190
     **1.,-337.3,4*1.,1616.,2*1.,-860.3,1.,-230.4,523.,1.,-222.7,5*1.  /00004200
      DATA A10/472.6,343.7,593.7,916.7,67.07,-47.41,353.8,-171.8,-825.7,00004210
     *0.,128.,48.93,-101.3,58.84,52.38,483.9,550.6,342.2,550.,190.5,-34900004220
     *.2,857.7,377.,211.6,82.77,2*1.,417.4,4*1.          /              00004230
      DATA A11/158.1,-214.7,362.3,1218.,1409.,-344.1,-338.6,-349.9,1.,-300004240
     *7.36,0.,-311.6,1051.,1090.,1.,-47.51,16*1./                       00004250
      DATA A12/383.,1.,31.14,715.6,-140.3,299.3,-241.8,66.95,1.,120.3,1700004260
     *24.,0.,-115.7,-46.13,2*1.,808.8,203.1,70.14,5*1.,-75.23,1.,-201.9,00004270
     *123.2,1.,-281.9,2*1./                                             00004280
      DATA A13/139.4,1647.,461.8,339.1,-104.,244.4,-57.98,-465.7,1.,124700004290
     *.,.75,1919.,0.,1417.,1402.,337.1,437.7,370.4,438.1,1349.,1.,681.4,00004300
     *152.4,1.,-1707.,2*1.,639.7,4*1.          /                        00004310
      DATA A14/972.4,-577.5,6.,5688.,195.6,19.57,487.1,-6.32,-898.3,258.00004320
     *70,-245.8,57.7,-117.6,0.,461.3,1.,-132.9,176.5,129.5,-246.3,2.41,300004330
     **1.,29.86,7*1.                             /                      00004340
      DATA A15/662.1,289.3,32.14,213.1,262.5,1970.,1970.,64.42,1.,5.202,00004350
     *2*1.,-96.62,-235.7,0.,225.4,-197.7,-20.93,113.9,3*1.,-94.49,9*1. /00004360
      DATA A16/42.14,99.61,-18.81,-114.1,62.05,-166.4,-166.4,315.9,1.,1000004370
     *00.,751.8,1.,19.77,1.,301.1,0.,-21.35,-157.1,11.8,13*1.       /   00004380
      DATA A17/-243.9,337.1,2*1.,272.2,128.6,507.8,370.7,1.,-301.,1.,-3400004390
     *7.9,1670.,108.9,137.8,110.5,0.,1.,17.97,13*1.         /           00004400
      DATA A18/7.5,4583.,-231.9,-12.14,-61.57,2*1544.,356.8,1.,12.01,1.,00004410
     *-249.3,48.15,-209.7,-154.3,249.2,1.,0.,51.9,1.,-15.62,-216.3,4*1.,00004420
     *-114.7,5*1. /                                                     00004430
      DATA A19/-5.55,5831.,3000.,-141.3,-41.75,224.6,-207.,502.9,4894.,-00004440
     *10.88,1.,61.59,43.83,54.57,47.67,62.42,56.33,-30.1,0.,-255.4,-54.800004450
     *6,8455.,-34.68,514.6,8*1. /                                       00004460
      DATA A20/924.8,1.,-878.1,-107.3,-597.1,2*1.,-97.27,1.,902.6,2*1.,800004470
     *74.3,629.,4*1.,475.8,0.,-465.2,1.,794.4,1.,-241.7,1.,-906.5,5*1. /00004480
      DATA A21/696.8,405.9,29.13,1208.,-189.3,2*1.,198.3,1.,430.6,3*1.,-00004490
     *149.2,3*1.,70.04,492.,346.2,0.,5*1.,-169.7,5*1.        /          00004500
      DATA A22/902.2,1.,1.64,689.6,-348.2,1.,1.,-109.8,-851.6,1010.,2*1.00004510
     *,942.2,4*1.,-75.5,1302.,2*1.,0.,1.,175.8,164.4,1.,-944.9,5*1./    00004520
      DATA A23/556.7,425.7,-1.77,3629.,-30.7,150.8,150.8,1538.6,1.,400.,00004530
     *2*1.,446.3,1.,95.18,3*1.,490.9,-154.5,2*1.,0.,1.,481.3,7*1.      /00004540
      DATA A24/575.7,1.,-11.19,-175.6,-159.,2*1.,32.92,-16.13,-328.6,8*100004550
     *.,534.7,2*1.,179.9,1.,0.,-246.,7*1.           /                   00004560
      DATA A25/527.5,1.,358.9,337.7,536.6,2*1.,-269.2,-538.6,211.6,1.,-200004570
     *78.2,572.7,343.1,5*1.,124.8,1.,125.3,139.8,963.,0.,7*1.    /      00004580
      DATA A26/269.2,1.,363.5,1023.,53.37,20*1.,0.,6*1.          /      00004590
      DATA A27/-300.,1.,-578.2,-390.7,183.3,2*1.,-873.6,-637.3,2*1.,-20800004600
     *.4,5*1.,18.98,1.,-387.7,134.3,924.5,4*1.,0.,5*1.     /            00004610
      DATA A28/-63.6,3*1.,-44.44,2*1.,1429.,1.,148.,1.,-13.91,-2.16,14*100004620
     *.,0.,4*1./                                                        00004630
      DATA A29/928.3,500.7,364.2,4*1.,-364.2,20*1.,0.,3*1.    /         00004640
      DATA A30/331.,115.4,-58.1,4*1.,-117.4,3*1.,173.8,17*1.,0.,2*1.   /00004650
      DATA A31/561.4,784.4,21.97,238.,3*1.,18.41,22*1.,0.,1.       /    00004660
      DATA A32/956.5,265.4,84.16,132.2,27*1.,0./                        00004670
      DO 5 I=1,32                                                       00004680
      A(I,1)=A1(I)                                                      00004690
      A(I,2)=A2(I)                                                      00004700
      A(I,3)=A3(I)                                                      00004710
      A(I,4)=A4(I)                                                      00004720
      A(I,5)=A5(I)                                                      00004730
      A(I,6)=A6(I)                                                      00004740
      A(I,7)=A7(I)                                                      00004750
      A(I,8)=A8(I)                                                      00004760
      A(I,9)=A9(I)                                                      00004770
      A(I,10)=A10(I)                                                    00004780
      A(I,11)=A11(I)                                                    00004790
      A(I,12)=A12(I)                                                    00004800
      A(I,13)=A13(I)                                                    00004810
      A(I,14)=A14(I)                                                    00004820
      A(I,15)=A15(I)                                                    00004830
      A(I,16)=A16(I)                                                    00004840
      A(I,17)=A17(I)                                                    00004850
      A(I,18)=A18(I)                                                    00004860
      A(I,19)=A19(I)                                                    00004870
      A(I,20)=A20(I)                                                    00004880
      A(I,21)=A21(I)                                                    00004890
      A(I,22)=A22(I)                                                    00004900
      A(I,23)=A23(I)                                                    00004910
      A(I,24)=A24(I)                                                    00004920
      A(I,25)=A25(I)                                                    00004930
      A(I,26)=A26(I)                                                    00004940
      A(I,27)=A27(I)                                                    00004950
      A(I,28)=A28(I)                                                    00004960
      A(I,29)=A29(I)                                                    00004970
      A(I,30)=A30(I)                                                    00004980
      A(I,31)=A31(I)                                                    00004990
    5 A(I,32)=A32(I)                                                    00005000
      IF(IOUT.EQ.0) IOUT=6                                              00005010
C     READ(2,501) NK                                                    00005020
      READ(2,*) NK                                                      00005020
      NC = NK                                                           00005030
      DO 15 I=1,10                                                      00005040
      DO 15 J=1,NK                                                      00005050
      QT(I,J)=0.D0                                                      00005060
   15 RT(I,J)=0.D0                                                      00005070
      IF(MODEL.NE.1) GOTO 19                                            00005080
      NG=NK                                                             00005090
      DO 16 I=1,NK                                                      00005100
C  16 READ(2,502) RT(I,I),QT(I,I),(P(I,J),J=1,NK)                       00005110
   16 READ(2,*) RT(I,I),QT(I,I),(P(I,J),J=1,NK)                         00005110
   19 CONTINUE                                                          00005120
      IF(MODEL.EQ.1) GOTO 21                                            00005130
C     READ(2,501) IOWNRQ,IOWNP                                          00005140
      READ(2,*) IOWNRQ,IOWNP                                            00005140
      IF(IOWNRQ.EQ.0) GOTO 10                                           00005150
      DO 6 I=1,IOWNRQ                                                   00005160
C   6 READ(2,503) K,RR(K),QQ(K)                                         00005170
    6 READ(2,*) K,RR(K),QQ(K)                                           00005170
   10 IF(IOWNP.EQ.0) GOTO 14                                            00005180
      DO 11 I=1,IOWNP                                                   00005190
C  11 READ(2,504) J,K,A(J,K)                                            00005200
   11 READ(2,*) J,K,A(J,K)                                              00005200
   14 CONTINUE                                                          00005210
c------
ccccccccccccccccccccccccccAlfonsinaccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	!rngoh=0.
	!LECTURA DE LA COMPOSICION GRUPAL ASOCIATIVA
           do ja=1, nga
		read(2,*) (rngoh(i,ja),i=1,nc) 
	end do     
	


      	!LECTURA DEL NUMERO DE SITIOS Y PARAMETROS ASOCIATIVOS

CCCCCCCCCCCCCCCCCCCCCCCCCCCCC ALFONSINA (basado en el Aparaest) CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  


	IF(NGA.GT.0) READ(2,*)(MASS(I),I=1,NGA)
C
c     lectura parametros ENERGÉTICOS DE asociacion
c
	DO J=1,NGA
			IF(MASS(J).EQ.0) GO TO 201
		DO L=1,NGA
				IF(MASS(L).EQ.0) GO TO 103
			DO I=1,MASS(J)
				DO K=1,MASS(L)
						READ(2,*)ENASS(I,J,K,L) 
				END DO
			END DO
  103			CONTINUE
			END DO
  201		   CONTINUE
	END DO
C
c     lectura parametros VOLUMÉTRICOS DE asociacion
C
      DO J=1,NGA
			IF(MASS(J).EQ.0) GO TO 3330
		DO L=1,NGA
				IF(MASS(L).EQ.0) GO TO 5550
			DO I=1,MASS(J)
				DO K=1,MASS(L)
						READ(2,*) RKASS(I,J,K,L)
 179						FORMAT (F16.10)
				END DO
			END DO
 5550			CONTINUE 
		END DO
 3330			CONTINUE
	END DO


ccccccccccccccccccccccccccAlfonsinaccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO 48 I=1,NK                                                      00005220
      DO 48 J=1,10                                                      00005230
      DO 48 K=1,2                                                       00005240
   48 MS(I,J,K)=0                                                       00005250
      DO 49 I=1,57                                                      00005260
   49 JH(I)=0                                                           00005270
      DO 50 I=1,NK                                                      00005280
C  50 READ(2,501) (MS(I,J,1),MS(I,J,2),J=1,10)                          00005290
      READ(2,*) NGI,(MS(I,J,1),MS(I,J,2),J=1,NGI)    
                           
		       do 50 j=1,ngi
      igamt(j,i)=ms(i,j,1)
      nytt(j,i)=ms(i,j,2)
c------
   50 continue
      IC=1                                                              00005300
      DO 71 I=1,NK                                                      00005310
      DO 70 J=1,10                                                      00005320
      IF(MS(I,J,1).EQ.0) GOTO 71                                        00005330
      IH(IC)=MS(I,J,1)                                                  00005340
      IF(IC.EQ.1) GOTO 69                                               00005350
      IF(IH(IC).EQ.IH(IC-1)) GOTO 70                                    00005360
      IF(IH(IC).GT.IH(IC-1)) GOTO 69                                    00005370
      IF(IC.GT.2) GOTO 55                                               00005380
      IHH=IH(1)                                                         00005390
      IH(1)=IH(2)                                                       00005400
      IH(2)=IHH                                                         00005410
      GOTO 69                                                           00005420
   55 I1=IC-1                                                           00005430
      DO 65 I2=1,I1                                                     00005440
      IF(IH(IC).GT.IH(I2)) GOTO 65                                      00005450
      IF(IH(IC).EQ.IH(I2)) GOTO 70                                      00005460
      I4=IC-I2                                                          00005470
      DO 61 I3=1,I4                                                     00005480
   61 IH(IC+1-I3)=IH(IC-I3)                                             00005490
      IH(I2)=MS(I,J,1)                                                  00005500
   65 CONTINUE                                                          00005510
   69 IC=IC+1                                                           00005520
      IF(IC.GT.20) WRITE(6,607)                                         00005530
      IF(IOUT.NE.6.AND.IC.GT.20) WRITE(IOUT,607)                        00005540
   70 CONTINUE                                                          00005550
   71 CONTINUE                                                          00005560
      IC=IC-1                                                           00005570
c------
      nktt=ic
c------
      DO 73 I=1,IC                                                      00005580
   73 JH(IH(I))=I                                                       00005590
      DO 72 I=1,10                                                      00005600
      DO 72 J=1,20                                                      00005610
   72 NY(I,J)=0                                                         00005620
      DO 75 I=1,NK                                                      00005630
      DO 74 J=1,10                                                      00005640
      IF(MS(I,J,1).EQ.0) GOTO 75                                        00005650
      N1=MS(I,J,1)                                                      00005660
      N2=MS(I,J,2)                                                      00005670
      IF(N1.EQ.0) GOTO 75                                               00005680
      N3=JH(N1)                                                         00005690
   74 NY(I,N3)=N2                                                       00005700
   75 CONTINUE                                                          00005710
      I=0                                                               00005720
      NGMGL=0                                                           00005730
      DO 80 K=1,IC                                                      00005740
      NSG=IH(K)                                                         00005750
      NGMNY=MAINSG(NSG)                                                 00005760
      IF(NGMNY.NE.NGMGL) I=I+1                                          00005770
      NGM(I)=NGMNY                                                      00005780
      NGMGL=NGMNY                                                       00005790
      DO 80 J=1,NK                                                      00005800
      RT(I,J)=RT(I,J)+NY(J,K)*RR(NSG)                                   00005810
   80 QT(I,J)=QT(I,J)+NY(J,K)*QQ(NSG)                                   00005820
      NG=I                                                              00005830
      WRITE(6,608) (IH(K),K=1,IC)                                       00005840
      WRITE(6,609) (MAINSG(IH(K)),K=1,IC)                               00005850
      WRITE(6,610)                                                      00005860
      DO 90 I=1,NK                                                      00005870
   90 WRITE(6,611) I,(NY(I,K),K=1,IC)                                   00005880
      WRITE(6,699)                                                      00005890
      IF(IOUT.EQ.6) GOTO 85                                             00005900
      WRITE(IOUT,608) (IH(K),K=1,IC)                                    00005910
      WRITE(IOUT,609) (MAINSG(IH(K)),K=1,IC)                            00005920
      WRITE(IOUT,610)                                                   00005930
      DO 91 I=1,NK                                                      00005940
   91 WRITE(IOUT,611) I,(NY(I,K),K=1,IC)                                00005950
      WRITE(IOUT,699)                                                   00005960
   85 CONTINUE                                                          00005970
      DO 20 I=1,NG                                                      00005980
      DO 20 J=1,NG                                                      00005990
      NI=NGM(I)                                                         00006000
      NJ=NGM(J)                                                         00006010
   20 P(I,J)=A(NI,NJ)                                                   00006020
      WRITE(6,612)                                                      00006030
      DO 95 K=1,IC                                                      00006040
      NN=IH(K)                                                          00006050
   95 WRITE(6,613) NN,RR(NN),QQ(NN)                                     00006060
      WRITE(6,699)                                                      00006070
      IF(IOUT.EQ.6) GOTO 99                                             00006080
      WRITE(IOUT,612)                                                   00006090
      DO 96 K=1,IC                                                      00006100
      NN=IH(K)                                                          00006110
   96 WRITE(IOUT,613) NN,RR(NN),QQ(NN)                                  00006120
      WRITE(IOUT,699)                                                   00006130
   99 CONTINUE                                                          00006140
   21 CONTINUE                                                          00006150
      WRITE(6,604)                                                      00006160
      DO 25 I=1,NG                                                      00006170
   25 WRITE(6,603) (P(I,J),J=1,NG)                                      00006180
      WRITE(6,699)                                                      00006190
      IF(MODEL.EQ.0) WRITE(6,605)                                       00006200
      IF(MODEL.EQ.1) WRITE(6,627)                                       00006210
      IF(IOUT.EQ.6) GOTO 26                                             00006220
      WRITE(IOUT,604)                                                   00006230
      DO 27 I=1,NG                                                      00006240
   27 WRITE(IOUT,603) (P(I,J),J=1,NG)                                   00006250


ccccccccccccccccc Escritura de los parámetros de asociación ALFONSINAccccccccccccccccccccccccc
       IF (NGA.GT.0) THEN
c
	WRITE(1,218) (I, I=1,NC)
 218	FORMAT(/,X,'"ASSOC GROUP COMPOSITION" ',/,23X,'COMPONENTES',/,
     @' GRUPO 	  #  SIT ASOC  ',<NC>I5,/) 

	DO ja=1,NGA
	WRITE(1,219) ja,MASS(ja)  , (rngoh(i,ja),i=1,nc) 
 219	FORMAT(3X,I3,9X,I3,6X,<NC>f5.1)
	END DO
C
	WRITE(1,220)
 220	FORMAT(/,X,'PARAMETROS DE ENERGIA DE ASOCIACION (Kelvin)  ',/)    

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	DO J=1,NGA
			IF(MASS(J).EQ.0) GO TO 202
		DO L=1,NGA
				IF(MASS(L).EQ.0) GO TO 104
			DO I=1,MASS(J)
				DO K=1,MASS(L)
						WRITE(1,221) I,J,K,L,ENASS(I,J,K,L)
  221	FORMAT(X,' ENASS( ',I3,I3,I3,I3,' ) = ',F10.4)
				END DO
			END DO
  104			CONTINUE
			END DO
  202			CONTINUE
	END DO
C
C
	WRITE(1,222)
  222	FORMAT (/,X,'PARAMETROS DE VOLUMEN DE ASOCIACIÓN (cm3/mol) ',/)
C
	DO J=1,NGA
			IF(MASS(J).EQ.0) GO TO 301
		DO L=1,NGA
				IF(MASS(L).EQ.0) GO TO 5011
			DO I=1,MASS(J)
				DO K=1,MASS(L)
					WRITE(1,223) I,J,K,L,RKASS(I,J,K,L)
  223	FORMAT(X,' RKASS( ',I3,I3,I3,I3,' ) = ',F10.4)
				END DO
			END DO
 5011			CONTINUE
			END DO
  301		   CONTINUE
	END DO
	END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCC ALFONSINA (basado en el Aparaest) CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c------
      WRITE(IOUT,699)                                                   00006260
      IF(MODEL.EQ.0) WRITE(IOUT,605)                                    00006270
      IF(MODEL.EQ.1) WRITE(IOUT,627)                                    00006280
   26 CONTINUE                                                          00006290
      DO 30 I=1,NK                                                      00006300
      Q(I)=0.D0                                                         00006310
      R(I)=0.D0                                                         00006320
      DO 30 K=1,NG                                                      00006330
      Q(I)=Q(I)+QT(K,I)                                                 00006340
   30 R(I)=R(I)+RT(K,I)                                                 00006350
      DO 40 I=1,NK                                                      00006360
   40 WRITE(6,606) I,R(I),Q(I)                                          00006370
      IF(IOUT.EQ.6) GOTO 42                                             00006380
      DO 41 I=1,NK                                                      00006390
   41 WRITE(IOUT,606) I,R(I),Q(I)                                       00006400
   42 CONTINUE                                                          00006410
  501 FORMAT(20I3)                                                      00006420
  502 FORMAT(8F10.2)                                                    00006430
  503 FORMAT(I3,2F10.2)                                                 00006440
  504 FORMAT(2I3,F10.2)                                                 00006450
  603 FORMAT(1X,10F12.3)                                                00006460
  604 FORMAT('  INTERACTION PARAMETERS',/)                              00006470
  605 FORMAT(' UNIFAC MOLECULAR R AND Q',/)                             00006480
  606 FORMAT(I5,2F15.4)                                                 00006490
  607 FORMAT(' ** WARNING: NUMBER OF SUB GROUPS MUST NOT EXCEED 20 **') 00006500
  608 FORMAT(//,' SUB GROUPS :',20I3)                                   00006510
  609 FORMAT(' MAIN GROUPS:',20I3)                                      00006520
  610 FORMAT(' COMPONENT')                                              00006530
  611 FORMAT(6X,I2,5X,20I3)                                             00006540
  612 FORMAT(' GROUP R- AND Q-VALUES',/)                                00006550
  613 FORMAT(1X,I3,2F10.4)                                              00006560
  627 FORMAT(' SPECIFIED UNIQUAC R AND Q',/)                            00006570
  699 FORMAT(//)                                                        00006580
c------
 1603 format('  ASSOCIATION PARAMETERS',//,10X,'K(OH)   :',F7.3,/,
     >                                     10X,'E(OH)/k :',F7.1,' K-1')
c------
      RETURN                                                            00006590
      END                                                               00006600
      SUBROUTINE PARAM2                                                 00006610
      IMPLICIT REAL*8(A-H,O-Z)                                          00006620
      COMMON/CUFAC/NK,NG,P(10,10),T                                     00006630
      COMMON/CPAR/TAU(10,10),S(10,10),F(10)                             00006640
      COMMON/CQT/QT(10,10),Q(10),R(10)                                  00006650
      DO 30 I=1,NG                                                      00006660
      DO 30 J=1,NG                                                      00006670
   30 TAU(I,J)=DEXP(-P(I,J)/T)                                          00006680
   40 CONTINUE                                                          00006690
      DO 50 I=1,NK                                                      00006700
      DO 50 K=1,NG                                                      00006710
      S(K,I)=0.D0                                                       00006720
      DO 50 M=1,NG                                                      00006730
   50 S(K,I)=S(K,I)+QT(M,I)*TAU(M,K)                                    00006740
      DO 60 I=1,NK                                                      00006750
      F(I)=1.D0                                                         00006760
      DO 60 J=1,NG                                                      00006770
   60 F(I)=F(I)+QT(J,I)*DLOG(S(J,I))                                    00006780
      RETURN                                                            00006790
      END                                                               00006800
      SUBROUTINE GAMINF(N1,N2,GAM)                                      00006810
      IMPLICIT REAL*8(A-H,O-Z)                                          00006820
      COMMON/CUFAC/NK,NG,P(10,10),T                                     00006830
      COMMON/CPAR/TAU(10,10),S(10,10),F(10)                             00006840
      COMMON/CQT/QT(10,10),Q(10),R(10)                                  00006850
c------
      common/asoc/nktt,igamt(20,12),nytt(20,12)  
      common/nga/nga,mass(12)
      common/grupas1/rkass(6,12,6,12),enass(6,12,6,12), deloh(6,12,6,12) !Alfonsina
c------
      dimension xohi0(10),xnohi0(10),tgt(10),xgamk(20),x(2)
	common/ioh2/rngoh(12,12)
	common/ioh2sis/rngoht(3,2)

c------
      dk=1.381e-23
      deloh=0.0
      xnoh=0.0
      xnoh1=0.0
	xoh=0.0
      xgam=0.0
      xgamt=0.0
      do 7777 i=1,10
      xnohi0(i)=0.0
      tgt(i)=0.0
 7777 continue
      do 8888 k=1,20
      xgamk(k)=0.0
 8888 continue
c------
c      x(n1)=1.0
c      x(n2)=0.0
c------


c      do 33 jc=1,2
c      if(jc.eq.1) then
c      i=n1
c      else if(jc.eq.2) then
c      i=n2
c     end if
c      goh(i)=0
c      tgt(i)=0.0
c      xnohi0(i)=0.0
c      xgam=0.0
c      do 33 j=1,nktt
c      if(((igamt(j,i).eq.14).or.(igamt(j,i).eq.17)).and.(nytt(j,i). 
c     *ne.0)) goh(i)=nytt(j,i)
c   33 continue
c      do 32 jc=1,2
c      if(jc.eq.1) then
c      i=n1
c      else if(jc.eq.2) then
c      i=n2
c      end if
c      if(nga.eq.1) then
c      do 5 k=1,nktt
c      tgt(i)=tgt(i)+nytt(k,i)
c   5 continue
      !alterar
c	xnoh1=xnoh1+goh(i)*x(i)
	!write (1,*) "xnoh1parte2=", xnoh1 
c      end if
c      xnohi0(i)=goh(i)/R(i)
	!write (1,*) "xnohi0parte2(",i,")=", xnohi0(i) 
c   32 continue
c      do 7 jc=1,2
c      if(jc.eq.1) then
c      i=n1
c      else if(jc.eq.2) then
c      i=n2
c      end if
c	!alterar
c      xgam=xgam+R(i)*x(i)
	!write (1,*) "xgamparte2=", xgam
c    7 continue
      !alterar
c     xnoh=xnoh1/xgam
	!write (1,*) "xnohparte2=", xnoh 
c------
      Q1=Q(N2)/Q(N1)                                                    00006860
      R1=R(N2)/R(N1)                                                    00006870
      QR=R1/Q1                                                          00006880
      GAM=F(N2)+Q(N2)*(1.-DLOG(Q1))-R1+DLOG(R1)-5.D0*Q(N2)*(1.-QR+DLOG(R00006890
     *1)-DLOG(Q1))                                                      00006900
      DO 10 I=1,NG                                                      00006910
   10 GAM=GAM-S(I,N2)/S(I,N1)*QT(I,N1)-QT(I,N2)*DLOG(S(I,N1))           00006920
c------
c      deloh=p4*(dexp(p5/t)-1.)
c      if((xnohi0(n2).eq.0.0).or.(deloh.eq.0.0)) then
c      xohi0(n2)=1.0
c      else
c      xohi0(n2)=(-1.+dsqrt(1.+4.*xnohi0(n2)*deloh))/(2.*xnohi0(n2)*
c     *deloh)
c      end if
c      if((xnoh.eq.0.0).or.(deloh.eq.0.0)) then
c      xoh=1.0
c      else
c      xoh=(-1.+dsqrt(1.+4.*xnoh*deloh))/(2.*xnoh*deloh)
c      end if
	!alterar
c      gam=gam+goh(n2)*(2.*dlog(xoh/xohi0(n2))+xohi0(n2)-xoh)-(2.*xoh-
c     *xoh**2.)*deloh*xnoh*(goh(n2)-R(n2)*xnoh)/(1.+2.*xnoh*xoh*deloh)
c------
      RETURN                                                            00006930
      END                                                               00006940
      SUBROUTINE STIG(Y,S)                                              00006950
      IMPLICIT REAL*8(A-H,O-Z)                                          00006960
      COMMON/CVAP/NOVAP,NDUM,IDUM(4),PRAT(10)                           00006970
      COMMON/CUFAC/N,NG,P(10,10),T                                      00006980
      COMMON/CACT/Y1(10),Y2(10),ACT1(10),ACT2(10),DACT1(10,10),DACT2(10,00006990
     *10),PACT(2,2)                                                     00007000
      COMMON/CGIBBS/NF,MAXZ,GNUL,Z(10),A(10),XVL(10,4),SFAS(4),GAM(10,1000007010
     *),AA(10),DA(10,10),XM(10,4)                                       00007020
      DIMENSION Y(10),V(10),YGEM(10)                                    00007030
      common/nga/nga,mass(12)
      JPUR=0                                                            00007040
      AMAX=0.                                                           00007050
      DO 10 I=1,N                                                       00007060
      IF(A(I).LT.AMAX) GOTO 10                                          00007070
      JPUR=I                                                            00007080
      AMAX=A(I)                                                         00007090
   10 CONTINUE                                                          00007100
      RMAX=1.D5                                                         00007110
      NGN=N                                                             00007120
      IF(NF.GT.1) NGN=N+NF                                              00007130
      NEG=0                                                             00007140
      DO 100 KK=1,NGN                                                   00007150
      JM=KK                                                             00007160
      IF(JPUR.NE.0) JM=JPUR                                             00007170
      IF(JM.LE.N) GOTO 30                                               00007180
      DO 20 I=1,N                                                       00007190
   20 Y(I)=Z(I)*(2+XVL(I,JM-N)/SFAS(JM-N))/3                            00007200
      GOTO 40                                                           00007210
   30 SUM=0.                                                            00007220
      DO 35 I=1,N                                                       00007230
      GG=A(I)-GAM(JM,I)                                                 00007240
      IF(GG.LT.-50.D0) GG=-50.D0                                        00007250
      Y(I)=DEXP(GG)                                                     00007260
   35 SUM=SUM+Y(I)                                                      00007270
   40 NA=3                                                              00007280
      DO 43 K=1,NA                                                      00007290
      DO 36 I=1,N                                                       00007300
   36 Y(I)=Y(I)/SUM                                                     00007310
      CALL UNIFA2(1,Y,AA,DA,PACT)                                       00007320
      IF(K.EQ.NA) GOTO 44                                               00007330
      DO 41 I=1,N                                                       00007340
   41 Y(I)=DEXP(A(I)-AA(I))                                             00007350
   42 SUM=0.                                                            00007360
      DO 43 I=1,N                                                       00007370
   43 SUM=SUM+Y(I)                                                      00007380
   44 CONTINUE                                                          00007390
      YV1=0.                                                            00007400
      DO 50 J=1,NF                                                      00007410
   50 V(J)=0.                                                           00007420
      DO 60 I=1,N                                                       00007430
      GD=DLOG(Y(I))+AA(I)-A(I)                                          00007440
      YV1=YV1+Y(I)*GD                                                   00007450
      DO 60 J=1,NF                                                      00007460
      K=J                                                               00007470
      VV=XVL(I,K)*Z(I)/SFAS(K)                                          00007480
      D=GD*(Y(I)-VV)                                                    00007490
   60 V(J)=V(J)+D                                                       00007500
      YV2=V(1)                                                          00007510
      DO 70 J=1,NF                                                      00007520
      IF(V(J).LT.YV2) YV2=V(J)                                          00007530
   70 CONTINUE                                                          00007540
      RT1=YV1                                                           00007550
      IF(YV2.GT.0.) RT1=RT1-YV2/2                                       00007560
      IF(NEG.EQ.0.AND.YV1.GT.0.) GOTO 80                                00007570
      RT1=YV1                                                           00007580
      IF(NEG.EQ.0) RMAX=0.                                              00007590
      NEG=1                                                             00007600
   80 IF(RT1.GT.RMAX) GOTO 100                                          00007610
      S=YV1                                                             00007620
      RMAX=RT1                                                          00007630
      CC=DEXP(-YV1)                                                     00007640
      DO 90 I=1,N                                                       00007650
   90 YGEM(I)=Y(I)*CC                                                   00007660
      IF(JPUR.NE.0) GOTO 110                                            00007670
  100 CONTINUE                                                          00007680
  110 DO 120 I=1,N                                                      00007690
  120 Y(I)=YGEM(I)                                                      00007700
      RETURN                                                            00007710
      END                                                               00007720
      SUBROUTINE GMIX(NARG,NDIF,FUN,GRAD,XMAT,YVAL)                     00007730
      IMPLICIT REAL*8(A-H,O-Z)                                          00007740
      COMMON/CVAP/NOVAP,NDUM,IDUM(4),PRAT(10)                           00007750
      COMMON/CACT/Y1(10),Y2(10),ACT1(10),ACT2(10),DACT1(10,10),DACT2(10,00007760
     *10),PACT(2,2)                                                     00007770
      COMMON/CGIBBS/NF,MAXZ,GNUL,Z(10),A(10),XVL(10,4),SFAS(4),GAM(10,1000007780
     *),XX(10),DA(10,10),XM(10,4)                                       00007790
      DIMENSION YVAL(30),GRAD(30),X(10),TM(10),FG(10),XMAT(30,30)       00007800
      NG=NF-1                                                           00007810
      N=NARG/NG                                                         00007820
      JD=1                                                              00007830
      IF(NDIF.EQ.2) JD=2                                                00007840
      IF(NDIF.EQ.2) CALL CHECK(N,YVAL)                                  00007850
      IF(NF.NE.NG) GOTO 20                                              00007860
      NARG=NARG-N                                                       00007870
      NG=NG-1                                                           00007880
      DO 10 I=1,N                                                       00007890
      DO 10 J=1,NG                                                      00007900
   10 YVAL(I+(J-1)*N)=XVL(I,J)                                          00007910
   20 FUN=-GNUL                                                         00007920
      DO 50 I=1,N                                                       00007930
      XVL(I,NF)=1.                                                      00007940
      DO 30 J=1,NG                                                      00007950
      XVL(I,J)=YVAL(I+(J-1)*N)                                          00007960
   30 XVL(I,NF)=XVL(I,NF)-XVL(I,J)                                      00007970
      ! write(*,*)xvl
      DO 40 J=1,NF                                                      00007980
      IF(XVL(I,J).GT.0.) GOTO 40                                        00007990
      FUN=0.                                                            00008000
      GOTO 1000                                                         00008010
   40 CONTINUE                                                          00008020
   50 CONTINUE                                                          00008030
      DO 200 J=1,NF                                                     00008040
      SFAS(J)=0.                                                        00008050
      DO 60 I=1,N                                                       00008060
      X(I)=XVL(I,J)*Z(I)                                                00008070
   60 SFAS(J)=SFAS(J)+X(I)                                              00008080
      DO 65 I=1,N                                                       00008090
      XX(I)=X(I)/SFAS(J)                                                00008100
   65 XM(I,J)=XX(I)                                                     00008110
      CALL UNIFA2(JD,XX,FG,DA,PACT)                                     00008120
      IDUM(J)=NDUM                                                      00008130
      DO 70 I=1,N                                                       00008140
      TM(I)=DLOG(XVL(I,J)/SFAS(J))+FG(I)                                00008150
   70 FUN=FUN+X(I)*TM(I)                                                00008160
      IF(NDIF.EQ.0) GOTO 200                                            00008170
      DO 80 I=1,N                                                       00008180
      S=Z(I)*TM(I)                                                      00008190
      IF(J.EQ.NF) GOTO 75                                               00008200
      GRAD(I+(J-1)*N)=S                                                 00008210
      GOTO 80                                                           00008220
   75 DO 76 K=1,NG                                                      00008230
      NK=I+(K-1)*N                                                      00008240
   76 GRAD(NK)=GRAD(NK)-S                                               00008250
   80 CONTINUE                                                          00008260
      IF(NDIF.EQ.1) GOTO 200                                            00008270
      DO 100 I=1,N                                                      00008280
      ST=Z(I)/SFAS(J)                                                   00008290
      DO 100 L=1,N                                                      00008300
      S=ST*(DA(I,L)-1.)*Z(L)                                            00008310
      IF(L.EQ.I)S=S+Z(I)/XVL(I,J)                                       00008320
      IF(J.EQ.NF) GOTO 90                                               00008330
      XMAT(I+(J-1)*N,L+(J-1)*N)=S                                       00008340
      GOTO 95                                                           00008350
   90 DO 92 K=1,NG                                                      00008360
      DO 92 M=1,K                                                       00008370
      NK=I+(K-1)*N                                                      00008380
      NM=L+(M-1)*N                                                      00008390
      IF(K.NE.M) XMAT(NK,NM)=S                                          00008400
      IF(K.EQ.M) XMAT(NK,NM)=XMAT(NK,NM)+S                              00008410
   92 CONTINUE                                                          00008420
   95 CONTINUE                                                          00008430
  100 CONTINUE                                                          00008440
  200 CONTINUE                                                          00008450
 1000 RETURN                                                            00008460
      END                                                               00008470
      SUBROUTINE STABIL(N,NDIF,FUN,GRAD,XMAT,Y)                         00008480
      IMPLICIT REAL*8(A-H,O-Z)                                          00008490
      COMMON/CACT/Y1(10),Y2(10),ACT1(10),ACT2(10),DACT1(10,10),DACT2(10,00008500
     *10),PACT(2,2)                                                     00008510
      COMMON/CGIBBS/NF,MAXZ,GNUL,Z(10),A(10),XVL(10,4),SFAS(4),GAM(10,1000008520
     *),AL(10),DA(10,10),XM(10,4)                                       00008530
      DIMENSION GRAD(30),XMAT(30,30),Y(30),YEX(10),XEX(10)              00008540
      common/nga/nga,mass(12)

      SUM=0.                                                            00008550
      DO 10 I=1,N                                                       00008560
      YEX(I)=0.                                                         00008570
      IF(Y(I).GT.-40.) YEX(I)=DEXP(Y(I))                                00008580
   10 SUM=SUM+YEX(I)                                                    00008590
      DO 15 I=1,N                                                       00008600
   15 XEX(I)=YEX(I)/SUM                                                 00008610
      JD=1                                                              00008620
      IF(NDIF.EQ.2) JD=2                                                00008630
      CALL UNIFA2(JD,XEX,AL,DA,PACT)                                    00008640
      FUN=1.                                                            00008650
      DO 20 I=1,N                                                       00008660
      S=Y(I)+AL(I)-A(I)                                                 00008670
      IF(NDIF.EQ.0) GOTO 20                                             00008680
      GRAD(I)=YEX(I)*S                                                  00008690
   20 FUN=FUN+YEX(I)*(S-1)                                              00008700
      IF(NDIF.LT.2) GOTO 50                                             00008710
      DO 30 I=1,N                                                       00008720
      S=XEX(I)                                                          00008730
      DO 40 J=1,I                                                       00008740
      XMAT(I,J)=S*YEX(J)*DA(I,J)                                        00008750
   40 XMAT(J,I)=XMAT(I,J)                                               00008760
   30 XMAT(I,I)=XMAT(I,I)+YEX(I)+GRAD(I)                                00008770

  
  
   50 RETURN                                                            00008780
      END                                                               00008790
      SUBROUTINE CHECK(N,YVAL)                                          00008800
      IMPLICIT REAL*8(A-H,O-Z)                                          00008810
      COMMON/CGIBBS/NF,MAXZ,GNUL,Z(10),A(10),XVL(10,4),SFAS(4),GAM(10,1000008820
     *),AL(10),DA(10,10),XM(10,4)                                       00008830
      COMMON/CIPR/IPR                                                   00008840
      COMMON/COUT/IOUT                                                  00008850
      DIMENSION YVAL(30)                                                00008860
      JMAX=NF                                                           00008870
      SMAX=SFAS(NF)                                                     00008880
      NT=NF-1                                                           00008890
      DO 5 J=1,NT                                                       00008900
      IF(SFAS(J).LT.SMAX) GOTO 5                                        00008910
      SMAX=SFAS(J)                                                      00008920
      JMAX=J                                                            00008930
    5 CONTINUE                                                          00008940
      IF(JMAX.EQ.NF) GOTO 20                                            00008950
      DO 10 I=1,N                                                       00008960
      DS=1.                                                             00008970
      DO 15 J=1,NT                                                      00008980
   15 DS=DS-YVAL(I+(J-1)*N)                                             00008990
   10 YVAL(I+(JMAX-1)*N)=DS                                             00009000
   20 IF(NF.EQ.2) GOTO 100                                              00009010
      DO 21 I=1,N                                                       00009020
      XVL(I,NF)=1.                                                      00009030
      DO 21 J=1,NT                                                      00009040
      XVL(I,J)=YVAL(I+(J-1)*N)                                          00009050
   21 XVL(I,NF)=XVL(I,NF)-XVL(I,J)                                      00009060
      DO 23 J=1,NF                                                      00009070
      SFAS(J)=0.                                                        00009080
      DO 22 I=1,N                                                       00009090
      AL(I)=XVL(I,J)*Z(I)                                               00009100
   22 SFAS(J)=SFAS(J)+AL(I)                                             00009110
      DO 23 I=1,N                                                       00009120
   23 XM(I,J)=AL(I)/SFAS(J)                                             00009130
      DO 30 I=1,NT                                                      00009140
      JN=I+1                                                            00009150
      DO 30 J=JN,NF                                                     00009160
      IF(DABS(XM(MAXZ,I)-XM(MAXZ,J)).LT.5.D-3) GOTO 40                  00009170
   30 CONTINUE                                                          00009180
      GOTO 100                                                          00009190
   40 IV=I                                                              00009200
      JV=J                                                              00009210
      DMAX=0.                                                           00009220
      DO 50 I=1,N                                                       00009230
      R1=DABS(XM(I,IV)-XM(I,JV))                                        00009240
      IF(R1.GT.DMAX) DMAX=R1                                            00009250
   50 CONTINUE                                                          00009260
      IF(DMAX.GT.2.5D-2) GOTO 100                                       00009270
      WRITE(6,601)                                                      00009280
      IF(IOUT.NE.6) WRITE(IOUT,601)                                     00009290
      NF=NF-1                                                           00009300
      NT=NT-1                                                           00009310
      DO 60 I=1,N                                                       00009320
      XVL(I,IV)=XVL(I,IV)+XVL(I,JV)                                     00009330
   60 XVL(I,JV)=XVL(I,NF+1)                                             00009340
  100 CONTINUE                                                          00009350
      IF(NF.LT.3) GOTO 250                                              00009360
      MINF=0                                                            00009370
      DO 200 I=1,NF                                                     00009380
      IF(SFAS(I).LT.1.D-12) MINF=I                                      00009390
  200 CONTINUE                                                          00009400
      IF(MINF.EQ.0) GOTO 250                                            00009410
      WRITE(6,602) MINF,SFAS(MINF)                                      00009420
      IF(IOUT.NE.6) WRITE(IOUT,602) MINF,SFAS(MINF)                     00009430
      DO 220 I=1,NF                                                     00009440
      IF(I.EQ.MINF) GOTO 220                                            00009450
      DO 210 J=1,N                                                      00009460
  210 XVL(J,I)=XVL(J,I)+SFAS(I)*XVL(J,MINF)                             00009470
  220 CONTINUE                                                          00009480
      IF(MINF.EQ.NF) GOTO 250                                           00009490
      NNF=NF-1                                                          00009500
      DO 230 I=1,NNF                                                    00009510
      IF(I.LT.MINF) GOTO 230                                            00009520
      DO 240 J=1,N                                                      00009530
  240 XVL(J,I)=XVL(J,I+1)                                               00009540
  230 CONTINUE                                                          00009550
      NF=NF-1                                                           00009560
  250 CONTINUE                                                          00009570
  601 FORMAT(//,' * * * TWO PHASES ARE IDENTICAL * * *'//)              00009580
  602 FORMAT(/,' * THE AMOUNT OF PHASE',I2,' IS',D12.4,'. THE PHASE IS E00009590
     *LIMINATED *',/)                                                   00009600
      RETURN                                                            00009610
      END                                                               00009620
      SUBROUTINE SPLIT(ND,N,IDI,BETA,DEL,G,W)                           00009630
      IMPLICIT REAL*8(A-H,O-Z)                                          00009640
      DIMENSION G(ND,ND),W(ND,5)                                        00009650
      IDI=0                                                             00009660
      DO 20 I=1,N                                                       00009670
      I1=I-1                                                            00009680
      I2=I+1                                                            00009690
      ID=0                                                              00009700
      TV=G(I,I)                                                         00009710
      SV=TV                                                             00009720
      IF (I1.EQ.0) GO TO 25                                             00009730
      DO 30 J=1,I1                                                      00009740
   30 SV=SV-G(I,J)**2                                                   00009750
   25 IF (SV.LT. DEL*DEL) ID=1                                          00009760
      SVR=DSQRT(DABS(SV))                                               00009770
      IF (SVR.LT. DEL) SVR=DEL                                          00009780
      XM=0.                                                             00009790
      IF (I.EQ.N) GO TO 35                                              00009800
      DO 40 J=I2,N                                                      00009810
      S=G(J,I)                                                          00009820
      IF (I1.EQ.0) GO TO 45                                             00009830
      DO 50 K=1,I1                                                      00009840
   50 S=S-G(I,K)*G(J,K)                                                 00009850
   45 S=S/SVR                                                           00009860
      IF (DABS(S).GT. XM) XM=DABS(S)                                    00009870
   40 G(J,I)=S                                                          00009880
   35 IF (XM.LT. BETA) GO TO 55                                         00009890
      ID=1                                                              00009900
      XM=XM/BETA                                                        00009910
      DO 60 J=I,N                                                       00009920
   60 G(J,I)=G(J,I)/XM                                                  00009930
      SVR=SVR*XM                                                        00009940
   55 IF (ID.EQ.1) W(I,1)=SVR**2-SV                                     00009950
      G(I,I)=SVR                                                        00009960
      IDI=IDI+ID                                                        00009970
   20 CONTINUE                                                          00009980
      RETURN                                                            00009990
      END                                                               00010000
      SUBROUTINE LINE(ND,N,XLAM,GD,G,W)                                 00010010
      IMPLICIT REAL*8(A-H,O-Z)                                          00010020
      DIMENSION GD(ND),G(ND,ND),W(ND,5)                                 00010030
   65 W(1,2)=-GD(1)/(G(1,1)+XLAM)                                       00010040
      IF (N.EQ.1) GO TO 75                                              00010050
      DO 70 I=2,N                                                       00010060
      I1=I-1                                                            00010070
      S=-GD(I)                                                          00010080
      DO 80 J=1,I1                                                      00010090
   80 S=S-G(I,J)*W(J,2)                                                 00010100
   70 W(I,2)=S/(G(I,I)+XLAM)                                            00010110
   75 W(N,3)=W(N,2)/(G(N,N)+XLAM)                                       00010120
      IF (N.EQ.1) GO TO 85                                              00010130
      DO 90 II=2,N                                                      00010140
      I=N+1-II                                                          00010150
      I1=I+1                                                            00010160
      S=W(I,2)                                                          00010170
      DO 100  J=I1,N                                                    00010180
  100 S=S-G(J,I)*W(J,3)                                                 00010190
   90 W(I,3)=S/(G(I,I)+XLAM)                                            00010200
   85 RETURN                                                            00010210
      END                                                               00010220
      SUBROUTINE TMSSJ(ND,N,IPR,NMAX,XLAM,GLIM,F   ,X,GD,G,W,ifunc)     00010230
      IMPLICIT REAL*8(A-H,O-Z)                                          00010240
      COMMON/COUT/IOUT                                                  00010250
      integer::ifunc
      DIMENSION X(ND),GD(ND),G(ND,ND),W(ND,5)                           00010260
      NTM=0                                                             00010270
      DEL=1.D-7                                                         00010280
  150 NTM=NTM+1                                                         00010290
      GNM=0.                                                            00010300
      
      if(ifunc==1)then
        CALL STABIL(N,2,F,GD,G,X)                                       00010310
      elseif(ifunc==2)then
        CALL GMIX(N,2,F,GD,G,X)  
      endif
      DO 5 I=1,N                                                        00010320
    5 GNM=GNM+GD(I)**2                                                  00010330
      IF (GNM.LT. GLIM) GO TO 200                                       00010340
      BETA=0.                                                           00010350
      DO 10 I=1,N                                                       00010360
      W(I,4)=X(I)                                                       00010370
      W(I,5)=GD(I)                                                      00010380
      DO 10 J=1,I                                                       00010390
      S=DABS(G(I,J))                                                    00010400
      IF (I.EQ.J) S=S*N                                                 00010410
      IF (S.GT. BETA) BETA=S                                            00010420
   10 W(I,1)=0.                                                         00010430
      BETA=DSQRT(BETA/N)                                                00010440
      CALL SPLIT(ND,N,IDI,BETA,DEL,G,W)                                 00010450
      XLM=0.                                                            00010460
      NTS=0                                                             00010470
  350 NTS=NTS+1                                                         00010480
      CALL LINE(ND,N,XLM,GD,G,W)                                        00010490
      SMAX=0.                                                           00010500
      GP=0.                                                             00010510
      DP=0.                                                             00010520
      DO 205 I=1,N                                                      00010530
      S=W(I,3)                                                          00010540
      IF (DABS(S).GT. SMAX) SMAX=DABS(S)                                00010550
      GP=GP+S*GD(I)                                                     00010560
  205 DP=DP+S*S*W(I,1)                                                  00010570
      FAC=1.                                                            00010580
      IF (SMAX.GT. XLAM) FAC=XLAM/SMAX                                  00010590
      DER=FAC*GP                                                        00010600
      ALFA=1.                                                           00010610
  210 FF=ALFA*FAC                                                       00010620
      DO 215 I=1,N                                                      00010630
  215 X(I)=W(I,4)+FF*W(I,3)                                             00010640
 
       if(ifunc==1)then
        CALL STABIL(N,1,FNEW,GD,G,X)                           
      elseif(ifunc==2)then
        CALL GMIX(N,1,FNEW,GD,G,X)   
      endif
 
 
 
                                             
      IF(FNEW.NE.0.) GOTO 220                                           00010660
      ALFA=ALFA/2.                                                      00010670
      GOTO 210                                                          00010680
  220 DELS=FNEW-F                                                       00010690
       IF (FNEW.NE. 0.  .AND. DELS.LT. 1.D-10) GO TO 230                00010700
      IF (NTS.GT. 1) GO TO 125                                          00010710
      DE2=(DELS-ALFA*DER)/ALFA**2/2                                     00010720
      GT=-DER/DE2                                                       00010730
      IF (GT.GT. ALFA) ALFA=ALFA/3.                                     00010740
      IF (GT.LE. ALFA) ALFA=GT                                          00010750
      GO TO 210                                                         00010760
  230 PRED=FF*GP-FF**2/2*(GP+DP)                                        00010770
      CALPRE=DELS/PRED                                                  00010780
      F=FNEW                                                            00010790
      DO 345 I=1,N                                                      00010800
  345 W(I,4)=X(I)                                                       00010810
      IF (NTS.GE.3) GO TO 125                                           00010820
      IF (ALFA.EQ. 1.  .AND. CALPRE.GT. .8) GO TO 350                   00010830
  125 DO 126 I=1,N                                                      00010840
  126 X(I)=W(I,4)                                                       00010850
      IF (IPR.NE.0) WRITE(6,130) NTM,F,CALPRE,GNM,ALFA                  00010860
      IF(IOUT.NE.6.AND.IPR.NE.0) WRITE(IOUT,130) NTM,F,CALPRE,GNM,ALFA  00010870
  130 FORMAT(1X,I2,':  F = ',D16.8,'  CALPRE = ',F8.3,'  GRAD = ',D16.8,00010880
     *'  ALFA = ',D10.2)                                                00010890
      IF (IPR.GT.1) WRITE(6,131) (X(I),I=1,N)                           00010900
      IF(IOUT.NE.6.AND.IPR.GT.1) WRITE(IOUT,131) (X(I),I=1,N)           00010910
  131 FORMAT(' X-VECTOR',10F11.5,(/,9X,10F11.5))                        00010920
      IF (IPR.GT. 1) WRITE(6,132)                                       00010930
      IF(IOUT.NE.6.AND.IPR.GT.1) WRITE(IOUT,132)                        00010940
  132 FORMAT(' ')                                                       00010950
      IF (NTM.LT. NMAX) GO TO 150                                       00010960
  200 IF(IPR.GT.0) WRITE(6,133) NTM,GNM,F                               00010970
      IF(IOUT.NE.6.AND.IPR.GT.0) WRITE(IOUT,133) NTM,GNM,F              00010980
  133 FORMAT(/,'  NUMBER OF ITERATIONS = ',I2,', NORM OF GRADIENT = ',  00010990
     *D12.5,', F = ',D14.6)                                             00011000
      IF(IPR.GT.0) WRITE(6,131) (X(I),I=1,N)                            00011010
      IF(IOUT.NE.6.AND.IPR.GT.0) WRITE(IOUT,131) (X(I),I=1,N)           00011020
      RETURN                                                            00011030
      END                                                               00011040
      SUBROUTINE BINOD                                                  00011050
      IMPLICIT REAL*8 (A-H,O-Z)                                         00011060
      COMMON/CY/Y13,Y21,STEP                                            00011070
      COMMON/COUT/IOUT                                                  00011080
      DIMENSION YMAT(70,6),NITE3(70)                                    00011090
      COMMON/CBISO/NIC1,NIC2,IC1(120),IC2(120)                          00011100
      DIMENSION Y(4),YOLD(4),DY(4),DYOLD(4),DMAT(4,4)                   00011110
      DIMENSION K(6)                                                    00011120
c-----
      dimension xandrea1(10),xandrea2(10),act1(10),act2(10),dact(10,10),
     >pact(2,2),acttot(70,6)
      common/ig/ig
c-----      
      DATA K/1,3,2,4,5,6/                                               00011130
      WRITE(6,601)                                                      00011140
      IF(IOUT.NE.6) WRITE(IOUT,601)                                     00011150
  601 FORMAT(///,10X,' ** BINODAL CURVE, CONCENTRATIONS IN MOLE PERCENT 00011160
     ***',//,18X,'COMP. 1',13X,'COMP. 2',13X,'COMP. 3')                 00011170
c-----
      write(6,1601)
      if(iout.ne.6) write(iout,1601)
 1601 format(18x,'gamma1',14x,'gamma2',14x,'gamma3',/)
c-----     
      DO 1 I=1,4                                                        00011180
      DO 1 J=1,4                                                        00011190
1     DMAT(I,J)=0.D0                                                    00011200
      IRUND=200                                                         00011210
      NIC1=0                                                            00011220
      NIC2=0                                                            00011230
      ICOND=0                                                           00011240
      NOLD=2                                                            00011250
      N=0                                                               00011260
      Y(1)=1.D0-Y13/100.D0                                              00011270
      Y(2)=0.D0                                                         00011280
      Y(3)=Y21/100.D0                                                   00011290
      Y(4)=0.D0                                                         00011300
   12 CALL SOLVE(Y,DY,NOLD,NEW,NITER,N,NT)                              00011310
      IF(NITER.LE.NT) GOTO 16                                           00011320
      IF(N.GT.0) GOTO 19                                                00011330
      ICOND=-10                                                         00011340
      WRITE(6,902)                                                      00011350
      IF(IOUT.NE.6) WRITE(IOUT,902)                                     00011360
  902 FORMAT(/,' THE BASE LINE CALCULATION DID NOT CONVERGE IN 10 ITERAT00011370
     *IONS'/)                                                           00011380
      GOTO 3                                                            00011390
   19 IF(IHALF.LT.5) GOTO 20                                            00011400
      ICOND=-3                                                          00011410
      GOTO 3                                                            00011420
   20 IHALF=IHALF+1                                                     00011430
      ST=ST/2.D0                                                        00011440
      GOTO 17                                                           00011450
   16 IF(DABS(Y(1)-Y(3))+DABS(Y(2)-Y(4)).GT.1.D-8) GOTO 21              00011460
      IF(N.GT.0) GOTO 19                                                00011470
      WRITE(6,903)                                                      00011480
      IF(IOUT.NE.6) WRITE(IOUT,903)                                     00011490
  903 FORMAT(/,' THE CONCENTRATIONS ON THE BASE LINE ARE IDENTICAL IN TH00011500
     *E TWO PHASES'/)                                                   00011510
      GOTO 3                                                            00011520
   21 N=N+1                                                             00011530
      NITE3(N)=NITER                                                    00011540
      IHALF=0                                                           00011550
      DO 2 I=1,4                                                        00011560
2     YMAT(N,I)=Y(I)                                                    00011570
      IF(ICOND.EQ.2.AND.Y(1).LT.1.D-10) GOTO 3                          00011580
      DYMAX=DABS(DY(NEW))                                               00011590
      DO 4 I=1,4                                                        00011600
4     DY(I)=DY(I)/DYMAX                                                 00011610
      IF(N.EQ.1)GOTO 5                                                  00011620
      STAP=DABS(Y(NEW)-YOLD(NEW))                                       00011630
      IF(DY(NEW)*DYOLD(NEW).GT.0.D0)GOTO 6                              00011640
      DO 7 I=1,4                                                        00011650
7     DY(I)=-DY(I)                                                      00011660
6     IF(NEW.EQ.NOLD)GOTO 8                                             00011670
      RR=DY(NEW)/DYOLD(NEW)                                             00011680
      DO 9 I=1,4                                                        00011690
9     DYOLD(I)=DYOLD(I)*RR                                              00011700
8     DO 10 I=1,4                                                       00011710
      Z=(YOLD(I)-Y(I))/STAP                                             00011720
      DMAT(I,3)=(3.D0*Z+2.D0*DY(I)+DYOLD(I))/STAP                       00011730
10    DMAT(I,4)=(2.D0*Z+DY(I)+DYOLD(I))/STAP**2                         00011740
5     ST=RUND(Y(NEW),DY(NEW),STEP,IRUND)                                00011750
      DO 18 I=1,4                                                       00011760
      DMAT(I,1)=Y(I)                                                    00011770
      DMAT(I,2)=DY(I)                                                   00011780
      YOLD(I)=Y(I)                                                      00011790
18    DYOLD(I)=DY(I)                                                    00011800
17    DO 11 I=1,4                                                       00011810
      Y(I)=DMAT(I,4)                                                    00011820
      DO 11 J=1,3                                                       00011830
11    Y(I)=ST*Y(I)+DMAT(I,4-J)                                          00011840
      IF(IHALF.GT.0)GOTO 12                                             00011850
      CALL TERM(Y,DMAT,ICOND,NEW)                                       00011860
      NOLD=NEW                                                          00011870
      IF(ICOND.EQ.0.OR.ICOND.EQ.2) GOTO 12                              00011880
3     NGIT=N                                                            00011890
      IF(N.EQ.0) RETURN                                                 00011900
      IF(ICOND.NE.1)GOTO 13                                             00011910
      N=N+1                                                             00011920
      DO 14 I=1,4                                                       00011930
14    YMAT(N,I)=Y(I)                                                    00011940
      NITE3(N)=0                                                        00011950
13    IF(N.EQ.0)RETURN                                                  00011960
      DO 15 I=1,N                                                       00011970
      YMAT(I,5)=1.D0-YMAT(I,1)-YMAT(I,2)                                00011980
15    YMAT(I,6)=1.D0-YMAT(I,3)-YMAT(I,4)                                00011990
      DO 30 I=1,N                                                       00012000
c-----
      xandrea1(1)=ymat(i,k(1))
      xandrea1(2)=ymat(i,k(3))
      xandrea1(3)=ymat(i,k(5))
      xandrea2(1)=ymat(i,k(2))
      xandrea2(2)=ymat(i,k(4))
      xandrea2(3)=ymat(i,k(6))
      call unifa2(0,xandrea1,act1,dact,pact)
      call unifa2(0,xandrea2,act2,dact,pact)
      acttot(i,1)=dexp(act1(1))
      acttot(i,2)=dexp(act2(1))
      acttot(i,3)=dexp(act1(2))
      acttot(i,4)=dexp(act2(2))
      acttot(i,5)=dexp(act1(3))
      acttot(i,6)=dexp(act2(3))
c-----
      WRITE(6,600) I,NITE3(I),(YMAT(I,K(J)),J=1,6)   
	WRITE(3,611) I,NITE3(I),(YMAT(I,K(J)),J=1,6)   
	
      write(8,47) YMAT (i,1),  YMAT (i,2), YMAT (i,5)    !Alfonsina
	 write(8,47) YMAT (i,3),  YMAT (i,4), YMAT (i,6)  !Alfonsina
	 write(8,*) 
	 
   47	FORMAT (8X,F12.6 , 8X,F12.6 , 8X,F12.6) !Alfonsina                  
c-----
      if(ig.eq.1) write(6,1600) (acttot(i,j),j=1,6)
   30 continue
c-----   
  600 FORMAT(5X,2I3,2X,3(2P2F9.4,2X))     
  611 FORMAT(5X,2I3,2X,3(2P2F9.4,2X))                                    00012020
      IF(NIC1.GT.0) WRITE(6,900) IC1(1),IC1(NIC1)                       00012030
 1600 format(12x,6(f9.3,1x))
  900 FORMAT(/,' FALSE SOLUTION IN PHASE 1 FOR CALCULATED TIE LINES',I3,00012040
     *' TO',I3,/)                                                       00012050
      IF(NIC2.GT.0) WRITE(6,901) IC2(1),IC2(NIC2)                       00012060
  901 FORMAT(/,' FALSE SOLUTION IN PHASE 2 FOR CALCULATED TIE LINES',I3,00012070
     *' TO',I3,/)                                                       00012080
      IF(IOUT.EQ.6) GOTO 50                                             00012090
      DO 35 I=1,N                                                       00012100
      WRITE(IOUT,600) I,NITE3(I),(YMAT(I,K(J)),J=1,6)                   00012110
    

c-----
      if(ig.eq.1) write(iout,1600) (acttot(i,j),j=1,6)
   35 continue
c-----   
      IF(NIC1.GT.0) WRITE(IOUT,900) IC1(1),IC1(NIC1)                    00012120
      IF(NIC2.GT.0) WRITE(IOUT,901) IC2(1),IC2(NIC2)                    00012130
   50 CONTINUE                                                          00012140
      RETURN                                                            00012150
      END                                                               00012160
      SUBROUTINE SOLVE(Y,DY,NOLD,NEW,NITER,N,NT)                        00012170
      IMPLICIT REAL*8(A-H,O-Z)                                          00012180
      COMMON/CACT/Y1(10),Y2(10),ACT1(10),ACT2(10),DACT1(10,10),DACT2(10,00012190
     *10),PACT(2,2)                                                     00012200
      COMMON/CBISO/NIC1,NIC2,IC1(120),IC2(120)                          00012210
      DIMENSION Y(4),DY(4),AMAT(3,5)                                    00012220
      DIMENSION INO(3)                                                  00012230
      common/nga/nga,mass(12)
      NK=1000  !cambié de 3 a 100 !Alfonsina                            00012240
      NITER=0                                                           00012250
      NT=1000  !cambié de 3 a 100 !Alfonsina                          00012260
      DO 1 I=1,3                                                        00012270
    1 IF(Y(I).LT.1.D-14) NT=1000    !Cambie de 10 iteraciones a100 !Alfonsina 0
11    NITER=NITER+1                                                     00012290
      IF(NITER.GT.NT) RETURN                                            00012300
      DO 2 I=1,4                                                        00012310
2     IF(Y(I).LT.0.D0)Y(I)=0.D0                                         00012320
      DO 3 I=1,2                                                        00012330
      Y1(I)=Y(I)                                                        00012340
3     Y2(I)=Y(I+2)                                                      00012350
      Y1(3)=1.D0-Y1(1)-Y1(2)                                            00012360
      Y2(3)=1.D0-Y2(1)-Y2(2)                                            00012370
      IF(Y1(3).LT.0.)Y1(3)=0.                                           00012380
      IF(Y2(3).LT.0.) Y2(3)=0.                                          00012390
      CALL UNIFA2(3,Y1,ACT1,DACT1,PACT)                                 00012400
      CALL UNIFA2(3,Y2,ACT2,DACT2,PACT)                                 00012410
      J=0                                                               00012420
      DO 6 I=1,4                                                        00012430
      IF(I.EQ.NOLD)GOTO 6                                               00012440
      J=J+1                                                             00012450
      INO(J)=I                                                          00012460
6     CONTINUE                                                          00012470
      DO 7 I=1,3                                                        00012480
      DO 7 J=1,2                                                        00012490
      AMAT(I,J)=DACT1(I,J)-DACT1(I,3)                                   00012500
7     AMAT(I,J+2)=DACT2(I,3)-DACT2(I,J)                                 00012510
      DO 8 I=1,3                                                        00012520
      AMAT(I,5)=AMAT(I,NOLD)                                            00012530
      DO 9 J=1,3                                                        00012540
9     AMAT(I,J)=AMAT(I,INO(J))                                          00012550
8     AMAT(I,4)=ACT1(I)-ACT2(I)                                         00012560
      CALL GAUSL(3,5,3,2,AMAT)                                          00012570
      RES=0.D0                                                          00012580
      DO 10 I=1,3                                                       00012590
      Y(INO(I))=Y(INO(I))-AMAT(I,4)                                     00012600
      DY(INO(I))=-AMAT(I,5)                                             00012610
10    RES=RES+AMAT(I,4)**2                                              00012620
      IF(RES.GT.1.D-10)GOTO 11                                          00012630
      IZ=0                                                              00012640
      DO 14 I=1,2                                                       00012650
      IF(Y1(I).LT.1.D-14) IZ=1                                          00012660
   14 IF(Y2(I).LT.1.D-14) IZ=1                                          00012670
      IF(IZ.EQ.1) GOTO 13                                               00012680
      CALL GCON(3,Y1,ACT1,DACT1,ICVEX)                                  00012690
      IF(ICVEX.EQ.1) GOTO 15                                            00012700
      NIC1=NIC1+1                                                       00012710
      IC1(NIC1)=N+1                                                     00012720
   15 CALL GCON(3,Y2,ACT2,DACT2,ICVEX)                                  00012730
      IF(ICVEX.EQ.1) GOTO 13                                            00012740
      NIC2=NIC2+1                                                       00012750
      IC2(NIC2)=N+1                                                     00012760
13    DY(NOLD)=1.D0                                                     00012770
      NEW=NOLD                                                          00012780
      DYMAX=1.D0                                                        00012790
      DO 12 I=1,4                                                       00012800
      IF(DABS(DY(I)).LE.DYMAX)GOTO 12                                   00012810
      NEW=I                                                             00012820
      DYMAX=DABS(DY(I))                                                 00012830
12    CONTINUE                                                          00012840
      RETURN                                                            00012850
      END                                                               00012860
      SUBROUTINE GCON(NK,X,ACT,DACT,ICVEX)                              00012870
      IMPLICIT REAL*8(A-H,O-Z)                                          00012880
      DIMENSION X(3),DG(2),DDG(2,2),ACT(3),DACT(10,10)                  00012890
      ICVEX=1                                                           00012900
      DO 1 I=1,NK                                                       00012910
    1 IF(ACT(I).LT.1.D-10) ACT(I)=1.D-10                                00012920
      DO 5 I=1,NK                                                       00012930
      DO 5 J=1,NK                                                       00012940
    5 DACT(I,J)=DACT(I,J)/ACT(I)                                        00012950
      IF(NK.EQ.3) GOTO 9                                                00012960
      DDG(2,2)=DACT(2,2)-DACT(1,2)-DACT(2,1)+DACT(1,1)                  00012970
      GOTO 30                                                           00012980
9     DO 20 I=2,NK                                                      00012990
      II=I-1                                                            00013000
      DO 20 J=2,NK                                                      00013010
      JJ=J-1                                                            00013020
   20 DDG(II,JJ)=DACT(I,J)-DACT(1,J)-DACT(I,1)+DACT(1,1)                00013030
      IF(X(1).LE.1.D-12.OR.X(2).LE.1.D-12) GOTO 30                      00013040
      DET=DDG(1,1)*DDG(2,2)-DDG(2,1)*DDG(2,1)                           00013050
      IF(DET.LE.0.D0.OR.DDG(1,1).LE.0.D0.OR.DDG(2,2).LE.0.D0) ICVEX=-1  00013060
      GOTO 100                                                          00013070
   30 CONTINUE                                                          00013080
      IF(DDG(2,2).LE.0.D0) ICVEX=-1                                     00013090
  100 CONTINUE                                                          00013100
      RETURN                                                            00013110
      END                                                               00013120
      FUNCTION RUND(Y,DY,S,IRUND)                                       00013130
      IMPLICIT REAL*8(A-H,O-Z)                                          00013140
      X=Y+S*DY+1.D-8*DY**2                                              00013150
      IX=IRUND*X                                                        00013160
      Z=DFLOAT(IX)/IRUND-Y                                              00013170
      RUND=DABS(Z)                                                      00013180
      RETURN                                                            00013190
      END                                                               00013200
      SUBROUTINE TERM(Y,DMAT,ICOND,NEW)                                 00013210
      IMPLICIT REAL*8(A-H,O-Z)                                          00013220
      DIMENSION Y(4),DMAT(4,4),A(4)                                     00013230
      IF(Y(1).LT.1.D-14.OR.Y(3).LT.1.D-14) GOTO 1                       00013240
      IF(Y(1)+Y(2).GT.1.D0.OR.Y(3)+Y(4).GT.1.D0) GOTO 2                 00013250
      IF(Y(1)+Y(2)-.01D0.LT.Y(3)+Y(4).AND.Y(1)-.01D0.LT.Y(3))GOTO 3     00013260
      RETURN                                                            00013270
1     ICOND=2                                                           00013280
      DS=DMAT(1,1)/(DMAT(1,1)-Y(1))                                     00013290
      DO 5 I=1,4                                                        00013300
5     Y(I)=DMAT(I,1)+DS*(Y(I)-DMAT(I,1))                                00013310
      Y(1)=0.D0                                                         00013320
      NEW=1                                                             00013330
      RETURN                                                            00013340
2     ICOND=-2                                                          00013350
      RETURN                                                            00013360
3     ICOND=1                                                           00013370
      ND=2+NEW                                                          00013380
      IF(ND.GT.4)ND=ND-4                                                00013390
      DO 6 I=1,4                                                        00013400
6     A(I)=DMAT(NEW,I)-DMAT(ND,I)                                       00013410
      DS=0.D0                                                           00013420
      NITER=0                                                           00013430
7     NITER=NITER+1                                                     00013440
      IF(NITER.LE.10)GOTO 8                                             00013450
      ICOND=-1                                                          00013460
      RETURN                                                            00013470
8     F=((A(4)*DS+A(3))*DS+A(2))*DS+A(1)                                00013480
      DF=(3.D0*A(4)*DS+2.D0*A(3))*DS+A(2)                               00013490
      DF=-F/DF                                                          00013500
      DS=DS+DF                                                          00013510
      IF(DABS(DF).GT.1.D-6)GOTO 7                                       00013520
      DO 9 I=1,4                                                        00013530
      Y(I)=DMAT(I,4)                                                    00013540
      DO 9 J=1,3                                                        00013550
9     Y(I)=Y(I)*DS+DMAT(I,4-J)                                          00013560
      RETURN                                                            00013570
      END                                                               00013580
      SUBROUTINE SOLBIN                                                 00013590
      IMPLICIT REAL*8(A-H,O-Z)                                          00013600
      COMMON/CACT/X1(10),X2(10),ACT1(10),ACT2(10),DACT1(10,10),DACT2(10,00013610
     *10),PACT(2,2)                                                     00013620
      COMMON/CY/Y13,Y21,STEP                                            00013630
      COMMON/COUT/IOUT                                                  00013640
      DIMENSION DMAT(2,3)                                               00013650
      common/nga/nga,mass(12)

      NITER=0                                                           00013660
      X1(1)=1.D0-Y13/100.D0                                             00013670
      X2(1)=Y21/100.D0                                                  00013680
   10 NITER=NITER+1                                                     00013690
      IF(NITER.GT.10) GOTO 50                                           00013700
      IF(X1(1).LT.0.D0) X1(1)=0.D0                                      00013710
      IF(X2(1).LT.0.D0) X2(1)=0.D0                                      00013720
      X1(2)=1.D0-X1(1)                                                  00013730
      X2(2)=1.D0-X2(1)                                                  00013740
      CALL UNIFA2(3,X1,ACT1,DACT1,PACT)                                 00013750
      CALL UNIFA2(3,X2,ACT2,DACT2,PACT)                                 00013760
      DO 20 I=1,2                                                       00013770
      DMAT(I,1)=DACT1(I,1)-DACT1(I,2)                                   00013780
      DMAT(I,2)=DACT2(I,2)-DACT2(I,1)                                   00013790
   20 DMAT(I,3)=ACT1(I)-ACT2(I)                                         00013800
      CALL GAUSL(2,3,2,1,DMAT)                                          00013810
      RES=DMAT(1,3)**2+DMAT(2,3)**2                                     00013820
      X1(1)=X1(1)-DMAT(1,3)                                             00013830
      X2(1)=X2(1)-DMAT(2,3)                                             00013840
      IF(RES.GT.1.D-20) GOTO 10                                         00013850
   50 CONTINUE                                                          00013860
      WRITE(6,603)                                                      00013870
      IF(IOUT.NE.6) WRITE(IOUT,603)                                     00013880
      IF(IOUT.NE.6) WRITE(IOUT,604) X1(1),X2(1),X1(2),X2(2)             00013890
      WRITE(6,604) X1(1),X2(1),X1(2),X2(2)                              00013900
  603 FORMAT(///,5X,'** BINARY SOLUBILITIES IN MOLE FRACTIONS **',//,11X00013910
     *,'COMPONENT 1',15X,'COMPONENT 2',/)                               00013920
  604 FORMAT(2(2X,2P2D12.2)//)                                          00013930
      CALL GCON(2,X1,ACT1,DACT1,ICVEX)                                  00013940
      IF(IOUT.NE.6.AND.ICVEX.EQ.-1) WRITE(IOUT,601)                     00013950
      IF(ICVEX.EQ.-1) WRITE(6,601)                                      00013960
      CALL GCON(2,X2,ACT2,DACT2,ICVEX)                                  00013970
      IF(IOUT.NE.6.AND.ICVEX.EQ.-1) WRITE(IOUT,602)                     00013980
      IF(ICVEX.EQ.-1) WRITE(6,602)                                      00013990
  601 FORMAT(' FALSE SOLUTION IN PHASE 1')                              00014000
  602 FORMAT(' FALSE SOLUTION IN PHASE 2')                              00014010
      RETURN                                                            00014020
      END                                                               00014030
      SUBROUTINE FUNC(N,M,NDIF,X,SSQ)                                   00014040
      IMPLICIT REAL*8(A-H,O-Z)                                          00014050
      COMMON/CACT/Y1(10),Y2(10),ACT1(10),ACT2(10),DACT1(10,10),DACT2(10,00014060
     *10),PACT(2,2)                                                     00014070
      COMMON/CMARQ/GRAD(2),XJTJ(2,2)                                    00014080
      COMMON/CUFAC/NK,NG,P(10,10),T                                     00014090
      COMMON/CA/XC(5),GE(5,2),GC(5,2)                                   00014100
      DIMENSION X(2),F(2)                                               00014110
      JD=0                                                              00014120
      IF(NDIF.EQ.1) JD=4                                                00014130
      P(1,2)=X(1)*300.D0                                                00014140
      P(2,1)=X(2)*300.D0                                                00014150
      CALL PARAM2                                                       00014160
      SSQ=0.                                                            00014170
      IF(NDIF.EQ.0) GOTO 11                                             00014180
      DO 10 I=1,2                                                       00014190
      GRAD(I)=0.                                                        00014200
      DO 10 J=1,2                                                       00014210
   10 XJTJ(I,J)=0.                                                      00014220
   11 CONTINUE                                                          00014230
      DO 21 L=1,5                                                       00014240
      Y1(1)=XC(L)                                                       00014250
      Y1(2)=1.D0-XC(L)                                                  00014260
      CALL UNIFA2(JD,Y1,ACT1,DACT1,PACT)                                00014270
      DO 17 I=1,2                                                       00014280
      F(I)=ACT1(I)-GE(L,I)                                              00014290
      GC(L,I)=ACT1(I)                                                   00014300
   17 SSQ=SSQ+F(I)*F(I)                                                 00014310
      IF(JD.EQ.0) GOTO 21                                               00014320
      DO 19 I=1,2                                                       00014330
      DO 20 J=1,2                                                       00014340
      GRAD(J)=GRAD(J)+F(I)*PACT(I,J)                                    00014350
      DO 20 K=1,2                                                       00014360
   20 XJTJ(J,K)=XJTJ(J,K)+PACT(I,J)*PACT(I,K)                           00014370
   19 CONTINUE                                                          00014380
   21 CONTINUE                                                          00014390
      RETURN                                                            00014400
      END                                                               00014410
      SUBROUTINE MARQ(FUNC,N,M,X,XLAMB,FAC,EPSG,MAXF)                   00014420
      IMPLICIT REAL*8(A-H,O-Z)                                          00014430
      COMMON/COUT/IOUT                                                  00014440
      COMMON/CMARQ/GRAD(2),XJTJ(2,2)                                    00014450
      COMMON/CIPR/IPR                                                   00014460
      DIMENSION X(2),Y(2),XNY(2),A(2,2),DX(2)                           00014470
      IEVAL=0                                                           00014480
      ISTOP=0                                                           00014490
      IER=0                                                             00014500
      XMAXL=XLAMB*1.D+4                                                 00014510
      ITER=0                                                            00014520
      IF(IOUT.NE.6.AND.IPR.EQ.1) WRITE(IOUT,603)                        00014530
      IF(IPR.EQ.1) WRITE(6,603)                                         00014540
      CALL FUNC(N,M,1,X,SRES)                                           00014550
      IEVAL=IEVAL+1                                                     00014560
      SSQ=SRES                                                          00014570
      IF(IPR.EQ.1.AND.IOUT.NE.6) WRITE(IOUT,601) ITER,SSQ               00014580
      IF(IPR.EQ.1) WRITE(6,601) ITER,SSQ                                00014590
   10 CONTINUE                                                          00014600
      IF(IEVAL.NE.1) CALL FUNC(N,M,1,X,SRES)                            00014610
      GNORM=0.D0                                                        00014620
      DO 140 I=1,N                                                      00014630
  140 GNORM=GNORM+GRAD(I)**2                                            00014640
      GNORM=DSQRT(GNORM)                                                00014650
      IF(GNORM.LT.EPSG) ISTOP=ISTOP+1                                   00014660
      IF(ISTOP.GT.0) GOTO 1000                                          00014670
      ITER=ITER+1                                                       00014680
   49 CONTINUE                                                          00014690
      IF(IEVAL.GT.MAXF) GOTO 998                                        00014700
      DO 41 I=1,N                                                       00014710
      DO 40 J=1,N                                                       00014720
   40 A(I,J)=XJTJ(I,J)                                                  00014730
   41 A(I,I)=A(I,I)+XLAMB                                               00014740
      CALL CHOL(N,A)                                                    00014750
      Y(1)=-GRAD(1)/A(1,1)                                              00014760
      DO 81 I=2,N                                                       00014770
      SUM=0.D0                                                          00014780
      II=I-1                                                            00014790
      DO 80 J=1,II                                                      00014800
   80 SUM=SUM+A(I,J)*Y(J)                                               00014810
   81 Y(I)=(-GRAD(I)-SUM)/A(I,I)                                        00014820
      DX(N)=Y(N)/A(N,N)                                                 00014830
      DO 85 I=2,N                                                       00014840
      II=N-I+1                                                          00014850
      SUM=0.D0                                                          00014860
      III=II+1                                                          00014870
      DO 84 J=III,N                                                     00014880
   84 SUM=SUM+A(J,II)*DX(J)                                             00014890
   85 DX(II)=(Y(II)-SUM)/A(II,II)                                       00014900
      DO 90 I=1,N                                                       00014910
   90 XNY(I)=X(I)+DX(I)                                                 00014920
      CALL FUNC(N,M,0,XNY,SRES)                                         00014930
      IEVAL=IEVAL+1                                                     00014940
      SSQNY=SRES                                                        00014950
      SQ1=0.D0                                                          00014960
      SQ2=0.D0                                                          00014970
      DO 110 I=1,N                                                      00014980
      Y(I)=XNY(I)*300.D0                                                00014990
      SQ1=SQ1+DX(I)**2                                                  00015000
      SQ2=SQ2-DX(I)*GRAD(I)                                             00015010
  110 CONTINUE                                                          00015020
      CCAL=SSQ-SSQNY                                                    00015030
      CPRE=SQ2+XLAMB*SQ1                                                00015040
      CALPRE=CCAL/(CPRE+1.D-14)                                         00015050
      IF(IPR.EQ.1) WRITE(6,601) ITER,SSQNY,Y(1),Y(2),GNORM,XLAMB,CALPRE 00015060
      IF(IOUT.NE.6.AND.IPR.EQ.1) WRITE(IOUT,601) ITER,SSQNY,Y(1),Y(2),  00015070
     *GNORM,XLAMB,CALPRE                                                00015080
      IF(SSQ-SSQNY.GT..75*CPRE) XLAMB=XLAMB/FAC                         00015090
      IF(SSQ-SSQNY.LT..25*CPRE) XLAMB=XLAMB*FAC                         00015100
      IF(XLAMB.GT.XMAXL) GOTO 999                                       00015110
      IF(SSQNY-SSQ) 120,120,119                                         00015120
  119 CONTINUE                                                          00015130
      IF(SSQNY-SSQ.GT.DABS(SSQ)*.5D0) XLAMB=XLAMB*FAC                   00015140
      GOTO 49                                                           00015150
  120 CONTINUE                                                          00015160
      IF(SSQ-SSQNY.GT.DABS(SSQ)*.8D0) XLAMB=XLAMB/FAC                   00015170
      DO 130 I=1,N                                                      00015180
  130 X(I)=XNY(I)                                                       00015190
      SSQ=SSQNY                                                         00015200
      GOTO 10                                                           00015210
  998 IER=1                                                             00015220
      GOTO 1000                                                         00015230
  999 IER=2                                                             00015240
      GOTO 1000                                                         00015250
  601 FORMAT(1X,I3,2X,D12.4,2F10.3,5X,2D12.4,3X,F10.2)                  00015260
  602 FORMAT(//,' ISTOP= ',I2,5X,'IER = ',I2,5X,'IEVAL = ',I5,//)       00015270
  603 FORMAT(///,'  ** ITERATIONCOURSE, UNIQUAC-PARAMETERS FROM UNIFAC *00015280
     **'/)                                                              00015290
 1000 CONTINUE                                                          00015300
      IF(IOUT.NE.6.AND.IPR.EQ.1) WRITE(IOUT,602) ISTOP,IER,IEVAL        00015310
      IF(IPR.EQ.1) WRITE(6,602)ISTOP, IER, IEVAL                        00015320
      RETURN                                                            00015330
      END                                                               00015340
      SUBROUTINE CHOL(N,A)                                              00015350
      IMPLICIT REAL*8(A-H,O-Z)                                          00015360
      DIMENSION A(2,2)                                                  00015370
      DO 50 I=1,N                                                       00015380
      I1=I-1                                                            00015390
      IF(I1.EQ.0) GOTO 30                                               00015400
      DO 20 J=I,N                                                       00015410
      DO 20 K=1,I1                                                      00015420
   20 A(I,J)=A(I,J)-A(I,K)*A(J,K)                                       00015430
   30 IF(A(I,I).LT.1.D-14) A(I,I)=1.D-14                                00015440
      A(I,I)=DSQRT(A(I,I))                                              00015450
      IF(I.EQ.N) GOTO 100                                               00015460
      J1=I+1                                                            00015470
      DO 50 J=J1,N                                                      00015480
   50 A(J,I)=A(I,J)/A(I,I)                                              00015490
  100 RETURN                                                            00015500
      END                                                               00015510
C  SUBROUTINE GAUSL SOLVES N LINEAR ALGEBRAIC EQUATIONS BY GAUSS        00015520
C  ELIMINATION WITH ROW PIVOTING                                        00015530
C  TO SOLVE THE PROBLEM QX=U, WHERE Q IS A NXN MATRIX AND U IS NXNS,    00015540
C  ONE PLACES Q IN THE FIRST N COLUMNS OF A AND U IS PLACED IN THE      00015550
C  FOLLOWING NS COLUMNS.                                                00015560
C  THE PROGRAM RETURNS X=Q**(-1)*U AT THE PREVIOUS POSITION OF U.       00015570
C  *                                                                    00015580
C  ND IS THE ROW DIMENSION AND NCOL IS THE COLUMN DIMENSION OF A.       00015590
C  BOTH MUST BE TRANSFERRED TO THE SUBROUTINE.                          00015600
C  *****************                                                    00015610
C                                                                       00015620
      SUBROUTINE GAUSL(ND,NCOL,N,NS,A)                                  00015630
C                                                                       00015640
      IMPLICIT REAL*8 (A-H,O-Z)                                         00015650
      DIMENSION A(ND,NCOL)                                              00015660
      N1=N+1                                                            00015670
      NT=N+NS                                                           00015680
      IF (N .EQ. 1) GO TO 50                                            00015690
C      START ELIMINATION                                                00015700
C                                                                       00015710
C                                                                       00015720
      DO 10 I=2,N                                                       00015730
      IP=I-1                                                            00015740
      I1=IP                                                             00015750
      X=DABS(A(I1,I1))                                                  00015760
      DO 11 J=I,N                                                       00015770
      IF (DABS(A(J,I1)) .LT. X) GO TO 11                                00015780
      X=DABS(A(J,I1))                                                   00015790
      IP=J                                                              00015800
   11 CONTINUE                                                          00015810
      IF (IP .EQ. I1) GO TO 13                                          00015820
C                                                                       00015830
C     ROW INTERCHANGE                                                   00015840
C                                                                       00015850
      DO 12 J=I1,NT                                                     00015860
      X=A(I1,J)                                                         00015870
      A(I1,J)=A(IP,J)                                                   00015880
   12 A(IP,J)=X                                                         00015890
   13 DO 10 J=I,N                                                       00015900
      X=A(J,I1)/A(I1,I1)                                                00015910
      DO 10 K=I,NT                                                      00015920
   10 A(J,K)=A(J,K) - X*A(I1,K)                                         00015930
C                                                                       00015940
C      ELIMINATION FINISHED, NOW BACKSUBSTITUTION                       00015950
C                                                                       00015960
   50 DO 20 IP=1,N                                                      00015970
      I=N1-IP                                                           00015980
      DO 20 K=N1,NT                                                     00015990
      A(I,K) = A(I,K)/A(I,I)                                            00016000
      IF (I .EQ. 1) GO TO 20                                            00016010
      I1=I-1                                                            00016020
      DO 25 J=1,I1                                                      00016030
   25 A(J,K) = A(J,K) - A(I,K)*A(J,I)                                   00016040
   20 CONTINUE                                                          00016050
      RETURN                                                            00016060
      END                                                               00016070
C******************************* F I N ***************************************
C
