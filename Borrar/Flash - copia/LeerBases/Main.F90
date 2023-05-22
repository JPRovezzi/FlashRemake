program LeerBases
    implicit none
    integer::model, ipareq, Ncomp, i, j
    integer,dimension(:,:,:):: ms(10,10,2)
    character(len=36)::name

    open (unit=1,FILE='name',status='OLD',FORM='FORMATTED')
    read(1,"(A36)") name
    close (unit=1)    
    name = name(2:len_trim(name)-1)
    
    open (unit=2,FILE=name,status='OLD',FORM='FORMATTED')
    
    read(2,"(2i)") model, ipareq, Ncomp

    do i=1,Ncomp
        read(2,"(20i)") (ms(I,J,1),ms(I,J,2),j=1,size(ms(1,:,1)))    
    enddo
    
    call ab_ban1(model)
    
    close (unit=2)
endprogram LeerBases

subroutine ab_ban1(model)
!-------------------------------------------------------------------
!      Esta subrunina ABRE los bancos de datos siguientes:
!     - INTRCN.MDS      (UNIT=13)
!     - INTRCNAS.MDS    (UNIT=13)
!     - GRUPOSRAM.MDS   (UNIT=14)
!     - PARVOLAS.MDS    (UNIT=15)  
!     - PARENEAS.MDS    (UNIT=16)     
!-------------------------------------------------------------------
!
    !use input_data, only:model
   !   COMMON/AS/ASOC
   !   LOGICAL ASOC
      integer::model, mod
    mod=model
      
    !CALL MODEL(mod)
    if(mod /= 3)then
        if(mod==1)then
            open (unit=13,file='Database\intrcn.mds',status='old',&
                  access='direct',form='formatted',recl=850,CARRIAGECONTROL='LIST')   
        else     
            open (unit=13,file='Database\intrcnas.mds',status='old',&
                  access='direct',form='formatted',recl=850,CARRIAGECONTROL='LIST')        
        endif
        open (unit=14,file='Database\gruposram.mds',status='old',&
     	      access='direct',form='formatted',recl=300,CARRIAGECONTROL='LIST')
        open (unit=15,file='Database\parvolas.mds',status='old',&
              access='direct',form='formatted',recl=850,CARRIAGECONTROL='LIST')
        open (unit=16,file='Database\pareneas.mds',status='old',&
              access='direct',form='formatted',recl=850,CARRIAGECONTROL='LIST')    
    else

        open (unit=14,file='Database\gruposramgc.mds',status='old',&
     	      access='direct',form='formatted',recl=263,CARRIAGECONTROL='LIST')
        open (unit=13,file='Database\intrcngcalpha.mds',status='old',&
              access='direct',form='formatted',recl=730,CARRIAGECONTROL='LIST')
        open (unit=16,file='Database\intrcngckapa.mds',status='old',&
              access='direct',form='formatted',recl=730,CARRIAGECONTROL='LIST')    
        
    endif
      

      return
endsubroutine ab_ban1