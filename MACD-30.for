c **********************************************************************
c * MACD-30.for                                                        *
c *                                                                    *
c *      This program is used to pre-process some data for BPE         *
c *      of Bernese 5.2 and output *.sum files for TurboNet.           *
c *                                                                    *
c *                 created for 5.2 version by Kwo-Hwa Chen 2017.09.27 *
c *                                                         2017.11.15 *
c *                                                         2018.11.27 *
c *                                                         2019.02.15 *
c **********************************************************************
      program macd30
      
      implicit none
      character basename*4,tmp*4,cmm*200,campname*7,snxname*12
      character doy*3,ss*2,yr*2
            
      open(5,file='MACDDATA.LST')
      
 100  read(5,*,end=600) campname
     
      if(campname(1:1).eq.'*') go to 100
      
      call system("copy/y FIXSTA-MACD.SIG FIXSTA.SIG")
ccc      if(fixsta(1:4).eq.'LSB0') cmm='copy/y FIXSTA-LSB0.SIG FIXSTA.SIG'
ccc      if(fixsta(1:4).eq.'JUNA') cmm='copy/y FIXSTA-JUNA.SIG FIXSTA.SIG'
ccc      if(fixsta(1:4).eq.'CKSV') cmm='copy/y FIXSTA-CKSV.SIG FIXSTA.SIG'
ccc      if(fixsta(1:4).eq.'S01R') cmm='copy/y FIXSTA-S01R.SIG FIXSTA.SIG'
ccc      if(fixsta(1:4).eq.'HENC') cmm='copy/y FIXSTA-HECN.SIG FIXSTA.SIG'
ccc      call system(cmm)
         
      open(15,file='RUN.INP')   
      
      write(15,*) '2'
      write(15,'(a7,1x,a8,1x,a8,a7,a1)') campname(1:7),
     +'D:\GPSR\','D:\GPSD\',campname,'\' 
  
      close(15)

      cmm='dir/b/o D:\GPSD\'//campname//'\*.??O > BASE.LST'
      call system(cmm)
     
      open(10,file='BASE.LST')
   
  200 read(10,'(a4,a3,2a2)',end=300) basename,doy,ss,yr
      tmp=basename
      basename=tmp

      go to 200
      
  300 continue    

      cmm='md D:\GPSR\'//campname(1:7)//'\SUM'
      call system(cmm)    
      
      cmm='BERN52-OBS30-MACD RUN.INP'
      call system(cmm)

      cmm='copy D:\GPSR\'//campname(1:7)//'\SOL\*.SNX C:\BERN52\AUTO\'
      call system(cmm)
      
      close(10)
      
      cmm='dir/b/o C:\BERN52\AUTO\*.SNX > TR2SUM.LST'
      call system(cmm)
      
      open(80,file='TR2SUM.LST')

      
      read(80,'(a12)') snxname
      
      if(snxname(1:2).ne.'  ') then
  
        cmm='copy '//snxname//' 20'//yr//doy//ss(1:1)//'.SNX'
        call system(cmm)
      
        cmm='RDSINEX50 20'//yr//doy//ss(1:1)//'.SNX'
        call system(cmm)

        cmm='move C:\BERN52\AUTO\*.SUM D:\GPSR\'//campname(1:7)
     +//'\SUM\'
        call system(cmm)   
        cmm='rename D:\GPSR\'//campname(1:7)//' '//campname(1:7)//'P'
        call system(cmm)   
      
        cmm='del/q C:\BERN52\AUTO\*.SNX'
        call system(cmm)  

      else
      
        cmm='rename D:\GPSR\'//campname(1:7)//' '//campname(1:7)//'K'
        call system(cmm)        
     
      end if      
      
      close(80)
 
      go to 100
      
 600  continue
      cmm='del BASE.LST'
      call system(cmm)
      cmm='del TR2SUM.LST'
      call system(cmm)
      cmm='del RUN.INP'
      call system(cmm) 
      
      close(5)
      close(10)
      close(15)
      close(80)
      
      stop
      end     
