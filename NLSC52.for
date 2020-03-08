c **********************************************************************
c * NLSC52.for                                                         *
c *                                                                    *
c *      This program is used to pre-process some data for BPE         *
c *      of Bernese 5.2 and output *.sum files for TurboNet.           *
c *                                                                    *
c *                 created for 5.2 version by Kwo-Hwa Chen 2016.03.24 *
c **********************************************************************
      program nlsc52
      
      implicit none
      character buffer*20,cl*80,campname*7,snxname*12
      character outf1*12,outf2*12,basename*4,tmp*200,cmm*200,inf*50
      character doy*3,ss*2,yr*2,tmp1*200,site1*4,site2*4
      integer*4 nargs,num,i
            
      call getcl(cl)
      call getarg(nargs,buffer,cl)
      
      inf=buffer
      
      open(5,file=inf)
      
 100  read(5,'(a7)',end=600) campname
CC      runf='RUN.INP'
      
      if(campname(1:1).eq.'!') go to 100
         
      open(15,file='RUN.INP')   
      
      write(15,*) '2'
      write(15,'(a6,a1,1x,a8,1x,a8,a7,a1)') campname(1:6),'P',
     +'C:\GPSR\','C:\GPSD\',campname,'\' 
  
      close(15)
      
CC      cmm='copy C:\BERN52\AUTO\RUN.INP C:\BERN52\'
CC      call system(cmm)
 
CC      cmm='cd C:\GPSD\'//campname//'\BASE'
CC      call system(cmm)      
      
      cmm='dir/b/o C:\GPSD\'//campname//'\BASE\*.??O > BASE.LST'
      call system(cmm)
      
CC      cmm='copy C:\BERN52\AUTO\GETBASE.EXE'
CC      call system(cmm)
      
CC      cmm='copy C:\BERN52\AUTO\SNGDIF.DAT'
CC      call system(cmm)
      
CC      cmm='copy C:\BERN52\AUTO\GPSEST.DAT'
CC      call system(cmm)
      
      open(10,file='BASE.LST')
      
      num=0
 500  open(20,file='SNGDIF.DAT')
      open(30,file='GPSEST.DAT') 
     
 510  read(10,'(a4,a3,2a2)',end=550) basename,doy,ss,yr

      cmm='copy/y C:\GPSD\'//campname//'\BASE\'//basename//doy//ss
     +//yr//'O C:\GPSD\'//campname
      call system(cmm)   
      
      if(num.ne.0) then
         cmm='del/q C:\GPSR\'//campname(1:6)//'P\OBS\*.*'
         call system(cmm)  
      end if
      
      if(num.eq.0) then   
         cmm='md C:\GPSR\'//campname(1:6)//'P\SUM'
         call system(cmm)    
         cmm='md C:\GPSR\'//campname(1:6)//'P\RAW\BASE'
         call system(cmm)             
      end if   

      num=num+1
      
      outf1(1:4)='SNG_'
      outf1(5:8)=basename(1:4)
      outf1(9:12)='.INP'
 
      outf2(1:4)='GPS_'
      outf2(5:8)=basename(1:4)
      outf2(9:12)='.INP'      
            
      open(40,file=outf1)
      open(50,file=outf2) 
    
 520  read(20,'(a200)',end=530) tmp
      if(tmp(1:10).eq.'PHASEREF 1') then
         write(40,'(a13,a4,a5)') 'PHASEREF 1  "',basename,'$S+0"'
      else
         write(40,'(a200)') tmp   
      end if      
      go to 520
      
 530  read(30,'(a200)',end=540) tmp
      if(tmp(1:10).eq.'STATION4 1') then
         write(50,'(a13,a4,a1)') 'STATION4 1  "',basename,'"'
      else      
         write(50,'(a200)') tmp
      end if
      go to 530
      
 540  close(20)
      close(30)
      close(40)
      close(50)
      
      cmm='del/q C:\GPSUSER52\OPT\5230_GEN\SNGDIF.bak'
      call system(cmm)
      cmm='ren C:\GPSUSER52\OPT\5230_GEN\SNGDIF.INP SNGDIF.bak'
      call system(cmm)
      cmm='copy/y SNG_'//basename//'.INP '//'C:\GPSUSER52\OPT\5230_GEN'
     +//'\SNGDIF.INP'
      call system(cmm)
      cmm='del SNG_'//basename//'.INP'
      call system(cmm)
      
      cmm='del/q C:\GPSUSER52\OPT\5230_QIF\GPSEST.bak'
      call system(cmm)
      cmm='ren C:\GPSUSER52\OPT\5230_QIF\GPSEST.INP GPSEST.bak'
      call system(cmm)
      cmm='copy/y GPS_'//basename//'.INP '//'C:\GPSUSER52\OPT\5230_QIF'
     +//'\GPSEST.INP'
      call system(cmm)
      cmm='del GPS_'//basename//'.INP'
      call system(cmm)
      
      cmm='BERN52-STAR30 RUN.INP'
      call system(cmm)

      cmm='copy C:\GPSR\'//campname(1:6)//'P\SOL\*.SNX C:\BERN52\AUTO\'
CC     +//campname(1:6)//'P\SUM\'
      call system(cmm)

      cmm='del C:\GPSD\'//campname//'\'//basename//doy//ss
     +//yr//'O'
      call system(cmm)  
      
      cmm='move C:\GPSR\'//campname(1:6)//'P\RAW\'//basename//doy//ss
     +//yr//'O C:\GPSR\'//campname(1:6)//'P\RAW\BASE\'
      call system(cmm)  

      go to 500   
      
 550  close(10)
      
      cmm='dir/b/o C:\BERN52\AUTO\*.SNX > TR2SUM.LST'
      call system(cmm)
      
      open(70,file='TR2SUM.LST')
CC      open(80,file='TR2SUM.BAT')
      
 610  read(70,'(a12)',end=620) snxname
  
      cmm='copy '//snxname//' 20'//yr//doy//ss(1:1)//'.SNX'
      call system(cmm)
      
      cmm='RDSINEX50 20'//yr//doy//ss(1:1)//'.SNX'
      call system(cmm)

      cmm='copy C:\GPSR\'//campname(1:6)//'P\STA\ABBREV.ABB C:\BERN52\'
     +//'AUTO\'
      
      call system(cmm)

      open(90,file='ABBREV.ABB')
      
      do i = 1,5
         read(90,'(a200)') tmp1 
      end do

 700  read(90,'(a200)',end=710) tmp1
      if(snxname(1:2).eq.tmp1(35:36)) site1(1:4)=tmp1(26:29)
      if(snxname(3:4).eq.tmp1(35:36)) site2(1:4)=tmp1(26:29) 
      go to 700

 710  close(90)
      
CC      write(80,'(a17,1x,a12)') 'CALL RDSINEX52 20',yr,doy,ss,'.SUM'
  
      cmm='ren 20'//yr//doy//ss(1:1)//'.SUM '//campname(1:6)//'_'//
     +site1(1:4)//'_'//site2(1:4)//'.sum'
      call system(cmm)

      go to 610     
      
 620  close(70)
CC      close(80)
      
CC      cmm='CALL C:\BERN52\AUTO\TR2SUM.BAT'     
CC      call system(cmm)
      
      cmm='move C:\BERN52\AUTO\*.SUM C:\GPSR\'//campname(1:6)
     +//'P\SUM\'
      call system(cmm)   
      
      cmm='del/q C:\BERN52\AUTO\*.SNX'
      call system(cmm)  

      cmm='del/q C:\BERN52\AUTO\ABBREV.ABB'
      call system(cmm)
 
      go to 100
      
 600  cmm='del BASE.LST'
      call system(cmm)
      cmm='del TR2SUM.LST'
      call system(cmm)
      cmm='del RUN.INP'
      call system(cmm)
      stop
      end     


      subroutine getarg(numargs,arg,comline)
      implicit real*8(a-h,o-z)
      character  arg(*)*20,comline*(*)

      n=nblank(comline)
      n=n+1
      j1=0
      j2=0
      numargs=0

 560  ibegin=j2+1
      do i=ibegin, n
         if (comline(i:i).ne.' ') then
            j1=i
            do k=i,n
               if (comline(k:k).eq.' ') then
                  j2=k-1
                  numargs=numargs+1
                  arg(numargs)=comline(j1:j2)
                  go to 560
               endif
            end do
         endif
      end do
      return
      end
      
