c **********************************************************************
c * NTPU52-15.for                                                      *
c *                                                                    *
c *      This program is used to pre-process some data for BPE         *
c *      of Bernese 5.2 and output *.sum files for TurboNet.           *
c *                                                                    *
c *                 created for 5.2 version by Kwo-Hwa Chen 2017.04.09 *
c **********************************************************************
      program ntpu5215

      implicit none
      character buffer*20,cl*80,campname*7,snxname*12
      character basename*4,tmp*200,cmm*200,inf*50
      character doy*3,ss*2,yr*2,fixsta*4
CC      character outf1*12,outf2*12,tmp1*200,site1*4,site2*4,
      integer*4 nargs

      call get_command_argument(1, cl)
      call getarg_local(nargs,buffer,cl)

      inf=buffer

      open(5,file=inf)

 100  read(5,*,end=600) campname,fixsta
CC      runf='RUN.INP'

      if(campname(1:1).eq.'*') go to 100

      open(15,file='RUN.INP')

      write(15,*) '2'
      write(15,'(a7,1x,a8,1x,a8,a7,a1)') campname(1:7),'D:\GPSR\','D:\GPSD\',campname,'\'

      close(15)

CC      fixsta=buffer(2)

CC      cmm='copy C:\BERN52\AUTO\RUN.INP C:\BERN52\'
CC      call system(cmm)

CC      cmm='cd C:\GPSD\'//campname//'\BASE'
CC      call system(cmm)

      cmm='dir/b/o D:\GPSD\'//campname//'\*.??O > BASE.LST'
      call system(cmm)

CC      cmm='copy C:\BERN52\AUTO\GETBASE.EXE'
CC      call system(cmm)

CC      cmm='copy C:\BERN52\AUTO\SNGDIF.DAT'
CC      call system(cmm)

CC      cmm='copy C:\BERN52\AUTO\GPSEST.DAT'
CC      call system(cmm)

      open(10,file='BASE.LST')

CC      num=0

      open(20,file='GPSEST.RES')
      open(30,file='GPSEST.FIN')
      open(40,file='ADDNEQ2.DAT')

      read(10,'(a4,a3,2a2)',end=550) basename,doy,ss,yr
      tmp=basename

CC      cmm='copy/y C:\GPSD\'//campname//'\BASE\'//basename//doy//ss
CC     +//yr//'O C:\GPSD\'//campname
CC      call system(cmm)

CC      if(num.ne.0) then
CC         cmm='del/q C:\GPSR\'//campname(1:6)//'P\OBS\*.*'
CC         call system(cmm)
CC      end if

CC      if(num.eq.0) then
         cmm='md D:\GPSR\'//campname(1:7)//'\SUM'
         call system(cmm)
CC         cmm='md C:\GPSR\'//campname(1:6)//'P\RAW\BASE'
CC         call system(cmm)
CC      end if

CC      num=num+1

CC      outf1(1:4)='SNG_'
CC      outf1(5:8)=basename(1:4)
CC      outf1(9:12)='.INP'

CC      outf2(1:4)='GPS_'
CC      outf2(5:8)=basename(1:4)
CC      outf2(9:12)='.INP'

CC      outf1='GPSEST.RE1'
CC      open(50,file=outf1)

      open(50,file='GPSEST.RE1')
      open(60,file='GPSEST.FI1')
      open(70,file='ADDNEQ2.INP')

 520  read(20,'(a200)',end=530) tmp
      if(tmp(1:10).eq.'STATION4 1') then
         write(50,'(a13,a4,a1)') 'STATION4 1  "',fixsta,'"'
      else
         write(50,'(a200)') tmp
      end if
      go to 520

 530  read(30,'(a200)',end=540) tmp
      if(tmp(1:10).eq.'STATION4 1') then
         write(60,'(a13,a4,a1)') 'STATION4 1  "',fixsta,'"'
      else
         write(60,'(a200)') tmp
      end if
      go to 530

 540  read(40,'(a200)',end=550) tmp
      if(tmp(1:10).eq.'STATION4 1') then
         write(70,'(a13,a4,a1)') 'STATION4 1  "',fixsta,'"'
      else
         write(70,'(a200)') tmp
      end if
      go to 540

 550  close(20)
      close(30)
      close(40)
      close(50)
      close(60)
      close(70)

CC      cmm='del/q C:\GPSUSER52\OPT\5230_GEN\SNGDIF.bak'
CC      call system(cmm)
CC      cmm='ren C:\GPSUSER52\OPT\5230_GEN\SNGDIF.INP SNGDIF.bak'
CC      call system(cmm)
CC      cmm='copy/y SNG_'//basename//'.INP '//'C:\GPSUSER52\OPT\5230_GEN'
CC     +//'\SNGDIF.INP'
CC      call system(cmm)
CC      cmm='del SNG_'//basename//'.INP'
CC      call system(cmm)

      cmm='del/q C:\GPSUSER52\OPT\TP15_RES\GPSEST.bak'
      call system(cmm)
      cmm='ren C:\GPSUSER52\OPT\TP15_RES\GPSEST.INP GPSEST.bak'
      call system(cmm)
      cmm='copy/y GPSEST.RE1 C:\GPSUSER52\OPT\TP15_RES\GPSEST.INP'
      call system(cmm)
      cmm='del GPSEST.RE1'
      call system(cmm)

      cmm='del/q C:\GPSUSER52\OPT\TP15_FIN\GPSEST.bak'
      call system(cmm)
      cmm='ren C:\GPSUSER52\OPT\TP15_FIN\GPSEST.INP GPSEST.bak'
      call system(cmm)
      cmm='copy/y GPSEST.FI1 C:\GPSUSER52\OPT\TP15_FIN\GPSEST.INP'
      call system(cmm)
      cmm='del GPSEST.FI1'
      call system(cmm)

      cmm='del/q C:\GPSUSER52\OPT\TP15_ADD\ADDNEQ2.bak'
      call system(cmm)
      cmm='ren C:\GPSUSER52\OPT\TP15_ADD\ADDNEQ2.INP ADDNEQ2.bak'
      call system(cmm)
      cmm='copy/y ADDNEQ2.INP C:\GPSUSER52\OPT\TP15_ADD\ADDNEQ2.INP'
      call system(cmm)
      cmm='del ADDNEQ2.INP'
      call system(cmm)

      cmm='BERN52-OBS15 RUN.INP'
      call system(cmm)

      cmm='copy D:\GPSR\'//campname(1:7)//'\SOL\*.SNX C:\BERN52\AUTO\'
CC     +//campname(1:6)//'P\SUM\'
      call system(cmm)

CC      cmm='del C:\GPSD\'//campname//'\'//basename//doy//ss
CC     +//yr//'O'
CC      call system(cmm)

CC      cmm='move C:\GPSR\'//campname(1:6)//'P\RAW\'//basename//doy//ss
CC     +//yr//'O C:\GPSR\'//campname(1:6)//'P\RAW\BASE\'
CC      call system(cmm)

CC      go to 500

      close(10)

      cmm='dir/b/o C:\BERN52\AUTO\*.SNX > TR2SUM.LST'
      call system(cmm)

      open(80,file='TR2SUM.LST')
CC      open(80,file='TR2SUM.BAT')

      read(80,'(a12)',end=620) snxname

      cmm='copy '//snxname//' 20'//yr//doy//ss(1:1)//'.SNX'
      call system(cmm)

      cmm='RDSINEX50 20'//yr//doy//ss(1:1)//'.SNX'
      call system(cmm)

CC      cmm='copy C:\GPSR\'//campname(1:6)//'P\STA\ABBREV.ABB C:\BERN52\'
CC     +//'AUTO\'

CC      call system(cmm)

CC      open(90,file='ABBREV.ABB')

CC      do i = 1,5
CC         read(90,'(a200)') tmp1
CC      end do

CC 700  read(90,'(a200)',end=710) tmp1
CC      if(snxname(1:2).eq.tmp1(35:36)) site1(1:4)=tmp1(26:29)
CC      if(snxname(3:4).eq.tmp1(35:36)) site2(1:4)=tmp1(26:29)
CC      go to 700

CC 710  close(90)

CC      write(80,'(a17,1x,a12)') 'CALL RDSINEX52 20',yr,doy,ss,'.SUM'

CC      cmm='ren 20'//yr//doy//ss(1:1)//'.SUM '//campname(1:6)//'_'//
CC     +site1(1:4)//'_'//site2(1:4)//'.sum'
CC      call system(cmm)

CC      go to 610

 620  close(80)
CC      close(80)

CC      cmm='CALL C:\BERN52\AUTO\TR2SUM.BAT'
CC      call system(cmm)

      cmm='move C:\BERN52\AUTO\*.SUM D:\GPSR\'//campname(1:7)//'\SUM\'
      call system(cmm)
      cmm='rename D:\GPSR\'//campname(1:7)//' '//campname(1:7)//'P'
      call system(cmm)

      cmm='del/q C:\BERN52\AUTO\*.SNX'
      call system(cmm)

CC      cmm='del/q C:\BERN52\AUTO\ABBREV.ABB'
CC      call system(cmm)

      go to 100

 600  continue
      cmm='del BASE.LST'
      call system(cmm)
      cmm='del TR2SUM.LST'
      call system(cmm)
      cmm='del RUN.INP'
      call system(cmm)

      close(10)
      close(20)
      close(30)
      close(40)
      close(50)
      close(60)
      close(70)
      close(80)

      stop
      end


      subroutine getarg_local(numargs,arg,comline)
      implicit real*8(a-h,o-z)
      character  arg(*)*20,comline*(*)

      n=len_trim(comline)
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

